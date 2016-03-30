# This module is the one responsible for converging
# dependencies in a recursive fashion. This
# module and its functions are private to Mix.
defmodule Mix.Dep.Converger do
  @moduledoc false

  @doc """
  Topsorts the given dependencies.
  """
  def topsort(deps) do
    graph = :digraph.new

    try do
      Enum.each(deps, fn %Mix.Dep{app: app} ->
        :digraph.add_vertex(graph, app)
      end)

      Enum.each(deps, fn %Mix.Dep{app: app, deps: other_deps} ->
        Enum.each(other_deps, fn %Mix.Dep{app: other_app} ->
          :digraph.add_edge(graph, other_app, app)
        end)
      end)

      if apps = :digraph_utils.topsort(graph) do
        Enum.map(apps, fn(app) ->
          Enum.find(deps, fn(%Mix.Dep{app: other_app}) -> app == other_app end)
        end)
      else
        Mix.raise "Could not sort dependencies. There are cycles in the dependency graph"
      end
    after
      :digraph.delete(graph)
    end
  end

  @doc """
  Converges all dependencies from the current project,
  including nested dependencies.

  There is a callback that is invoked for each dependency and
  must return an updated dependency in case some processing
  is done.

  See `Mix.Dep.Loader.children/1` for options.
  """
  def converge(acc, lock, opts, callback) do
    {deps, acc, lock} = all(acc, lock, opts, callback)
    {topsort(deps), acc, lock}
  end

  defp all(acc, lock, opts, callback) do
    main = Mix.Dep.Loader.children()
    main = Enum.map(main, &(%{&1 | top_level: true}))
    apps = Enum.map(main, &(&1.app))

    lock_given? = !!lock
    env = opts[:env]

    # If no lock was given, let's read one to fill in the deps
    lock = lock || Mix.Dep.Lock.read

    # Run converger for all dependencies, except remote
    # dependencies. Since the remote converger may be
    # lazily loaded, we need to check for it on every
    # iteration.
    {deps, rest, lock} =
      all(main, apps, callback, acc, lock, env, fn dep ->
        if (remote = Mix.RemoteConverger.get) && remote.remote?(dep) do
          {:loaded, dep}
        else
          {:unloaded, dep, nil}
        end
      end)

    diverged? = Enum.any?(deps, &Mix.Dep.diverged?/1)

    # Run remote converger if one is available and rerun Mix's
    # converger with the new information
    remote = Mix.RemoteConverger.get

    if not diverged? && remote do
      # If there is a lock, it means we are doing a get/update
      # and we need to hit the remote converger which do external
      # requests and what not. In case of deps.check, deps and so
      # on, there is no lock, so we won't hit this branch.
      lock = if lock_given?, do: remote.converge(deps, lock), else: lock

      deps = deps
             |> Enum.reject(&remote.remote?(&1))
             |> Enum.into(%{}, &{&1.app, &1})

      # In case no lock was given, we will use the local lock
      # which is potentially stale. So remote.deps/2 needs to always
      # check if the data it finds in the lock is actually valid.
      {deps, rest, lock} =
        all(main, apps, callback, rest, lock, env, fn dep ->
          if cached = deps[dep.app] do
            {:loaded, cached}
          else
            {:unloaded, dep, remote.deps(dep, lock)}
          end
        end)

      {reject_non_fullfilled_optional(deps), rest, lock}
    else
      {reject_non_fullfilled_optional(deps), rest, lock}
    end
  end

  defp all(main, apps, callback, rest, lock, env, cache) do
    {deps, rest, lock} = all(main, [], [], apps, callback, rest, lock, env, cache)
    # When traversing dependencies, we keep skipped ones to
    # find conflicts. We remove them now after traversal.
    {deps, _} = Mix.Dep.Loader.partition_by_env(deps, env)
    {deps, rest, lock}
  end

  # We traverse the tree of dependencies in a breadth-first
  # fashion. The reason for this is that we converge
  # dependencies, but allow the parent to override any
  # dependency in the child. Consider this tree with
  # dependencies "a", "b", etc and the order they are
  # converged:
  #
  #     * project
  #       1) a
  #       2) b
  #         4) d
  #       3) c
  #         5) e
  #         6) f
  #           7) d
  #
  # Notice that the "d" dependency exists as a child of "b"
  # and child of "f". In case the dependency is the same,
  # we proceed. However, if there is a conflict, for instance
  # different Git repositories are used as source in each, we
  # raise an exception.
  #
  # In order to solve such dependencies, we allow the project
  # to specify the same dependency, but it will be considered
  # to have higher priority:
  #
  #     * project
  #       1) a
  #       2) b
  #         5) d
  #       3) c
  #         6) e
  #         7) f
  #           8) d
  #       4) d
  #
  # Now, since "d" was specified in a parent project, no
  # exception is going to be raised since d is considered
  # to be the authoritative source.
  defp all([dep|t], acc, upper_breadths, current_breadths, callback, rest, lock, env, cache) do
    cond do
      new_acc = diverged_deps(acc, upper_breadths, dep) ->
        all(t, new_acc, upper_breadths, current_breadths, callback, rest, lock, env, cache)
      Mix.Dep.Loader.skip?(dep, env) ->
        # We still keep skipped dependencies around to detect conflicts.
        # They must be rejected after every all iteration.
        all(t, [dep|acc], upper_breadths, current_breadths, callback, rest, lock, env, cache)
      true ->
        {dep, rest, lock} =
          case cache.(dep) do
            {:loaded, cached_dep} ->
              {cached_dep, rest, lock}
            {:unloaded, dep, children} ->
              {dep, rest, lock} = callback.(put_lock(dep, lock), rest, lock)

              # After we invoke the callback (which may actually check out the
              # dependency), we load the dependency including its latest info
              # and children information.
              {Mix.Dep.Loader.load(dep, children), rest, lock}
          end

        {acc, rest, lock} =
          all(t, [dep|acc], upper_breadths, current_breadths, callback, rest, lock, env, cache)

        deps = reject_non_fullfilled_optional(dep.deps, current_breadths)
        new_breadths = Enum.map(deps, &(&1.app)) ++ current_breadths
        all(deps, acc, current_breadths, new_breadths, callback, rest, lock, env, cache)
    end
  end

  defp all([], acc, _upper, _current, _callback, rest, lock, _env, _cache) do
    {Enum.reverse(acc), rest, lock}
  end

  defp put_lock(%Mix.Dep{app: app} = dep, lock) do
    put_in dep.opts[:lock], lock[app]
  end

  # Look for divergence in dependencies.
  #
  # If the same dependency is specified more than once,
  # we need to guarantee they converge. If they don't
  # converge, we mark them as diverged.
  #
  # The only exception is when the dependency that
  # diverges is in the upper breadth, in those cases we
  # also check for the override option and mark the dependency
  # as overridden instead of diverged.
  defp diverged_deps(list, upper_breadths, %Mix.Dep{app: app} = dep) do
    in_upper? = app in upper_breadths

    {acc, match} =
      Enum.map_reduce list, false, fn(other, match) ->
        %Mix.Dep{app: other_app, opts: other_opts} = other

        cond do
          app != other_app ->
            {other, match}
          in_upper? && other_opts[:override] ->
            {other |> with_matching_only(dep, in_upper?), true}
          converge?(other, dep) ->
            {other |> with_matching_only(dep, in_upper?)
                   |> with_matching_req(dep) |> merge_manager(dep, in_upper?), true}
          true ->
            tag = if in_upper?, do: :overridden, else: :diverged
            {%{other | status: {tag, dep}}, true}
        end
      end

    if match, do: acc
  end

  defp with_matching_only(%{opts: other_opts} = other, %{opts: opts} = dep, in_upper?) do
    if opts[:optional] do
      other
    else
      with_matching_only(other, other_opts, dep, opts, in_upper?)
    end
  end

  # When in_upper is true
  #
  # When a parent dependency specifies :only that is a subset
  # of a child dependency, we are going to abort as the parent
  # dependency must explicitly outline a superset of child
  # dependencies.
  #
  # We could resolve such conflicts automatically but, since
  # the user has likely written only: :env in their mix.exs
  # file, we decided to go with a more explicit approach of
  # asking them to change it to avoid later surprises and
  # headaches.
  defp with_matching_only(other, other_opts, dep, opts, true) do
    case Keyword.fetch(other_opts, :only) do
      {:ok, other_only} ->
        case Keyword.fetch(opts, :only) do
          {:ok, only} ->
            case List.wrap(only) -- List.wrap(other_only) do
              [] -> other
              _  -> %{other | status: {:divergedonly, dep}}
            end
          :error ->
            %{other | status: {:divergedonly, dep}}
        end
      :error ->
        other
    end
  end

  # When in_upper is false
  #
  # In this case, the two dependencies do not have a common path and
  # only solution is to merge the environments. We have decided to
  # perform it explicitly as, opposite to in_upper above, the
  # dependencies are never really laid out in the parent tree.
  defp with_matching_only(other, other_opts, _dep, opts, false) do
    other_only = Keyword.get(other_opts, :only)
    only = Keyword.get(opts, :only)
    if other_only && only do
      put_in other.opts[:only], Enum.uniq(List.wrap(other_only) ++ List.wrap(only))
    else
      %{other | opts: Keyword.delete(other_opts, :only)}
    end
  end

  defp converge?(%Mix.Dep{scm: scm1, opts: opts1}, %Mix.Dep{scm: scm2, opts: opts2}) do
    scm1 == scm2 and opts_equal?(opts1, opts2) and scm1.equal?(opts1, opts2)
  end

  defp opts_equal?(opts1, opts2) do
    keys = ~w(app env compile)a
    Enum.all?(keys, &(Keyword.fetch(opts1, &1) == Keyword.fetch(opts2, &1)))
  end

  defp reject_non_fullfilled_optional(deps) do
    apps = Enum.map(deps, & &1.app)
    for dep <- deps do
      update_in dep.deps, &reject_non_fullfilled_optional(&1, apps)
    end
  end

  defp reject_non_fullfilled_optional(children, upper_breadths) do
    Enum.reject children, fn %Mix.Dep{app: app, opts: opts} ->
      opts[:optional] && not(app in upper_breadths)
    end
  end

  defp merge_manager(%{manager: other_manager} = other, %{manager: manager}, in_upper?) do
    %{other | manager: sort_manager(other_manager, manager, in_upper?)}
  end

  @managers [:mix, :rebar3, :rebar, :make]

  defp sort_manager(other_manager, manager, true) do
    other_manager || manager
  end
  defp sort_manager(other_manager, manager, false) do
    priority = @managers -- (@managers -- (List.wrap(other_manager) ++ List.wrap(manager)))
    List.first(priority) || other_manager || manager
  end

  defp with_matching_req(%Mix.Dep{} = other, %Mix.Dep{} = dep) do
    case other.status do
      {:ok, vsn} when not is_nil(vsn) ->
        case Mix.Dep.Loader.vsn_match(dep.requirement, vsn, dep.app) do
          {:ok, true} -> other
          _ -> %{other | status: {:divergedreq, dep}}
        end
      _ ->
        other
    end
  end
end