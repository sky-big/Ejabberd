#!/usr/bin/env escript
%%! -smp enable -pa ebin -sname testsqlite3

-record(user, {name, age, wage}).

test() ->
    file:delete("ct.db"),
    sqlite3:open(ct),
    sqlite3:create_table(ct, user, [{id, integer, [primary_key]}, {name, text}, {age, integer}, {wage, integer}]),
    [user] = sqlite3:list_tables(ct),
    0 = sqlite3:changes(ct),
    [{id, integer, [primary_key]}, {name, text}, {age, integer}, {wage, integer}] = sqlite3:table_info(ct, user),
    {rowid, Id1} = sqlite3:write(ct, user, [{name, "abby"}, {age, 20}, {wage, 2000}]),
    Id1 = 1,
    1 = sqlite3:changes(ct),
    {rowid, Id2} = sqlite3:write(ct, user, [{name, "marge"}, {age, 30}, {wage, 2000}]),
    Id2 = 2,
    [{columns, Columns}, {rows, Rows1}] = sqlite3:sql_exec(ct, "select * from user;"),
    Columns = ["id", "name", "age", "wage"],
    Rows1 = [{1, <<"abby">>, 20, 2000}, {2, <<"marge">>, 30, 2000}],
    [{columns, Columns}, {rows, Rows2}] = sqlite3:read(ct, user, {name, "abby"}),
    Rows2 = [{1, <<"abby">>, 20, 2000}],
    [{columns, Columns}, {rows, Rows1}] = sqlite3:read(ct, user, {wage, 2000}),
    sqlite3:delete(ct, user, {name, "marge"}),
    [{columns, Columns}, {rows, Rows2}] = sqlite3:sql_exec(ct, "select * from user;"),
    sqlite3:drop_table(ct, user),
    sqlite3:close(ct),
    io:format("Tests passed~n").

main(_) ->
  try test() of
    _ -> ok
  catch
    Class:Error ->
      io:format("~p:~p:~p~n", [Class, Error, erlang:get_stacktrace()])
  end.
