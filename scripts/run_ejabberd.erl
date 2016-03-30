#!/usr/bin/env escript

%%! -smp enable -pa ../ebin

main([]) ->
    case run_option:get_options("../options/run.option") of
		[] ->
			io:format("run_ejabberd escript start find run.option is empty~n");
		Options ->
            BeamDir = run_option:get_beam_dir(Options),
             file:set_cwd(BeamDir),
	        run_ejabberd:start()
    end;
	
main(Args) ->
	io:format("run ejabberd args error : ~p~n", [Args]).
