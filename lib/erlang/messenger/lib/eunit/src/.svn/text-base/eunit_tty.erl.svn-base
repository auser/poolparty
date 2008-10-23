%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id:$ 
%%
%% @author Richard Carlsson <richardc@it.uu.se>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc Text-based frontend for EUnit

-module(eunit_tty).

-include("eunit.hrl").
-include("eunit_internal.hrl").

-export([start/1, start/2]).


-record(state, {verbose = false,
		succeed = 0,
		fail = 0,
		abort = 0,
		skip = 0,
		indent = 0}).

start(List) ->
    start(List, []).

start(List, Options) ->
    St = #state{verbose = proplists:get_bool(verbose, Options)},
    Id = [],
    spawn(fun () -> init(Id, List, St) end).

init(Id, List, St0) ->
    receive
	{start, Reference} ->
	    if St0#state.verbose -> print_header();
	       true -> ok
	    end,
	    St = group_begin(Id, "", List, St0),
	    receive
		{stop, Reference, ReplyTo} ->
		    Result = if St#state.fail == 0, St#state.abort == 0,
				St#state.skip == 0 ->
				     ok;
				true ->
				     error
			     end,
		    report(Result, St),
		    ReplyTo ! {result, Reference, Result},
		    ok
	    end
    end.

report(ok, St) ->
    if St#state.succeed == 0 ->
	    io:fwrite("  There were no tests to run.\n");
       true ->
	    if St#state.verbose -> print_bar();
	       true -> ok
	    end,
	    if St#state.succeed == 1 ->
		    io:fwrite("  Test successful.\n");
	       true ->
		    io:fwrite("  All ~w tests successful.\n",
			      [St#state.succeed])
	    end
    end;
report(error, St) ->
    print_bar(),
    io:fwrite("  Failed: ~w.  Aborted: ~w."
	      "  Skipped: ~w.  Succeeded: ~w.\n",
	      [St#state.fail, St#state.abort,
	       St#state.skip, St#state.succeed]).

print_header() ->
    io:fwrite("======================== EUnit "
	      "========================\n").

print_bar() ->
    io:fwrite("============================"
	      "===========================\n").    

wait(Id, St) ->
    receive
	{status, Id, Data} -> {Data, St}
    end.

entry({item, Id, Desc, Test}, St) ->
    test_begin(Id, Desc, Test, St);
entry({group, Id, Desc, Es}, St) ->
    group_begin(Id, Desc, Es, St).

tests([E | Es], St) ->
    tests(Es, entry(E, St));
tests([], St) ->
    St.

test_begin(Id, Desc, {Module, Name}, St) ->
    test_begin(Id, Desc, {Module, Name, 0}, St);
test_begin(Id, Desc, {Module, Name, Line}, St) ->
    Text = format_test_begin(Module, Name, Line, Desc),
    if St#state.verbose -> print_test_begin(St#state.indent, Text);
       true -> ok
    end,
    case wait(Id, St) of
	{{progress, 'begin', test}, St1} ->
	    test_end(Id, Text, St1);
	{{cancel, Reason}, St1} ->
	    if St#state.verbose -> print_test_cancel(Reason);
	       Reason /= undefined ->
		    print_test_begin(St#state.indent, Text),
		    print_test_cancel(Reason);
	       true -> ok
	    end,
	    St1#state{skip = St1#state.skip + 1}
    end.

test_end(Id, Text, St) ->
    case wait(Id, St) of
	{{progress, 'end', {Result, Time, _Output}}, St1} ->
	    if Result == ok ->
		    if St#state.verbose -> print_test_end(Time);
		       true -> ok
		    end,
		    St1#state{succeed = St1#state.succeed + 1};
	       true ->
		    if St#state.verbose -> ok;
		       true -> print_test_begin(St#state.indent, Text)
		    end,
		    print_test_error(Result),
		    St1#state{fail = St1#state.fail + 1}
	    end;
	{{cancel, Reason}, St1} ->
	    if St#state.verbose -> ok;
	       true -> print_test_begin(St#state.indent, Text)
	    end,
	    print_test_cancel(Reason),
	    St1#state{abort = St1#state.abort + 1}
    end.

group_begin(Id, Desc, Es, St0) ->
    I = St0#state.indent,
    St = if Desc /= "", St0#state.verbose ->
		 print_group_start(I, Desc),
		 St0#state{indent = I + 1};
	    true ->
		 St0
	 end,
    case wait(Id, St) of
	{{progress, 'begin', group}, St1} ->
	    group_end(Id, I, Desc, tests(Es, St1));
	{{cancel, Reason}, St1} ->
	    if Desc /= "", St1#state.verbose ->
		    print_group_cancel(I, Reason);
	       Desc /= "" ->
		    print_group_start(I, Desc),
		    print_group_cancel(I, Reason);
	       true ->
		    ok
	    end,
	    %% TODO: eliminate this size calculation if possible
	    Size = eunit_data:list_size(Es),
	    St1#state{indent = I, skip = St1#state.skip + Size}
    end.

group_end(Id, I, Desc, St) ->
    (case wait(Id, St) of
	 {{progress, 'end', {_Count, Time, _Output}}, St1} ->
	     if Desc /= "", St#state.verbose ->
		     print_group_end(St1#state.indent, Time);
		true ->
		     ok
	     end,
	     St1;
	 {{cancel, undefined}, St1} ->
	     St1;  %% "skipped" message is not interesting here
	 {{cancel, Reason}, St1} ->
	     if Desc /= "", St1#state.verbose ->
		     print_group_cancel(I, Reason);
		true ->
		     print_group_start(I, Desc),
		     print_group_cancel(I, Reason)
	     end,
	     St1
     end)#state{indent = I}.

indent(N) when is_integer(N), N >= 1 ->
    io:put_chars(lists:duplicate(N * 2, $\s));
indent(_) ->
    ok.

print_group_start(I, Desc) ->
    indent(I),
    io:fwrite("~s\n", [Desc]).

print_group_end(I, Time) ->
    if Time > 0 ->
	    indent(I),
	    io:fwrite("[done in ~.3f s]\n", [Time/1000]);
       true ->
	    ok
    end.

format_test_begin(Module, Name, Line, Desc) ->
    L = if Line == 0 -> "";
	   true -> io_lib:fwrite("~w:", [Line])
	end,
    D = if Desc == "" -> "";
	   true -> io_lib:fwrite(" (~s)", [Desc])
	end,
    io_lib:fwrite("~s:~s~s~s...", [Module, L, Name, D]).

print_test_begin(I, Text) ->
    indent(I),
    io:put_chars(Text).

print_test_end(Time) ->
    T = if Time > 0 -> io_lib:fwrite("[~.3f s] ", [Time/1000]);
	   true -> ""
	end,
    io:fwrite("~sok\n", [T]).

print_test_error({error, Exception}) ->
    io:fwrite("*failed*\n::~s\n\n",
	      [eunit_lib:format_exception(Exception)]);
print_test_error({skipped, Reason}) ->
    io:fwrite("*did not run*\n::~s\n\n",
	      [format_skipped(Reason)]).

format_skipped({module_not_found, M}) ->
    io_lib:format("missing module: ~w", [M]);
format_skipped({no_such_function, {M,F,A}}) ->
    io_lib:format("no such function: ~w:~w/~w", [M,F,A]).    

print_test_cancel(Reason) ->
    io:fwrite(format_cancel(Reason)).

print_group_cancel(_I, {blame, _}) ->
    ok;
print_group_cancel(I, Reason) ->
    indent(I),
    io:fwrite(format_cancel(Reason)).

format_cancel(undefined) ->
    "*skipped*\n";
format_cancel(timeout) ->
    "*timed out*\n";
format_cancel({startup, Reason}) ->
    io_lib:fwrite("*could not start test process*\n::~P\n\n",
		  [Reason, 15]);
format_cancel({blame, _SubId}) ->
    "*cancelled because of subtask*\n";
format_cancel({exit, Reason}) ->
    io_lib:fwrite("*unexpected termination of test process*\n::~P\n\n",
		  [Reason, 15]);
format_cancel({abort, Reason}) ->
    eunit_lib:format_error(Reason).

