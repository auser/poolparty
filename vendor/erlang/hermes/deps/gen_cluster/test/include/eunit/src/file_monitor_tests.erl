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
%% $Id: file_monitor.erl 283 2008-12-06 12:20:44Z rcarlsson $ 
%%
%% @private (for now)
%% @author Richard Carlsson <richardc@it.uu.se>
%% @copyright 2006 Richard Carlsson
%% @doc Unit tests for the Erlang file monitoring service

-module(file_monitor_tests).

-include_lib("kernel/include/file.hrl").
%%-include_lib("eunit/include/eunit.hrl").
-include("../include/eunit.hrl").

-define(SERVER, file_monitor).
-define(MSGTAG, ?SERVER).

%% Basic tests: these start and stop the server for each test
basic_test_() ->
    case os:type() of
	{unix,_} ->
	    {foreach,
	     fun new_test_server/0,
	     fun stop_test_server/1,
	     [{with, [fun return_value_test/1]},
	      {with, [fun flatten_path_test/1]},
	      {with, [fun no_file_test/1]},
	      {with, [fun no_dir_test/1]},
	      {with, [fun existing_dir_test/1]},
	      {with, [fun existing_file_test/1]},
	      {with, [fun notdir_test/1]},
	      {with, [fun dir_as_file_test/1]}
	     ]
	    };
	_ ->
	    []
    end.

%% All the below tests run only on unix-like platforms

return_value_test(Server) ->
    Path = <<"/tmp/nonexisting">>,  % single binary path
    MonitorResult = ?SERVER:monitor_file(Server, Path, []),
    ?assertMatch({ok, Ref, Path} when is_reference(Ref), MonitorResult),
    {ok, MonitorRef, _} = MonitorResult,
    ?assertMatch(ok, ?SERVER:demonitor(Server, MonitorRef)),
    ?assertMatch({ok, Ref, Path} when is_reference(Ref),
		 ?SERVER:monitor_dir(Server, Path, [])).

flatten_path_test(Server) ->
    Path = ["/","tmp","/","foo"],
    ?assertMatch({ok, _, <<"/tmp/foo">>},
		 ?SERVER:monitor_file(Server, Path, [])),
    ?assertMatch({ok, _, <<"/tmp/foo">>},
		 ?SERVER:monitor_dir(Server, Path, [])).

no_file_test(Server) ->
    monitor_no_file(Server, <<"/tmp/nonexisting">>).
    
monitor_no_file(Server, Path) ->
    {ok, Ref, Path} = ?SERVER:monitor_file(Server, Path, []),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref, {error, Path, file, enoent}},
			 Msg)
    end,
    Ref.

no_dir_test(Server) ->
    monitor_no_dir(Server, <<"/tmp/nonexisting">>).

monitor_no_dir(Server, Path) ->
    {ok, Ref, Path} = ?SERVER:monitor_dir(Server, Path, []),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref, {error, Path, directory, enoent}},
			 Msg)
    end,
    Ref.

existing_dir_test(Server) ->
    Path = <<"/etc">>,
    {ok, Ref, Path} = ?SERVER:monitor_dir(Server, Path, []),
    receive
	Msg ->
	    %% we should get a nonempty list of directory entries
	    ?assertMatch({?MSGTAG, Ref,
			  {found, Path, directory, #file_info{}, Es}}
			 when (is_list(Es) and (Es =/= [])), Msg)
    end.

existing_file_test(Server) ->
    Path = <<"/etc/passwd">>,
    {ok, Ref, Path} = ?SERVER:monitor_file(Server, Path, []),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref,
			  {found, Path, file, #file_info{}, []}}, Msg)
    end.

notdir_test(Server) ->
    Path = <<"/etc/passwd">>,
    {ok, Ref, Path} = ?SERVER:monitor_dir(Server, Path, []),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref,
			  {error, Path, directory, enotdir}}, Msg)
    end.

dir_as_file_test(Server) ->
    Path = <<"/etc">>,
    {ok, Ref, Path} = ?SERVER:monitor_file(Server, Path, []),
    receive
	Msg ->
	    %% we should get an empty list of directory entries,
	    %% since we are just monitoring it as a file
	    ?assertMatch({?MSGTAG, Ref,
			  {found, Path, file, #file_info{}, []}}, Msg)
    end.

%% File event tests: this runs the server over a group of tests
file_event_test_() ->
    case os:type() of
	{unix,_} ->
	    {setup,
	     fun new_test_server/0,
	     fun stop_test_server/1,
	     fun (Server) ->
		     {setup, local,
		      fun () ->
			      Path = <<"/tmp/filemonitortestfile">>,
			      remove_file(Path),
			      Ref = monitor_no_file(Server, Path),
			      {Server, Path, Ref}
		      end,
		      fun ({_, Path, _}) -> remove_file(Path) end,
		      fun ({Server, Path0, Ref}=X) ->
			      [{with, X,
				[fun create_file_subtest/1,
				 fun delete_file_subtest/1,
				 fun create_file_subtest/1,
				 fun delete_file_subtest/1,
				 fun create_file_subtest/1,
				 fun touch_file_subtest/1,
				 fun touch_file_subtest/1,
				 fun touch_file_subtest/1,
				 fun change_file_type_subtest/1]
			       },
			       {setup, local,
				fun () ->
					{file_monitor:normalize_path([Path0, $2]),
					 file_monitor:normalize_path([Path0, $3])}
				end,
				fun ({Path2, Path3}) ->
				        catch remove_file(Path2),
					catch remove_file(Path3)
				end,
				fun ({Path2, Path3}) ->
					{with, {Server, Ref, Path2, Path3},
					 [fun add_monitor2_subtest/1,
					  fun add_monitor3_subtest/1,
					  fun create_file2_subtest/1,
					  fun create_file3_subtest/1,
					  fun touch_file2_subtest/1,
					  fun touch_file3_subtest/1,
					  fun delete_file2_subtest/1,
					  fun delete_file3_subtest/1,
					  fun remove_monitor2_subtest/1,
					  fun remove_monitor3_subtest/1]}
				end},
			       {with, X,
				[fun touch_file_subtest/1,
				 fun delete_file_subtest/1]
			       }]
		      end
		     }
	     end
	    };
	_ ->
	    []
    end.

create_file_subtest({_, Path, Ref}) ->
    create_file_subtest({Path, Ref});
create_file_subtest({Path, Ref}) ->
    assert_empty_mailbox(),
    write_file(Path),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref,
			  {changed, Path, file,
			   #file_info{type=regular}, []}}, Msg)
    end.

delete_file_subtest({_, Path, Ref}) ->
    delete_file_subtest({Path, Ref});
delete_file_subtest({Path, Ref}) ->
    assert_empty_mailbox(),
    remove_file(Path),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref,
			  {error, Path, file, enoent}}, Msg)
    end.

touch_file_subtest({_, Path, Ref}) ->
    touch_file_subtest({Path, Ref});
touch_file_subtest({Path, Ref}) ->
    assert_empty_mailbox(),
    touch_file(Path),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref,
 			  {changed, Path, file,
			   #file_info{type=regular}, []}}, Msg)
    end.

change_file_type_subtest({_, Path, Ref}) ->
    assert_empty_mailbox(),
    remove_file(Path),
    make_dir(Path),
    receive
	Msg1 ->
	    ?assertMatch({?MSGTAG, Ref,
			  {error, Path, file, enoent}}, Msg1)
    end,
    receive
	Msg2 ->
	    ?assertMatch({?MSGTAG, Ref,
			  {changed, Path, file,
			   #file_info{type=directory}, []}}, Msg2)
    end,
    remove_dir(Path),
    write_file(Path),
    receive
	Msg3 ->
	    ?assertMatch({?MSGTAG, Ref,
			  {error, Path, file, enoent}}, Msg3)
    end,
    receive
	Msg4 ->
	    ?assertMatch({?MSGTAG, Ref,
			  {changed, Path, file,
			   #file_info{type=regular}, []}}, Msg4)
    end.

add_monitor2_subtest({Server, Ref, Path2, _}) ->
    add_monitor_subtest_1(Server, Ref, Path2).

add_monitor3_subtest({Server, Ref, _, Path3}) ->
    add_monitor_subtest_1(Server, Ref, Path3).

add_monitor_subtest_1(Server, Ref, Path) ->
    assert_empty_mailbox(),
    {ok, Ref, Path} = ?SERVER:monitor_file(Server, Path,
					   [{monitor, Ref}]),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref, {error, Path, file, enoent}},
			 Msg)
    end.

create_file2_subtest({Server, Ref, Path2, _}) ->
    create_file_subtest({Server, Path2, Ref}).

create_file3_subtest({Server, Ref, _, Path3}) ->
    create_file_subtest({Server, Path3, Ref}).

touch_file2_subtest({Server, Ref, Path2, _}) ->
    touch_file_subtest({Server, Path2, Ref}).

touch_file3_subtest({Server, Ref, _, Path3}) ->
    touch_file_subtest({Server, Path3, Ref}).

delete_file2_subtest({Server, Ref, Path2, _}) ->
    delete_file_subtest({Server, Path2, Ref}).

delete_file3_subtest({Server, Ref, _, Path3}) ->
    delete_file_subtest({Server, Path3, Ref}).

remove_monitor2_subtest({Server, Ref, Path2, _}) ->
    remove_monitor_subtest_1(Server, Ref, Path2).

remove_monitor3_subtest({Server, Ref, _, Path3}) ->
    remove_monitor_subtest_1(Server, Ref, Path3).

remove_monitor_subtest_1(Server, Ref, Path) ->
    ?assertMatch(ok, ?SERVER:demonitor_file(Server, Path, Ref)),
    touch_file(Path),
    wait(500),
    assert_empty_mailbox().

%% Directory event tests: this runs the server over a group of tests
directory_event_test_() ->
    case os:type() of
	{unix,_} ->
	    {setup,
	     fun new_test_server/0,
	     fun stop_test_server/1,
	     fun (Server) ->
		     {setup, local,
		      fun () ->
			      Path = <<"/tmp/filemonitortestdir">>,
			      recursive_remove(Path),
			      Ref = monitor_no_dir(Server, Path),
			      {Path, Ref}
		      end,
		      {with,
		       [fun create_dir_subtest/1,
			fun delete_empty_dir_subtest/1,
 			fun create_dir_subtest/1,
			fun delete_empty_dir_subtest/1,
 			fun create_dir_subtest/1,
			fun change_dir_type_subtest/1,
 			fun create_subdir_subtest/1,
 			fun delete_subdir_subtest/1,
 			fun create_subdir_subtest/1,
 			fun create_subfile_subtest/1,
 			fun delete_subfile_subtest/1,
 			fun create_subfile_subtest/1,
			fun delete_recursive_subtest/1
		       ]}
		     }
	     end
	    };
	_ ->
	    []
    end.

create_dir_subtest({Path, Ref}) ->
    assert_empty_mailbox(),
    make_dir(Path),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref,
			  {changed, Path, directory,
			   #file_info{type=directory}, []}}, Msg)
    end.

delete_empty_dir_subtest({Path, Ref}) ->
    assert_empty_mailbox(),
    remove_dir(Path),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref,
			  {error, Path, directory, enoent}}, Msg)
    end.

create_subdir_subtest({Path, Ref}) ->
    Subdir = <<"subdir">>,
    assert_empty_mailbox(),
    make_dir(join(Path, Subdir)),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref,
			  {changed, Path, directory,
			   #file_info{type=directory},
			   [{added, Subdir}]}}, Msg)
    end.

delete_subdir_subtest({Path, Ref}) ->
    Subdir = <<"subdir">>,
    assert_empty_mailbox(),
    remove_dir(join(Path, Subdir)),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref,
			  {changed, Path, directory,
			   #file_info{type=directory},
			   [{deleted, Subdir}]}}, Msg)
    end.

create_subfile_subtest({Path, Ref}) ->
    File = <<"file">>,
    assert_empty_mailbox(),
    write_file(join(Path, File)),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref,
			  {changed, Path, directory,
			   #file_info{type=directory},
			   [{added, File}]}}, Msg)
    end.

delete_subfile_subtest({Path, Ref}) ->
    File = <<"file">>,
    assert_empty_mailbox(),
    remove_file(join(Path, File)),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref,
			  {changed, Path, directory,
			   #file_info{type=directory},
			   [{deleted, File}]}}, Msg)
    end.

delete_recursive_subtest({Path, Ref}) ->
    assert_empty_mailbox(),
    recursive_remove(Path),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref,
			  {error, Path, directory, enoent}}, Msg)
    end.

change_dir_type_subtest({Path, Ref}) ->
    assert_empty_mailbox(),
    remove_dir(Path),
    write_file(Path),
    receive
	Msg1 ->
	    ?assertMatch({?MSGTAG, Ref,
			  {error, Path, directory, enoent}}, Msg1)
    end,
    receive
	Msg2 ->
	    ?assertMatch({?MSGTAG, Ref,
			  {error, Path, directory, enotdir}}, Msg2)
    end,
    remove_file(Path),
    make_dir(Path),
    receive
	Msg3 ->
	    ?assertMatch({?MSGTAG, Ref,
			  {error, Path, directory, enoent}}, Msg3)
    end,
    receive
	Msg4 ->
	    ?assertMatch({?MSGTAG, Ref,
			  {changed, Path, directory,
			   #file_info{type=directory}, []}}, Msg4)
    end.


%% Automonitoring tests: this runs the server over a group of tests
automonitor_test_() ->
    case os:type() of
	{unix,_} ->
	    {setup,
	     fun new_test_server/0,
	     fun stop_test_server/1,
	     fun (Server) ->
		     {setup, local,
		      fun () ->
			      Path = <<"/tmp/filemonitortestdir">>,
			      recursive_remove(Path),
			      Ref = automonitor_noent(Server, Path),
			      {Path, Ref}
		      end,
		      {with,
		       [fun create_file_subtest/1,
			fun touch_file_subtest/1,
			fun delete_file_subtest/1,
			fun create_file_subtest/1,
			fun touch_file_subtest/1,
			fun auto_change_file_type_subtest/1,
			fun touch_file_subtest/1,
			fun delete_file_subtest/1
		       ]}
		     }
	     end
	    };
	_ ->
	    []
    end.

automonitor_noent(Server, Path) ->
    {ok, Ref, Path} = ?SERVER:automonitor(Server, Path, []),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref, {error, Path, file, enoent}},
			 Msg)
    end,
    Ref.

auto_change_file_type_subtest({Path, Ref}) ->
    assert_empty_mailbox(),
    remove_file(Path),
    make_dir(Path),
    receive
	Msg1 ->
	    ?assertMatch({?MSGTAG, Ref,
			  {error, Path, file, enoent}}, Msg1)
    end,
    receive
	Msg2 ->
	    ?assertMatch({?MSGTAG, Ref,
			  {changed, Path, file,
			   #file_info{type=directory}, []}}, Msg2)
    end,
    %% The type change will cause the server to recreate the
    %% monitor, which will do a single poll of the file. To avoid
    %% handling all possible consequences of races, we wait for this
    %% event before we remove the directory again, to ensure that
    %% the new monitor is set up for the directory while it exists.
    receive
	Msg3 ->
	    ?assertMatch({?MSGTAG, Ref,
			  {found, Path, directory,
			   #file_info{type=directory}, []}}, Msg3)
    end,
    remove_dir(Path),
    write_file(Path),
    receive
	Msg4 ->
	    ?assertMatch({?MSGTAG, Ref,
			  {error, Path, directory, enoent}}, Msg4)
    end,
    receive
	Msg5 ->
	    ?assertMatch({?MSGTAG, Ref,
			  {error, Path, directory, enotdir}}, Msg5)
    end,
    %% wait for the event following the monitor type change
    receive
	Msg6 ->
	    ?assertMatch({?MSGTAG, Ref,
			  {found, Path, file,
			   #file_info{type=regular}, []}}, Msg6)
    end.


%% test utilities

new_test_server() ->
    {ok, Server} = ?SERVER:start(undefined, [{interval, 100}]),
    Server.

stop_test_server(Server) ->
    ?SERVER:stop(Server).

wait(Millis) ->
    receive after Millis -> ok end.

assert_empty_mailbox() ->
    receive MsgX -> throw({unexpected_message, MsgX})
    after 0 -> ok
    end.

write_file(Path) when is_binary(Path) ->
    write_file(binary_to_list(Path));
write_file(Path) ->
    case file:write_file(Path, <<"this is a test\n">>) of
	ok -> ok;
	{error, Err} -> throw({could_not_write, Err, Path})
    end.    

touch_file(Path) ->
    %% we must ensure that the new timestamp is at least one second
    %% older than any previous write, otherwise the change may not be
    %% detected due to the low timestamp resolution
    receive after 1100 -> ok end,
    write_file(Path).

remove_file(Path) when is_binary(Path) ->
    remove_file(binary_to_list(Path));
remove_file(Path) ->
    case file:delete(Path) of
	ok -> ok;
	{error, enoent} -> ok;
	{error, Err} -> throw({could_not_delete, Err, Path})
    end.

remove_dir(Path) when is_binary(Path) ->
    remove_dir(binary_to_list(Path));
remove_dir(Path) ->
    case file:del_dir(Path) of
	ok -> ok;
	{error, enoent} -> ok;
	{error, Err} -> throw({could_not_delete, Err, Path})
    end.

recursive_remove(Path) when is_binary(Path) ->
    recursive_remove(binary_to_list(Path));
recursive_remove(Path) ->
    case file:read_file_info(Path) of
	{ok, #file_info{type=directory}} ->
	    lists:foreach(fun (Sub) ->
				  recursive_remove(join(Path, Sub))
			  end,
			  list_dir(Path)),
	    remove_dir(Path);
	_ ->
	    remove_file(Path)
    end.

make_dir(Path) when is_binary(Path) ->
    make_dir(binary_to_list(Path));
make_dir(Path) ->
    case file:make_dir(Path) of
	ok -> ok;
	{error, Err} -> throw({could_not_make_dir, Err, Path})
    end.    

list_dir(Path) when is_binary(Path) ->
    list_dir(binary_to_list(Path));
list_dir(Path) ->
    case file:list_dir(Path) of
	{ok, Files} -> [list_to_binary(File) || File <- Files];
	{error, _} -> []
    end.

join(Path, File) when is_binary(Path) ->
    join(binary_to_list(Path), File);
join(Path, File) when is_binary(File) ->
    join(Path, binary_to_list(File));
join(Path, File)  ->
    list_to_binary(filename:join(Path, File)).
