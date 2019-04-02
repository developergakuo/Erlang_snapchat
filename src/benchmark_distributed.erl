%%%-------------------------------------------------------------------
%%% @author gakuo
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Mar 2019 21:38
%%%-------------------------------------------------------------------
-module(benchmark_distributed).
-author("gakuo").

%% API

-export([test_fib/0, test_homepage/1, test_make_story/1, initialize_server/3,get_home_page/3,await_test_results/2
,make_a_story/4]).

%% Fibonacci
fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).

%% Benchmark helpers

% Recommendation: run each test at least 30 times to get statistically relevant
% results.
run_benchmark(Name, Fun, Times) ->
  ThisPid = self(),
  lists:foreach(fun (N) ->
    % Recommendation: to make the test fair, each run executes in its own,
    % newly created Erlang process. Otherwise, if all tests run in the same
    % process, the later tests start out with larger heap sizes and
    % therefore probably do fewer garbage collections. Also consider
    % restarting the Erlang emulator between each test.
    % Source: http://erlang.org/doc/efficiency_guide/profiling.html
    spawn_link(fun () ->
      run_benchmark_once(Name, Fun, N),
      ThisPid ! done
               end),
    receive done ->
      ok
    end
                end, lists:seq(1, Times)).

run_benchmark_once(_Name, Fun, _N) ->
  %io:format("Running benchmark ~s: ~p~n", [Name, N]),

  % Start timers
  % Tips:
  % * Wall clock time measures the actual time spent on the benchmark.
  %   I/O, swapping, and other activities in the operating system kernel are
  %   included in the measurements. This can lead to larger variations.
  %   os:timestamp() is more precise (microseconds) than
  %   statistics(wall_clock) (milliseconds)
  % * CPU time measures the actual time spent on this program, summed for all
  %   threads. Time spent in the operating system kernel (such as swapping and
  %   I/O) is not included. This leads to smaller variations but is
  %   misleading.
  StartTime = os:timestamp(), % Wall clock time
  %statistics(runtime),       % CPU time, summed for all threads

  % Run
  Fun(),

  % Get and print statistics
  % Recommendation [1]:
  % The granularity of both measurement types can be high. Therefore, ensure
  % that each individual measurement lasts for at least several seconds.
  % [1] http://erlang.org/doc/efficiency_guide/profiling.html
  WallClockTime = timer:now_diff(os:timestamp(), StartTime),
  %{_, CpuTime} = statistics(runtime),
  io:format(" ~p~n", [WallClockTime / 1000.0]).
  %io:format("CPU time = ~p ms~n", [CpuTime]),
  %io:format("~s done~n", [Name]).

%% Benchmarks

test_fib() ->
  run_benchmark("Fibonacci", fun test_fib_benchmark/0, 30).

test_fib_benchmark() ->
  fib(38).

% Creates a server with 5000 users with 10 friends and publishing 10 stories.
initialize_server(Users_count,Friends_count,Stories_count) ->
  rand:seed_s(exsplus, {0, 0, 0}),
  % Register users
  UserIds = lists:seq(1, Users_count),

  User_PIDs_pairs = lists:map(fun (UserId) ->
    {UserId,server_distributed:initialize(UserId)}
                      end,
                 UserIds),
  % Create their friends
  lists:foreach(fun ({UserId,Pid}) ->
    lists:foreach(fun (_) ->
      ok = server:befriend(Pid, UserId, pick_random(UserIds))
    % It may happen that we randomly pick a user which we're
    % already friends with. In that case the # subscriptions
    % for this user is < 10.
                  end,
      lists:seq(1, Friends_count))
                end,
    User_PIDs_pairs),
  % Add some stories
  lists:foreach(fun ({UserId,Pid}) ->
    lists:foreach(fun (_) ->
      _Timestamp = server:make_story(Pid, UserId, "Hello!")
                  end,
      lists:seq(1,Stories_count ))
                end,
    User_PIDs_pairs),
  User_PIDs_pairs.

% Get homepage of 10000 users (repeated 30 times).
test_homepage(Args) ->
  NumberOfUsers = erlang:list_to_integer(lists:nth(1, Args)),
  Stories_Count = erlang:list_to_integer(lists:nth(2, Args)),
  Home_page_requests = erlang:list_to_integer(lists:nth(4, Args)),
%%  Init server
  User_PIDs_pairs = initialize_server(NumberOfUsers,
    Stories_Count,
    erlang:list_to_integer(lists:nth(3, Args))
    ),


  run_benchmark("homepage",
    fun () ->
      TestPid= self(),
      TestAwaiterPID=spawn(?MODULE,await_test_results, [Home_page_requests,TestPid]),
      lists:foreach(fun (_) ->
        {UserId,ServerPid} =pick_random(User_PIDs_pairs),
        spawn(?MODULE,get_home_page,[ServerPid, UserId,TestAwaiterPID])
                    end,
        lists:seq(1, Home_page_requests)),
      receive
        test_completed -> done
      end
    end,
    30).

get_home_page(ServerPid, UserId, TestAwaiterPID)->
  _Timeline=server:homepage(ServerPid, UserId),
  TestAwaiterPID ! {test_step_done, UserId}.

await_test_results(RunCounts,TestPid) ->
  receive
    {test_step_done, _UserId} when RunCounts == 1->
      TestPid ! test_completed;
    {test_step_done, _UserId} ->
      await_test_results((RunCounts-1),TestPid)
  end.

% Publish a story for 1000 users.
test_make_story(Args) ->
  NumberOfUsers = erlang:list_to_integer(lists:nth(1, Args)),
  Stories_Count = erlang:list_to_integer(lists:nth(2, Args)),
  _Home_page_requests = erlang:list_to_integer(lists:nth(4, Args)),
%%  Init server
  User_PIDs_pairs = initialize_server(NumberOfUsers,
    Stories_Count,
    erlang:list_to_integer(lists:nth(3, Args))
  ),

  run_benchmark("make_story",
    fun () ->
      TestPid= self(),
      TestAwaiterPID=spawn(?MODULE,await_test_results, [Stories_Count,TestPid]),
      lists:foreach(fun (_) ->
        {UserId,ServerPid} =pick_random(User_PIDs_pairs),
        spawn(?MODULE,make_a_story,[ServerPid, UserId,"Test",TestAwaiterPID])

                    end,
        lists:seq(1, Stories_Count)),
      receive
      test_completed -> done
      end
    end,
    30).
make_a_story(ServerPid, UserId, Story, TestAwaiterPID)->
  _Timeline=server:make_story(ServerPid, UserId,Story),
  TestAwaiterPID ! {test_step_done, UserId}.
% Pick a random element from a list.
pick_random(List) ->
  lists:nth(rand:uniform(length(List)), List).
