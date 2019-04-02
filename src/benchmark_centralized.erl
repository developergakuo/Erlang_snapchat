-module(benchmark_centralized).

-export([test_fib/0, test_homepage/1, test_make_story/1]).

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
    io:format(" ~p ~n", [WallClockTime / 1000.0]).
    %io:format("CPU time = ~p ms~n", [CpuTime]),
    %io:format("~s done~n", [Name]).

%% Benchmarks

test_fib() ->
    run_benchmark("Fibonacci", fun test_fib_benchmark/0, 30).

test_fib_benchmark() ->
    fib(38).

% Creates a server with 5000 users with 10 friends and publishing 10 stories.
initialize_server(NumberOfUsers,NumberOfFriends,NumberOfStories) ->
    rand:seed_s(exsplus, {0, 0, 0}),
    ServerPid = server_centralized:initialize(),
    % Register users
    UserIds = lists:map(fun (_) ->
            {UserId, _} = server:register_user(ServerPid),
            UserId
        end,
        lists:seq(1, NumberOfUsers)),
    % Create their friends
    lists:foreach(fun (UserId) ->
            lists:foreach(fun (_) ->
                    ok = server:befriend(ServerPid, UserId, pick_random(UserIds))
                    % It may happen that we randomly pick a user which we're
                    % already friends with. In that case the # subscriptions
                    % for this user is < 10.
                end,
                lists:seq(1, NumberOfFriends))
        end,
        UserIds),
    % Add some stories
    lists:foreach(fun (UserId) ->
            lists:foreach(fun (_) ->
                    _Timestamp = server:make_story(ServerPid, UserId, "Hello!")
                end,
                lists:seq(1, NumberOfStories))
        end,
        UserIds),
    {ServerPid, UserIds}.

% Get homepage of 10000 users (repeated 30 times).
test_homepage(Args) ->
    NumberOfUsers = erlang:list_to_integer(lists:nth(1, Args)),
    Stories_Count = erlang:list_to_integer(lists:nth(2, Args)),
    Home_page_requests = erlang:list_to_integer(lists:nth(4, Args)),
%%  Init server
    {ServerPid, UserIds} = initialize_server(NumberOfUsers,
        Stories_Count,
        erlang:list_to_integer(lists:nth(3, Args))
    ),
    run_benchmark("homepage",
        fun () ->
            lists:foreach(fun (_) ->
                server:homepage(ServerPid, pick_random(UserIds))
            end,
            lists:seq(1, Home_page_requests))
        end,
        30).

% Publish a story for 1000 users.
test_make_story(Args) ->
    NumberOfUsers = erlang:list_to_integer(lists:nth(1, Args)),
    Stories_Count = erlang:list_to_integer(lists:nth(2, Args)),
%%  Init server
    {ServerPid, UserIds} = initialize_server(NumberOfUsers,
        Stories_Count,
        erlang:list_to_integer(lists:nth(3, Args))),
    run_benchmark("make_story",
        fun () ->
            lists:foreach(fun (_) ->
                server:make_story(ServerPid, pick_random(UserIds), "Test")
            end,
            lists:seq(1, Stories_Count))
        end,
        30).

% Pick a random element from a list.
pick_random(List) ->
    lists:nth(rand:uniform(length(List)), List).
