%% This is a simple implementation of the project, using one centralized server.
%%
%% It will create one "server" actor that contains all internal state (users,
%% their friends, and their stories).
%%
%% This implementation is provided with unit tests, however, these tests are
%% neither complete nor implementation independent, so be careful when reusing
%% them.
-module(server_centralized).

-include_lib("eunit/include/eunit.hrl").

-define(EXPIRATION_TIME, 30000). % time until a message expires, in milliseconds

%%
%% Exported Functions
%%
-export([initialize/0,
         % internal actors
         data_actor/1]).

%%
%% API Functions
%%

% Start server.
% This returns the pid of the server, but you can also use the name "data_actor"
% to refer to it.
initialize() ->
    ServerPid = spawn_link(?MODULE, data_actor, [[]]),
    register(data_actor, ServerPid),
    ServerPid.

% The data actor works like a small database and encapsulates all state of this
% simple implementation.
%
% The Data is a list of {user, UserId, Stories, LastStoryId, Friends}
% with Stories a map StoryId -> {story, UserId, Timestamp, Text}.
data_actor(Data) ->
    receive
        {Sender, register_user} ->
            {NewData, NewUserId} = add_new_user(Data),
            Sender ! {self(), registered_user, NewUserId},
            data_actor(NewData);

        {Sender, homepage, UserId} ->
            Sender ! {self(), homepage, UserId, homepage(Data, UserId)},
            data_actor(Data);

        {Sender, get_stories, UserId} ->
            Sender ! {self(), stories, UserId, stories(Data, UserId)},
            data_actor(Data);

        {Sender, make_story, UserId, Text} ->
            {NewData, Timestamp, StoryId} = make_story(Data, UserId, Text),
            Sender ! {self(), story_published, UserId, Timestamp},
            % send_after sends the destruct message after 30 seconds
            timer:send_after(?EXPIRATION_TIME, self(),
                {self(), destruct_story, UserId, StoryId}),
            data_actor(NewData);




        {Sender, befriend, UserId, NewFriendId} ->
            NewData = befriend(Data, UserId, NewFriendId),
            Sender ! {self(), befriended, UserId, NewFriendId},
            data_actor(NewData)
    end.

%%
%% Internal Functions
%%

add_new_user(Data) ->
    NewUserId = length(Data),
    % We store the stories in a map id => story.
    % See https://erldocs.com/current/stdlib/maps.html
    NewData = Data ++ [{user, NewUserId, #{}, 0, sets:new()}],
    {NewData, NewUserId}.

homepage(Data, UserId) ->
    {user, UserId, _, _, Friends} = lists:nth(UserId + 1, Data),

    UnsortedStories =
        lists:foldl(fun(FriendId, AccStories) ->
                        {_, _, FriendStories, _, _} = lists:nth(FriendId + 1, Data),
                        AccStories ++ maps:values(FriendStories)
                    end,
                    [],
                    sets:to_list(Friends)),
    % Sort stories by third element (timestamp), reversed
    SortedStories = lists:reverse(lists:keysort(3, UnsortedStories)),
    lists:sublist(SortedStories, 10).

stories(Data, UserId) ->
    {user, UserId, Stories, _, _} = lists:nth(UserId + 1, Data),
    % Sort stories by third element (timestamp), reversed
    lists:reverse(lists:keysort(3, maps:values(Stories))).

make_story(Data, UserId, Text) ->
    {user, UserId, Stories, LastStoryId, Friends} = lists:nth(UserId + 1, Data),
    StoryId = LastStoryId + 1,
    Timestamp = os:timestamp(),
    NewStories = maps:put(StoryId, {story, UserId, Timestamp, Text}, Stories),
    NewUser = {user, UserId, NewStories, StoryId, Friends},

    {UsersBefore, [_|UsersAfter]} = lists:split(UserId, Data),
    {lists:append([UsersBefore, [NewUser | UsersAfter]]), Timestamp, StoryId}.

destruct_story(Data, UserId, StoryId) ->
    {user, UserId, Stories, LastStoryId, Friends} = lists:nth(UserId + 1, Data),
    NewStories = maps:remove(StoryId, Stories),
    NewUser = {user, UserId, NewStories, LastStoryId, Friends},

    {UsersBefore, [_|UsersAfter]} = lists:split(UserId, Data),
    lists:append([UsersBefore, [NewUser | UsersAfter]]).

befriend(Data, UserId, NewFriendId) ->
    {user, UserId, Stories, LastStoryId, Friends} = lists:nth(UserId + 1, Data),
    NewFriends = sets:add_element(NewFriendId, Friends),
    NewUser = {user, UserId, Stories, LastStoryId, NewFriends},

    {UsersBefore, [_|UsersAfter]} = lists:split(UserId, Data),
    lists:append([UsersBefore, [NewUser | UsersAfter]]).

%%
%% Test Functions
%% 
%% These tests are for this specific implementation. They are a partial
%% definition of the semantics of the provided interface but also make certain
%% assumptions of its implementation. You can thus reuse them, but may need to
%% modify them.
%%

initialization_test() ->
    catch unregister(data_actor),
    initialize().

register_user_test() ->
    ServerPid = initialization_test(),

    % We assume here that everything is sequential, and we have simple
    % incremental ids
    ?assertMatch({0, _Pid1}, server:register_user(ServerPid)),
    ?assertMatch({1, _Pid2}, server:register_user(ServerPid)),
    ?assertMatch({2, _Pid3}, server:register_user(ServerPid)),
    ?assertMatch({3, _Pid4}, server:register_user(ServerPid)).

init_for_test() ->
    ServerPid = initialization_test(),
    {0, Pid1} = server:register_user(ServerPid),
    {1, Pid2} = server:register_user(ServerPid),
    {2, Pid3} = server:register_user(ServerPid),
    {3, Pid4} = server:register_user(ServerPid),
    [Pid1, Pid2, Pid3, Pid4].

homepage_test() ->
    Pids = init_for_test(),
    [Pid1, Pid2 | _ ] = Pids,

    ?assertMatch([], server:homepage(Pid1, 1)),
    ?assertMatch([], server:homepage(Pid2, 2)).

users_stories_test() ->
    Pids = init_for_test(),
    [Pid1 | _ ] = Pids,

    ?assertMatch([], server:get_stories(Pid1, 1)),
    ?assertMatch([], server:get_stories(Pid1, 2)).

story_test() ->
    Pids = init_for_test(),
    [Pid1, Pid2 | _ ] = Pids,

    ?assertMatch([], server:homepage(Pid1, 1)),
    ?assertMatch([], server:homepage(Pid2, 2)),

    ?assertMatch({_MegaSecs, _Secs, _MicroSecs},
        server:make_story(Pid1, 1, "My first story")),

    ?assertMatch([{story, _, _, "My first story"}], server:get_stories(Pid1, 1)),
    ?assertMatch([], server:get_stories(Pid1, 2)),

    Pids. % no friends

befriend_test() ->
    [Pid1, Pid2 | _ ] = story_test(),

    ?assertMatch(ok, server:befriend(Pid2, 2, 1)),

    % now there is a friend relation, so the first story should be visible
    ?assertMatch([{story, _, _, "My first story"}], server:homepage(Pid2, 2)), 

    % publish a second story and check whether it's visible
    ?assertMatch({_MegaSecs, _Secs, _MicroSecs},
        server:make_story(Pid1, 1, "My second story")),
    ?assertMatch([{story, _, _, "My second story"},
                  {story, _, _, "My first story"}],
                 server:homepage(Pid2, 2)),
    done. 
