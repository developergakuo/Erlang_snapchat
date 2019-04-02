%%%-------------------------------------------------------------------
%%% @author gakuo
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Mar 2019 17:45
%%%-------------------------------------------------------------------
-module(server_distributed).
-author("gakuo").

%% API
%% This is a simple implementation of the project, using one centralized server.
%%
%% It will create one "server" actor that contains all internal state (users,
%% their friends, and their stories).
%%
%% This implementation is provided with unit tests, however, these tests are
%% neither complete nor implementation independent, so be careful when reusing
%% them.

-include_lib("eunit/include/eunit.hrl").

-define(EXPIRATION_TIME, 30000). % time until a message expires, in milliseconds
-define(MaXIMUM_HOME_PAGE_SIZE,10).

%%

%% Exported Functions
%%
-export([initialize/1,
  % internal actors
  data_actor/1,receive_home_page_stories/3]).

%%
%% API Functions
%%

% Start server.
% This returns the pid of the server, but you can also use the name "data_actor"
% to refer to it.
initialize(UserId) ->
  ServerPid = spawn(?MODULE, data_actor, [{user, UserId, #{}, 0, sets:new()}]),
  catch unregister(list_to_atom("pID"++integer_to_list(UserId))),
  register(list_to_atom("pID"++integer_to_list(UserId)),ServerPid),
  ServerPid.

% The data actor works like a small database and encapsulates all state of this
% simple implementation.
%
% The Data is a tuple the form {user, UserId, Stories, LastStoryId, Friends}
% with Stories a map StoryId -> {story, UserId, Timestamp, Text}.
% Friends is a list of UserIDs
% FriendsServerIds is a list of PIDs



data_actor(Data) ->
  receive
    {Sender, homepage, UserId} ->
      homepage(Sender,Data, UserId),
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
      data_actor(NewData);

    {_Sender, second_befriend, UserId, NewFriendId}->
      NewData = befriend2(Data, UserId, NewFriendId),
      data_actor(NewData);

    {_Sender, destruct_story, UserId, StoryId} ->
          NewData = destruct_story(Data, UserId, StoryId),
          data_actor(NewData)
  end.

%%
%% Internal Functions
%%

homepage(Sender,Data, UserId) ->
  {user, UserId, _, _, Friends}=Data,
  aggregate_home_page(Sender,UserId,Friends).

%{user, UserId, Stories, LastStoryId, Friends}
stories(Data, UserId) ->
  {user, UserId, Stories, _,_}= Data,
  % Sort stories by third element (timestamp), reversed
  lists:reverse(lists:keysort(3, maps:values(Stories))).

%{user, UserId, Stories, LastStoryCount, Friends}
make_story(Data, UserId, Text) ->
  {user, UserId, Stories, LastStoryCount, Friends}=Data,
  StoryId=string:concat(integer_to_list(UserId),integer_to_list(LastStoryCount+1)),
  Timestamp = os:timestamp(),
  NewStories = maps:put(StoryId, {story, UserId, Timestamp, Text}, Stories),
  {{user, UserId, NewStories, LastStoryCount+1, Friends}, Timestamp, StoryId}.
%{user, UserId, Stories, LastStoryId, Friends}
destruct_story(Data, UserId, StoryId) ->
  {user, UserId, Stories, LastStoryCount, Friends}=Data,
  NewStories = maps:remove(StoryId, Stories),
  {user, UserId, NewStories, LastStoryCount, Friends}.

%{user, UserId, Stories, LastStoryId, Friends}
befriend(Data, UserId, NewFriendId) ->
  {user, UserId, Stories, LastStoryCount, Friends}=Data,
  NewFriends = sets:add_element(NewFriendId, Friends),
  list_to_atom("pID"++integer_to_list(NewFriendId))   ! {self(), second_befriend, NewFriendId, UserId},
  {user, UserId, Stories, LastStoryCount, NewFriends}.

befriend2(Data, UserId, NewFriendId) ->
  {user, UserId, Stories, LastStoryCount, Friends}=Data,
  NewFriends = sets:add_element(NewFriendId, Friends),
  {user, UserId, Stories, LastStoryCount, NewFriends}.

aggregate_home_page(RequesterID,UserId,Friends)->
  FriendsCount = sets:size(Friends),
  PID=spawn(?MODULE,receive_home_page_stories,[RequesterID,UserId,{[],0,FriendsCount}]),
  case FriendsCount of
        0-> PID ! {self(), stories, UserId, []};
      _ ->   lists:foreach( fun(Friend) -> list_to_atom("pID"++integer_to_list(Friend))! {PID, get_stories, Friend} end,
                                 sets:to_list(Friends))
  end.


receive_home_page_stories(RequesterID,UserId,StoriesTuple) ->
  {AccumulatedStories , ReceivedCount, FriendsCount}= StoriesTuple,
  receive
    {_ResponsePid, stories, UserId, _Stories} when FriendsCount == 0  ->
      RequesterID  ! {self(), homepage, UserId, []},
      exit(normal);

    {_ResponsePid, stories, _, Stories} when ReceivedCount == (FriendsCount-1)->
      NewAccumulatedStories=AccumulatedStories ++ Stories,
      SortedStories = lists:reverse(lists:keysort(3,NewAccumulatedStories)),
      RequesterID ! {self(), homepage, UserId,lists:sublist(SortedStories, 10)},
      exit(normal);

    {_ResponsePid, stories, _, Stories}->
      NewAccumulatedStories=AccumulatedStories ++ Stories,
      NewReceivedCount = ReceivedCount +1,
      receive_home_page_stories(RequesterID,UserId,{NewAccumulatedStories,NewReceivedCount,FriendsCount})
  end.



%%
%% Test Functions
%%
%% These tests are for this specific implementation. They are a partial
%% definition of the semantics of the provided interface but also make certain
%% assumptions of its implementation. You can thus reuse them, but may need to
%% modify them.
%%

initialization_test() ->
  initialize(0).

init_for_test() ->
  Pid1=initialize(1),
  Pid2 = initialize(2),
  Pid3 = initialize(3),
  Pid4 = initialize(4),
  [Pid1, Pid2, Pid3, Pid4].


users_stories_test() ->
  Pids = init_for_test(),
  [Pid1, Pid2 | _ ] = Pids,

  ?assertMatch([], server:get_stories(Pid1, 1)),
  ?assertMatch([], server:get_stories(Pid2, 2)).

homepage_test() ->
  Pids = init_for_test(),
  [Pid1, Pid2 | _ ] = Pids,

  ?assertMatch([], server:homepage(Pid1, 1)),
  ?assertMatch([], server:homepage(Pid2, 2)).

story_test() ->
  Pids = init_for_test(),
  [Pid1, Pid2 | _ ] = Pids,

  ?assertMatch([], server:homepage(Pid1, 1)),
  ?assertMatch([], server:homepage(Pid2, 2)),

  ?assertMatch({_MegaSecs, _Secs, _MicroSecs},
    server:make_story(Pid1, 1, "My first story")),

  ?assertMatch([{story, _, _, "My first story"}], server:get_stories(Pid1, 1)),
  ?assertMatch([], server:get_stories(Pid2, 2)),

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


