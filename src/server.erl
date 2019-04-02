%% This module provides the protocol that is used to interact with an
%% implementation of a messaging service.
%%
%% The interface is design to be synchrounous: it waits for the reply of the
%% system.
%%
%% This module defines the public API that is supposed to be used for
%% experiments. The semantics of the API here should remain unchanged.
-module(server).

-export([register_user/1,
         befriend/3,
         homepage/2,
         get_stories/2,
         make_story/3]).

%%
%% Server API
%%

% Register a new user. Returns its id and a pid that should be used for
% subsequent requests by this client.
-spec register_user(pid()) -> {integer(), pid()}.
register_user(ServerPid) ->
    ServerPid ! {self(), register_user},
    receive
        {ResponsePid, registered_user, UserId} -> {UserId, ResponsePid}
    end.

% Become friends (follow) another user.
-spec befriend(pid(), integer(), integer()) -> ok.
befriend(ServerPid, UserId, NewFriendId) ->
    ServerPid ! {self(), befriend, UserId, NewFriendId},
    receive
        {_ResponsePid, befriended, UserId, NewFriendId} -> ok
    end.

% Request the homepage of a user.
-spec homepage(pid(), integer()) -> [{story, integer(), erlang:timestamp(), string()}].
homepage(ServerPid, UserId) ->
    ServerPid ! {self(), homepage, UserId},
    receive

        {_ResponsePid, homepage, UserId, Timeline} ->
            Timeline
    end.

% Request the stories of a user.
-spec get_stories(pid(), integer()) -> [{story, integer(), erlang:timestamp(), string()}].
get_stories(ServerPid, UserId) ->
    ServerPid ! {self(), get_stories, UserId},
    receive
        {_ResponsePid, stories, UserId, Stories} ->
            Stories
    end.

% Publish a story for a user.
% (Authorization/security are not regarded in any way.)
-spec make_story(pid(), integer(), string()) -> erlang:timestamp(). 
make_story(ServerPid, UserId, Text) ->
    ServerPid ! {self(), make_story, UserId, Text},
    receive
        {_ResponsePid, story_published, UserId, Timestamp} ->
            Timestamp
    end.
