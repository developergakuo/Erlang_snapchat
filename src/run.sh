#!/bin/sh
# module function  args = NumberOfFriends ,NumberOfStories Times
erl -noshell +S 4:4 -run $1 $2 $3 $4 $5 -s init stop
