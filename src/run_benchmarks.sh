#!/usr/bin/env bash

friends=10
stories=10
users=5000
requests=10000


for (( k = 4 ; k <= 12; k += 3 ))
do
    while [ $users -le 30000 ]
    do
     echo "> homepage, $k threads, $users users , $friends friends, $stories -stories."
    erl +S $k -noshell -run benchmark_distributed test_homepage $users $friends $stories $requests -s init stop > output-homepage-$k-$users-$friends-$stories-$requests.csv
     echo "> Stories, $k threads"
    erl +S $k -noshell -run benchmark_distributed test_make_story $users $friends $stories -s init stop > output-test_make_story-$k-$users-$friends-$stories.csv
    echo "---"

    friends=$(($friends+10))
    users=$(($users*5))
    stories=$(($stories+5))
    done

    friends=10
    users=1000
    stories=10
    requests=10000

done
