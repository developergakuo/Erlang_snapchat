#!/usr/bin/env bash

threads1=8
friends=10
stories=10
requests=5000
for (( u = 500 ; u <= 2500; u += 1000 ))
do
        echo "---"
        echo "> homepage, $threads1 threads, $u users (10 friends per user, 10 stories per user)"
        erl +S $threads1 -noshell -run benchmark_distributed test_homepage $u $friends $stories $requests -s init stop  > out-dis-hp-users-$u.csv
        echo "---"
        echo "> make_story, $threads1 threads, $u users (10 friends per user, 10 stories per user)"

        erl +S $threads1 -noshell -run benchmark_distributed test_make_story $u $friends $stories $requests -s init stop > out-dis-ms-users-$u.csv
        echo "---"
        echo "> homepage, $threads1 threads, $u users (10 friends per user, 10 stories per user)"
        erl +S $threads1 -noshell -run benchmark_centralized test_homepage $u $friends $stories $requests -s init stop  > out-cen-hp-users-$u.csv
        echo "---"
        echo "> make_story, $threads1 threads, $u users (10 friends per user, 10 stories per user)"
        erl +S $threads1 -noshell -run benchmark_centralized test_make_story $u $friends $stories $requests -s init stop > out-cen-ms-users-$u.csv
        echo "---"

done

for ((f = 10; f<=50; f+=10))
do
        echo "---"
        echo "> homepage, $threads1 threads, $f friends per user (5000 users, 10 stories per user)"
        erl +S $threads1 -noshell -runbenchmark_distributed test_homepage 5000 $f 10 5000 -s init stop  > out-dis-hp-friends-$f.csv
        echo "---"
        echo "> make_story, $threads1 threads, $f friends per user (5000 users, 10 stories per user)"
        erl +S $threads1 -noshell -run benchmark_distributed test_make_story 5000 $f 10 5000 -s init stop > out-dis-ms-friends-$f.csv

         echo "---"
        echo "> homepage, $threads1 threads, $f friends per user (5000 users, 10 stories per user)"
        erl +S $threads1 -noshell -run benchmark_centralized test_homepage 5000 $f 10 5000 -s init stop  > out-cen-hp-friends-$f.csv
        echo "---"
        echo "> make_story, $threads1 threads, $f friends per user (5000 users, 10 stories per user)"
        erl +S $threads1 -noshell -run benchmark_centralized test_make_story 5000 $f 10 5000 -s init stop > out-cen-ms-friends-$f.csv

done

for ((s = 10; s<=50; s+=10))
do
        echo "---"
        echo "> homepage, 32 threads, $s stories per user (5000 users, 10 friends per user)"
        erl +S threads1 -noshell -run benchmark_distributed test_homepage 5000 10 $s 5000 -s init stop  > out-dis-hp-stories-$s.csv
        echo "---"
        echo "> make_story, 32 threads, $s stories per user (5000 users, 10 friends per user)"
        erl +S threads1 -noshell -run benchmark_distributed test_make_story 5000 10 $s 5000 -s init stop > out-dis-ms-stories-$s.csv

        echo "> homepage, 32 threads, $s stories per user (5000 users, 10 friends per user)"
        erl +S threads1 -noshell -run benchmark_centralized test_homepage 5000 10 $s 5000 -s init stop  > out-cen-hp-stories-$s.csv
        echo "---"
        echo "> make_story, 32 threads, $s stories per user (5000 users, 10 friends per user)"
        erl +S threads1 -noshell -run benchmark_centralized test_make_story 5000 10 $s 5000 -s init stop > out-cen-ms-stories-$s.csv

done

for ((i = 4; i<=12; i+=4))
do
        echo "---"
        echo "> homepage, $i threads (5000 users, 10 friends per user, 10 stories per user)"
        erl +S $i -noshell -run benchmark_distributed test_homepage 5000 10 10 5000 -s init stop  > out-hp-threads-$i.csv
        echo "---"
        echo "> make_story, $i threads (5000 users, 10 friends per user, 10 stories per user)"
        erl +S $i -noshell -run benchmark_distributed test_make_story 5000 10 10 5000 -s init stop > out-ms-threads-$i.csv

done

for ((s = 1000; s<=7000; s+=2000))
do
        echo "---"
        echo "> homepage, 32 threads, $s stories per user (5000 users, 10 friends per user)"
        erl +S threads1 -noshell -run benchmark_distributed test_homepage 5000 10 10 $s  -s init stop  > out-dis-hp-requests-$s.csv

        echo "---"
        echo "> homepage, 32 threads, $s stories per user (5000 users, 10 friends per user)"
        erl +S threads1 -noshell -run benchmark_centralized test_homepage 5000 10 10 $s -s init stop  > out-cen-hp-requests-$s.csv
        echo "---"
        echo "> make_story, 32 threads, $s stories per user (5000 users, 10 friends per user)"
        erl +S threads1 -noshell -run benchmark_centralized test_make_story 5000 10 10 $s -s init stop > out-cen-ms-requests-$s.csv

done

