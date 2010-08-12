#! /bin/bash

make

for i in {1..10}
do
    ./wuerfeln >> log.txt
    echo "*******************************************" >> log.txt
    sleep 5
done

