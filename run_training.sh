#! /bin/bash

make
echo "" > log.txt

for i in {1..10}
do
    ./wuerfeln >> log.txt
    echo "*******************************************" >> log.txt
    sleep 5
done

