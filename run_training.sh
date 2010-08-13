#! /bin/bash

logfile=$1

make
echo "" > "$logfile"

for i in {1..200}
do
    ./wuerfeln >> "$logfile"
    echo "*******************************************" >> "$logfile"
    sleep 5
done

