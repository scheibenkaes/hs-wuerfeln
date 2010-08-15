#! /bin/bash

if [ -z $1 ]
then
    algo='x'
else
    algo=$1
fi

logfile="$algo.log"

make
echo "" > "$logfile"

for i in {1..200}
do
    ./wuerfeln >> "$logfile"
    echo "*******************************************" >> "$logfile"
    sleep 5
done

