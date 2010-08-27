#! /bin/bash

if [ -z $1 ]
then
    algo='x'
else
    algo=$1
fi

if [ -z $2 ]
then
    name=''
else
    name=$2
fi

logfile="$algo.log"

make
echo "" > "$logfile"

for i in {1..200}
do
    ./wuerfeln -s127.0.0.1 -n$name $algo >> "$logfile"
    echo "*******************************************" >> "$logfile"
done

