#! /bin/bash

if [ -z $1 ]
then
    algo='x'
else
    algo=$1
fi

if [ -z $2 ]
then
    name='olddice'
else
    name=$2
fi

logfile="$algo.log"

make
echo "" > "$logfile"

for i in {1..500}
do
    ./wuerfeln >> x.log
    echo "*******************************************" >> "$logfile"
done

