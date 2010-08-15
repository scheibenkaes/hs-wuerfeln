#! /bin/bash

if [ -z $1 ]
then
    echo "Algo angeben!"
    exit
fi

algo=$1
logfile="$1.log"

make
echo "" > "$logfile"

for i in {1..200}
do
    ./wuerfeln >> "$logfile"
    echo "*******************************************" >> "$logfile"
    sleep 5
done

