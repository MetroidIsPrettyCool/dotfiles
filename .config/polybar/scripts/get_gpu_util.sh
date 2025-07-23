#!/bin/bash
# using data from `nvidia-smi`, print current GPU utilization percentage

util=$(nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits)

# if unable to retrieve gpu utilization
if [ $? -ne 0 ]
then
    printf "UNAVAILABLE"
    exit 255
fi

printf '%3d%%' "${util}"
