#!/bin/bash
# using data from `nvidia-smi`, print current GPU temperature

temp_c=$(nvidia-smi --query-gpu=temperature.gpu --format=csv,noheader,nounits)

# if unable to retrieve gpu temp
if [[ $? -ne 0 ]]; then
    printf "UNAVAILABLE"
    exit 255
fi


# temp_f=$(($temp_c * 9 / 5 + 32))

# echo $temp_c°C/$temp_f°F
printf "%s°C" "${temp_c}"

# if temp is 80C or higher, return non-zero exit code to inform polybar to use "warning" colors
if [[ $temp_c -ge 80 ]]; then
    exit 255
else
    exit 0
fi
