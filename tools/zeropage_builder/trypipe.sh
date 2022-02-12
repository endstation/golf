#!/bin/bash

myvar=$(< /dev/stdin)
echo "myvar=${myvar}"
printf "%02x\n" ${myvar}


