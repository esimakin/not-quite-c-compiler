#!/bin/bash

ASM_EXT=".s"
GCC_PROGRAM_NAME="gcc_program"
NQCC_PROGRAM_NAME="nqcc_program"

cargo build --release
echo "NQCC build finished"

for file in tests/*.c
do
    gcc -m32 $file -o $GCC_PROGRAM_NAME
    ./$GCC_PROGRAM_NAME
    GCC_RET_VAL=$?
    rm -f $GCC_PROGRAM_NAME
    ./target/release/nqcc $file
    ASM_FILE="${file%%.*}$ASM_EXT"
    gcc -m32 $ASM_FILE -o $NQCC_PROGRAM_NAME
    rm -f $ASM_FILE
    ./$NQCC_PROGRAM_NAME
    NQCC_RET_VAL=$?
    rm -f $NQCC_PROGRAM_NAME
    if [ $GCC_RET_VAL -eq $NQCC_RET_VAL ]
    then
        echo "Testing $file ...ok"
    else
        echo "Testing $file ...FAILED"
        exit 1
    fi 
done
