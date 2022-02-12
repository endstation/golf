#!/bin/bash

course_index=$1
TREES_DIR=$2
COURSES=("beechings" "sierre" "alab" "hokusai" "carver" "newyork")

if ! pike tile_generator.pike ${course_index} "${TREES_DIR}"
then
    echo "Cannot generate tiles for course #${course_index}!"
    exit 1
fi

if ! pike patterns.pike ${course_index} "${TREES_DIR}"
then
    echo "Cannot build tree patterns for course #${course_index}!"
    exit 1
fi

if ! pike process_config.pike ${course_index} "${TREES_DIR}"
then
    echo "Cannot process tree config for course #${course_index}!"
    exit 1
fi

mydir=${TREES_DIR}/${COURSES[$course_index]}
echo "mydir=${mydir}"
#myfile=$(printf 't%02d.bin' ${course_index})
#echo "myfile=${myfile}"
cd ${mydir}
#cat config.bin patterns.bin tiles.bin masks.bin >trees.bin
cat config.bin patterns.bin masks.bin >trees.bin



