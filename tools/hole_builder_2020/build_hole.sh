#!/bin/bash

HOLES_DIR="../../assets/holes/"
COURSE_NAMES=("beechings" "sierre" "alab" "hokusai" "carver" "newyork")

echo "Course: ${COURSE_NAMES[$1]}"
echo "Building hole #${2}..."
coursenum=$1
coursename=${COURSE_NAMES[$1]}
holenum=$2
#tmxfile=$(printf '%stop_hole_%02d.tmx' $HOLES_DIR ${holenum})
#outputbin=$(printf '%stophole%02d.bin' $HOLES_DIR ${holenum})
tmxfile=$(printf '%s%s_%02d.tmx' $HOLES_DIR ${COURSE_NAMES[$coursenum]} ${holenum})
outputbin=$(printf '%s%s_%02d.bin' $HOLES_DIR $coursename ${holenum})

echo "tmxfile=${tmxfile}"
echo "outputbin=${outputbin}"

# Build the hole.
if ! pike hole_builder3.pike ${tmxfile} ${outputbin}
then
    echo "error building hole"
    exit 1
fi

# Now create the overhead map.
#output_png=$(printf '%stopholemap%02d.png' $HOLES_DIR ${holenum})
#output_png_large=$(printf '%stopholemap%02d_original.png' $HOLES_DIR ${holenum})
output_png=$(printf '%s%s_%02d_omap.png' $HOLES_DIR ${coursename} ${holenum})
output_png_large=$(printf '%s%s_%02d_omap_original.png' $HOLES_DIR ${coursename} ${holenum})

# Check if there's a 'finished' map for this hole in the 'touched_up_maps'
# subdirectory.  If there is and it's more recent than what's in holes/,
# set a flag so we know to use it later.
finished_map=$(printf '%s/touched_up_maps/%s_%02d_omap_finished.png' \
    $HOLES_DIR ${coursename} ${holenum})
if [ -e ${finished_map} ] && [ ${finished_map} -nt ${tmxfile} ]
then
    png_to_use=${finished_map}
else
    png_to_use=${output_png}
fi

if ! pike overhead.pike ${tmxfile} ${output_png} ${output_png_large}
then
    echo "error building pngs"
    exit 1
fi

# Create binary from overhead map.
map_bin=$(printf '%s%s_%02d_omap.bin' ${HOLES_DIR} ${coursename} ${holenum})
#if ! pike icon_generator.pike ${output_png} ${map_bin} 4
if ! pike icon_generator.pike ${png_to_use} ${map_bin} 4
then
    echo "error building overhead map binary"
    exit 1
fi

# Edit binary so that icon will be drawn in correct position (first two bytes).
if ! pike hexedit.pike ${map_bin}
then
    echo "error EDITING overhead map binary"
    exit 1
fi

# Now put the hole and map binaries together!
final_bin=$(printf '%sh%02d%02d.bin' ${HOLES_DIR} ${coursenum} ${holenum})
if ! cat ${outputbin} ${map_bin} >${final_bin}
then
    echo "error concatenating binaries"
    exit 1
fi


