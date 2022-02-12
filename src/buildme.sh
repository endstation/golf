#!/bin/bash
# Copyright 2020 Matthew Clarke

BASE_DIR="/home/matthew/hp2_progs/programming/6510assembly/golfgit/commodore-golf"
SRC_DIR="${BASE_DIR}/src"
export ACME="${SRC_DIR}"
ASSETS_DIR="${BASE_DIR}/assets"
TOOLS_DIR="${BASE_DIR}/tools"
BACKDROPS_DIR="${ASSETS_DIR}/backdrops"
TREES_DIR="${ASSETS_DIR}/trees"

cd ${TOOLS_DIR}/zeropage_builder
if ! pike zp_builder.pike 
then
    echo "Cannot build 'zero_page.asm' from template!"
    exit
fi

cd ${TOOLS_DIR}/cbloader
if ! ./build_loader.sh
then
    echo "Cannot build loader!"
    exit
fi

# Make sure splash screen is up-to-date.
cd ${TOOLS_DIR}
if ! ./process_splash.pl
then
    echo "Cannot process splash!"
    exit
fi

# Club velocities for punch shots.
# May want to experiment with input values!
speed_reduction="0.9"
angle_reduction="0.33"
cd ${TOOLS_DIR}/clubs
if ! pike punch_creator.pike ${speed_reduction} ${angle_reduction}
then
    echo "Cannot create punch velocities!"
    exit
fi

# Build the core.
cd ${SRC_DIR}/core
if ! acme -l labels.asm birdie.asm
then
    echo "Build aborted!"
    exit
fi
grep -v "^i[ ]*=" labels.asm >tmpfile
mv tmpfile labels.asm
myline=`grep end_of_loader labels.asm`
myaddr=`get_c64_label_value "${myline}"`
exomizer sfx ${myaddr} birdie.o -o birdie.prg

# 'splash'
cd ${SRC_DIR}/splash
if ! acme -l labels.txt splash.asm
then
    echo "Build aborted!"
    exit
fi
exomizer level -osplash.prg splash.o

# 'finale'
cd ${SRC_DIR}/finale
if ! acme -l labels.txt finale.asm
then
    echo "Cannot assemble 'finale' module!"
    exit
fi
exomizer level -ofinale.prg finale.o

# 'nineteenth'
cd ${SRC_DIR}/nineteenth
if ! acme -l labels.txt nineteenth.asm
then
    echo "Cannot assemble 'nineteenth' module!"
    exit
fi
exomizer level -onineteenth.prg nineteenth.o

# Now 'prelude'.
cd ${SRC_DIR}/prelude
if ! acme -l labels.txt prelude.asm
then
    echo "Build aborted!"
    exit
fi
exomizer level -oprelude.prg prelude.o

# Then 'play'.
cd ${SRC_DIR}/play
if ! acme -l labels.txt play.asm
then
    echo "Build aborted!"
    exit
fi
# Make a copy of labels file with .asm prefix which can be used by
# score_table.asm.
cp labels.txt play_labels.asm

exomizer level -oplay.prg play.o

# Score tables (loaded in separately at end of each hole).
# Stroke play:
if ! acme -l scoretbl_sp_labels.txt score_table_sp.asm
then
    echo "Build aborted! (score_table_sp.asm)"
    exit
fi
exomizer level -osctblsp.prg sctblsp.o
# Match play:
if ! acme -l scoretbl_mp_labels.txt score_table_mp.asm
then
    echo "Build aborted! (score_table_mp.asm)"
    exit
fi
exomizer level -osctblmp.prg sctblmp.o

cd ${SRC_DIR}
c1541 mydisk.d64 -delete *.prg -write core/birdie.prg -write splash/splash.prg -write prelude/prelude.prg -write play/play.prg -write play/sctblsp.prg -write play/sctblmp.prg -write nineteenth/nineteenth.prg -write finale/finale.prg

# We will look for every possible hole file but, obviously, process only
# those files which actually exist!
NUM_COURSES=6
NUM_HOLES=18
i=0 # count courses
j=0 # count holes
while [ $i -lt ${NUM_COURSES} ]; do
    ((j=0))
    while [ $j -lt ${NUM_HOLES} ]; do
        cd ${ASSETS_DIR}/holes
        infile=$(printf 'h%02d%02d.bin' ${i} $((j+1)))
        if [ -e ${infile} ]
        then
            outfile=$(printf 'h%02d%02d.prg' ${i} ${j})
            exomizer level -o${outfile} ${infile}
            cd ${SRC_DIR}
            c1541 mydisk.d64 -write ${ASSETS_DIR}/holes/${outfile}
        fi
        ((j=j+1))
    done
    ((i=i+1))
done

#cd ${ASSETS_DIR}/holes
#exomizer level -otryhole.prg tryhole.bin
#cd ${SRC_DIR}
#c1541 mydisk.d64 -write ${ASSETS_DIR}/holes/tryhole.prg
    

# Backdrop data.
courses=("beechings" "sierre" "alab" "hokusai" "carver" "newyork")
count=0
for item in ${courses[@]}
do
    echo "$count: ${item}"

    coursedir="${BACKDROPS_DIR}/${item}"
    # Look for required file in this directory.  If it exists, we need to 
    # process backdrops for this course.
    if [ -e "${coursedir}/bdrop.png" ]
    then
        echo "Must process backdrop for ${item}!!!"
        cd ${TOOLS_DIR}/backdrop_builder_2020
        pike pattern_builder.pike "${count}" "${BASE_DIR}"

        # Data will be loaded directly into the module. First isolate the
        # relevant line in the labels file to pass to Pike program.
        bdrop_data_dest=`grep bdrop_v_num_distant_objects ${SRC_DIR}/play/labels.txt`
        pike prepare_backdrop_files.pike "${count}" "${bdrop_data_dest}" "${BACKDROPS_DIR}"

        # Crunch 'backdrop.bin' file and write to disk.
        # In same directory, crunch 'sxx.bin' and write to disk.
        bfile_crunched=$(printf 'b%02d.prg' ${count})
        sfile=$(printf 's%02d.bin' ${count})
        sfile_crunched=$(printf 's%02d.prg' ${count})
        cd ${coursedir}
        exomizer level -o${bfile_crunched} backdrop.bin
        exomizer level -o${sfile_crunched} ${sfile}
        # Writing to disk...
        cd ${SRC_DIR}
        c1541 mydisk.d64 -write ${coursedir}/${bfile_crunched} \
            -write ${coursedir}/${sfile_crunched}
    fi
    ((count += 1))
done

# Tree data.
count=0
for item in ${courses[@]}
do
    echo "$count: ${item}"

    coursedir="${TREES_DIR}/${item}"
    # Look for required file in this directory.  If it exists, we need to 
    # process trees for this course.
    if [ -e "${coursedir}/tiles.png" ]
    then
        echo "Must process trees for ${item}!!!"
        cd ${TOOLS_DIR}/tree_builder_2020
        if ! ./build_trees.sh ${count} ${TREES_DIR}
        then
            echo "Cannot build trees!"
            exit 1
        fi
        # Now we have a 'txx.bin' file in ${coursedir}...
        # Need to know the destination address for our data file.
        tree_data_dest=`grep trees_v_heights ${SRC_DIR}/play/labels.txt`
        echo ${tree_data_dest}
        if ! pike prepare_tree_data_file.pike "${count}" "${tree_data_dest}" "${TREES_DIR}"
        then
            echo "Cannot prepare tree data file!"
            exit 1
        fi
        # Crunch and write to disk.
        tfile_crunched=$(printf 't%02d.prg' ${count})
        cd ${coursedir}
        exomizer level -o${tfile_crunched} trees.bin
        cd ${SRC_DIR}
        c1541 mydisk.d64 -write ${coursedir}/${tfile_crunched}
    fi
    ((count += 1))
done

# Where will the 'best rounds' file be loaded to?
best_rounds_dest=`grep finale_v_best_rounds_data ${SRC_DIR}/finale/labels.txt`
cd ${TOOLS_DIR}/best_rounds_tables
pike best_rounds_gen.pike "${best_rounds_dest}"
# Write these tables to disk.  No need to crunch because only 35 bytes!!!
((i=0))
while [ $i -lt ${NUM_COURSES} ]; do
    cd ${TOOLS_DIR}/best_rounds_tables
    mytable=$(printf 'best%02d.prg' ${i})
#    mytable=$(printf 'best%02d.bin' ${i})
    if [ -e ${mytable} ]
    then
        #best_crunched=$(printf 'best%02d.prg' ${i})
        #exomizer level -o${best_crunched} ${mytable}
        cd ${SRC_DIR}
        #c1541 mydisk.d64 -write ${TOOLS_DIR}/best_rounds_tables/${best_crunched}
        c1541 mydisk.d64 -write ${TOOLS_DIR}/best_rounds_tables/${mytable}
    fi
    ((i += 1))
done



cd ${SRC_DIR}/play
label_sorter labels.txt ${SRC_DIR}/sorted_labels.txt
mem_left labels.txt


