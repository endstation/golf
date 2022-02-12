#!/bin/bash

CORE_DIR='/home/matthew/hp2_progs/programming/6510assembly/golfgit/commodore-golf/src/core'

function toupper {
    echo $1 | tr 'a-z' 'A-Z'
} # toupper

cp -f ${CORE_DIR}/vic_ii_template.asm ${CORE_DIR}/vic_ii.asm

# Value (decimal) is piped in.
# This is the zeropage address of the first variable set aside for use by
# the loader code.  Copy this into the config file before building.
#myinput=$(< /dev/stdin)
linematch=`grep ZP_LOADER_01 ${CORE_DIR}/zeropage.asm`
#myinput=`./get_zp_value.pl "${linematch}"`
myinput=`get_c64_label_value "${linematch}"`

myzpbase=$(printf "%02x" ${myinput})
myzpbase2=$(printf "%02x" $((myinput + 8)))
echo "myzpbase=${myzpbase}"
echo "myzpbase2=${myzpbase2}"

# Replace lines in config 'template' with correct zeropage
# values.
sed "-e s/\r//" "-e 37c zpbase = \$${myzpbase}" "-e 41c zpbase2 = \$${myzpbase2}" \
    cfg_exom_template.s >cfg_exom.s

# Build loader binary.
dasm loader.s -omyloader.bin -smysymbols.txt

# Find addresses of three required subroutines and write them into the
# file 'src/core/vic_ii.asm'.
subnames=('initloader' 'loadfile' 'loadfile_exomizer')
for item in ${subnames[@]}
do
    myline=`grep "${item}[[:space:]]\+" mysymbols.txt`
    echo "myline=${myline}"
    # Split this string into two parts with parenthesis notation (?!).
    parts=(${myline})
    assignment=$(printf "CB_%s = \$%s" `toupper ${parts[0]}` ${parts[1]})
    echo ${assignment} >> ${CORE_DIR}/vic_ii.asm 
done

# This binary ('myloader.bin') now needs to be split up into two separate
# files.
pike process_loader.pike



