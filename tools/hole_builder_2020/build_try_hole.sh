#!/bin/bash

SRC_DIR="/home/matthew/hp2_progs/programming/6510assembly/golfgit/commodore-golf/src"
HOLES_DIR="/home/matthew/hp2_progs/programming/6510assembly/golfgit/commodore-golf/assets/holes"

cd ${HOLES_DIR}
exomizer level -otryhole.prg tryhole.bin
cd ${SRC_DIR}
c1541 mydisk.d64 -delete tryhole.prg -write ${HOLES_DIR}/tryhole.prg


