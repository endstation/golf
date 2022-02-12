#/bin/bash

cd testdir
if [ -e h0101.bin ]
then
    echo 'found it'
else
    echo 'did not find it'
fi

echo ${PWD}


