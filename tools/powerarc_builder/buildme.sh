#!/bin/bash

if ! pike icon_generator.pike
then
    echo "Cannot generate icon!"
    exit 1
fi

if ! pike prepare_base.pike
then
    echo "Cannot prepare base!"
    exit 1
fi

if ! pike build_power_steps.pike
then
    echo "Cannot build power steps!"
    exit 1
fi

if ! pike build_precision_steps.pike
then
    echo "Cannot build precision steps!"
    exit 1
fi


