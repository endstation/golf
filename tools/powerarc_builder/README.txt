INPUTS:
* base icon 6*6 chars, saved as a PNG file.
* 'powarc_steps.png' - rendered in shades of purple (R=255,G=0,B=255; decreasing
    in steps of 5).

prepare_base.pike
This opens the 'base.png' icon and iterates over the chars: first rows then columns.
For each char, records the 8 bytes that will be written to the bitmap and outputs
them to the file, 'base_patterns.txt'.  This is a plain text file, with each line
consisting of one integer.

