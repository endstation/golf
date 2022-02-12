// Copyright 2020 Matthew Clarke

// Program will look at all 'backdrop' files in the tiles directory and
// re-write the address (in the first two bytes of the file) so that data
// is written directly into the 'bdrops2' module.

mapping HEX_MAP = ([ "0":0, "1":1, "2":2, "3":3, "4":4, "5":5, "6":6, "7":7,
        "8":8, "9":9, "a":10, "b":11, "c":12, "d":13, "e":14, "f":15 ]);
array(string) COURSES = ({ "beechings", "sierre", "alab", "hokusai", "carver",
        "newyork" });
// NOTE: must add 2 to this when modifying binary output, to take into account
// the load address.
constant OFFSET_TO_PATTERN_ADDRS = 397;
constant OFFSET_TO_PATTERN_DATA = OFFSET_TO_PATTERN_ADDRS + 12;
array(int) all_angles = ({});
string backdrops_dir;

int main(int argc, array(string) argv)
{
    // Single command-line argument is a line from grep output.
    int course_index = (int) argv[1];
    string line = argv[2];
    backdrops_dir = argv[3];

    // We must find the address.
    int i = search(line, "$");
    string addr = line[ i+1 .. i+4 ];
    int hex_addr = str2hex(addr);

    string path = sprintf("%s/%s/backdrop.bin", backdrops_dir,
            COURSES[course_index]);
    string path2 = sprintf("%s/%s/pattern_offsets.txt", backdrops_dir,
            COURSES[course_index]);
    process_file(path, path2, hex_addr);
    
    return 0;

} // main()

int str2hex(string s)
{
    int val = 0;
    int exponent = 0;

    for (int i = (sizeof(s)-1); i >= 0; --i)
    {
        string digit = s[ i .. i ];
        val += HEX_MAP[digit] * pow(2, exponent);
        exponent += 4;
    } // for
    return val;

} // str2hex()

void process_file(string path, string offsets_path, int addr)
{
    string data = Image.load_file(path);
    data[0] = addr & 0xff;
    data[1] = (addr >> 8) & 0xff;

    // What is the address of 'bdrop2_l_patterns_addr_lo'?
    int pattern_data_addr = addr + OFFSET_TO_PATTERN_DATA;
    //write("pattern_data_addr = %d [%04x]\n", pattern_data_addr, pattern_data_addr);
    // Load in the 'pattern_offsets' file.
    object fin = Stdio.File(offsets_path, "r");
    object iter = fin->line_iterator();
    string s = iter->value();
    fin->close();
    array arr = map(s / ",",
        lambda(string s) { return ((int) s) + pattern_data_addr; });
    //write("%O\n", arr);
    // NOTE: add another 2 to base destination address for
    // load address (two bytes).
    int base_dest = OFFSET_TO_PATTERN_ADDRS + 2;
    //write("base_dest=%d [%04x]\n", base_dest, base_dest);

    // NOTE: treat the first address (of the repeating pattern) separately.
    // Low and high bytes.
    data[ base_dest ] = arr[0] & 0xff;
    data[ base_dest+1 ] = (arr[0] >> 8) & 0xff;
    // Advance two bytes.
    base_dest += 2;
    // Shift off the address we just dealt with.
    arr = (Array.shift(arr))[1];
    for (int i = 0; i < sizeof(arr); ++i)
    {
        // Low byte.
        data[ base_dest+i ] = arr[i] & 0xff;
        // High byte.
        data[ base_dest+i+5 ] = (arr[i] >> 8) & 0xff;
    } // for

    object fout = Stdio.File(path, "wct");
    fout->write("%s", data);
    fout->close();

} // process_file




