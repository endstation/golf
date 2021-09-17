// Top-hole Golf
// Copyright 2020,2021 Matthew Clarke

array(string) COURSES = ({ "beechings","sierre","alab","hokusai","carver","newyork" });
constant OFFSET_TO_PATTERN_ADDRS = 52;  //100;
constant MAX_PATTERNS = 10;
string trees_dir;

int main(int argc, array(string) argv)
{
    if (argc !=4)
    {
        werror("Usage: pike prepare_tree_data_file.pike <course index> <grep output> <trees dir>\n");
        exit(1);
    } // if

    int course_index = (int) argv[1];
    string grep_result = argv[2];
    trees_dir = argv[3];

    int dest_addr = get_address(grep_result);
    write("dest_addr=%d [%04x]\n", dest_addr, dest_addr);

    // We need the (tree) pattern offsets for this course.
    string path = sprintf("%s/%s/pattern_offsets.txt", trees_dir,
            COURSES[course_index]);
    array(int) pattern_offsets = get_pattern_offsets(path);
    write("pattern_offsets=%O\n", pattern_offsets);

    // And also the bytes from the tree data file itself (so far)...
    // We will use this path again to write the modified file back to disk.
    path = sprintf("%s/%s/trees.bin", trees_dir, COURSES[course_index]);
    string all_bytes = Image.load_file(path);

    // Set destination address for file.
    all_bytes[0] = dest_addr & 0xff;
    all_bytes[1] = (dest_addr >> 8) & 0xff;

    // And now the pattern offsets.
    // Subtract 2 here because 'OFFSET_' refers to PRG file where there are an
    // extra two bytes for the address.
    // NOTE: plus an extra 6 for the modulation colors (4) and threshold (1)
    // and collision cut-off (1)...
    int base = dest_addr + OFFSET_TO_PATTERN_ADDRS - 2 + (2 * MAX_PATTERNS) + 6;
    for (int i = 0; i < sizeof(pattern_offsets); ++i)
    {
        int addr = base + pattern_offsets[i];
        int lo = addr & 0xff;
        int hi = (addr >> 8) & 0xff;
        all_bytes[OFFSET_TO_PATTERN_ADDRS+i] = lo;
        all_bytes[OFFSET_TO_PATTERN_ADDRS+i+MAX_PATTERNS] = hi;
    } // for

    // Finally, write the file back to disk.  This function throws error if
    // write failed.
    Stdio.write_file(path, all_bytes);

    return 0;

} // main()

int get_address(string grep_result)
{
    int i = search(grep_result, "$");
    string s = grep_result[ i+1 .. i+4 ];
    return hexstr2int(s);
    
} // get_address()

int hexstr2int(string s)
{
    mapping HEX_MAP = ([ "0":0,"1":1,"2":2,"3":3,"4":4,"5":5,"6":6,"7":7,"8":8,
        "9":9,"a":10,"b":11,"c":12,"d":13,"e":14,"f":15 ]);
    int val = 0;
    int exponent = 0;

    for (int i = sizeof(s)-1; i >= 0; --i)
    {
        string digit = lower_case( s[i..i] );
        val += HEX_MAP[digit] * pow(2, exponent);
        exponent += 4;
    } // for

    return val;

} // hexstr2int()

array(int) get_pattern_offsets(string path)
{
    string line = Stdio.read_file(path, 0, 1);
    array(int) arr = map( (line / ","), lambda(string s) { return (int) s; } );
    return arr;

} // get_pattern_offsets()

