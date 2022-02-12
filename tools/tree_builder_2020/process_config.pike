// Top-hole Golf
// Copyright 2020 Matthew Clarke

array(string) COURSES = ({
    "beechings", "sierre", "alab", "hokusai", "carver", "newyork" });
constant MAX_PATTERNS = 10;
//constant MAX_TILE_CLASSES = 24;

array(array(int)) all_data = ({});
array(int) all_bytes;
string trees_dir;


int main(int argc, array(string) argv)
{
    if (argc != 3)
    {
        werror("Usage: pike process_config.pike <course index> <trees dir>\n");
        exit(1);
    } // if

    string course_str = COURSES[ (int) argv[1] ];
    trees_dir = argv[2];
    string path = sprintf("%s/%s/config.txt", trees_dir, course_str);
    extract_data_from_file(path);
    process_data();
    
    path = sprintf("%s/%s/config.bin", trees_dir, course_str);
    write_file(path);

    //write("all_data=\n%O\n", all_data);

    return 0;

} // main()

void extract_data_from_file(string path)
{
    object fin = Stdio.File(path, "r");
    object iter = fin->line_iterator();
    string line;

    do
    {
        line = String.trim_all_whites( iter->value() );
        if ((!sizeof(line)) || (search(line, "#") == 0)) continue;
        write("%s\n", line);
        array a = map( (line / ","), lambda(string s) { return (int) s; } );
        // NOTE: we're adding a separate array of data for each section.
        all_data += ({ a });
    } while (iter->next());

    fin->close();

} // extract_data_from_file()

void process_data()
{
    // Two bytes at top of file to hold address (eventually!).
    all_bytes = ({ 0, 0 });

    // Data appears in this order: 
    // Foliage heights, trunk heights, trunk widths, trunk adjustments,
    // foliage offsets, selection masks, base offsets.
    enum { FOLIAGE_HEIGHTS, TRUNK_HEIGHTS, TRUNK_WIDTHS, TRUNK_ADJUSTS,
        FOLIAGE_OFFSETS, MODULATION_COLORS, MODULATION_THRESHOLD,
        COLLISION_CUT_OFF }; 
     
    // 1: tree heights.
    for (int i = 0; i < MAX_PATTERNS; ++i)
    {
        all_bytes += ({ all_data[FOLIAGE_HEIGHTS][i] + all_data[TRUNK_HEIGHTS][i] });
    } // for

    // 2: trunk heights.
    foreach (all_data[TRUNK_HEIGHTS], int x) { all_bytes += ({ x }); }
    // 3: trunk widths.
    foreach (all_data[TRUNK_WIDTHS], int x) { all_bytes += ({ x }); }
    // 4: trunk adjusts
    foreach (all_data[TRUNK_ADJUSTS], int x) { all_bytes += ({ x }); }
    // 5: foliage offsets
    foreach (all_data[FOLIAGE_OFFSETS], int x) { all_bytes += ({ x }); }

    // 8: pattern addresses - low and high bytes.
    // Don't have this information yet so just pad out with MAX_PATTERNS*2 bytes.
    for (int i = 0; i < MAX_PATTERNS*2; ++i) { all_bytes += ({ 0 }); }

    // 9: modulation colors.
    foreach (all_data[MODULATION_COLORS], int x) { all_bytes += ({ x<<4 }); }
    // 10: modulation threshold.
    all_bytes += ({ all_data[MODULATION_THRESHOLD][0] });
    all_bytes += ({ all_data[COLLISION_CUT_OFF][0] });

} // process_data()

void write_file(string path)
{
    object fout = Stdio.File(path, "wct");
    fout->write("%s", (string) all_bytes);
    fout->close();

} // write_file()

