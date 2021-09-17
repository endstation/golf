// Top-hole Golf
// Copyright 2020,2021 Matthew Clarke

array(string) COURSES = ({
    "beechings", "sierre", "alab", "hokusai", "carver", "newyork" });
constant NUM_PATTERNS = 10;
constant PATTERN_WIDTH = 9;
constant PATTERN_HEIGHT = 11;
array(int) byte_array = ({});
constant END_OF_ROW = 0xfe;
constant BLANK_TILE = 0xff;
constant END_OF_PATTERN = 0xfd;
constant PATTERN_BLOCK_SIZE = 512;
array(int) all_bytes;
array(array(int)) all_patterns = ({});
string trees_dir;


int main(int argc, array(string) argv)
{
    if (argc != 3)
    {
        werror("Usage: pike patterns.pike <course index> <trees dir>\n");
        exit(1);
    } // if
    
    string course_str = COURSES[ (int) argv[1] ];
    trees_dir = argv[2];
    string path = sprintf("%s/%s/patterns.tmx", trees_dir, course_str);
    write("path=%s\n", path);
    object mymap = TMX_parser.TMX_parser(path)->get_map();
    int w = mymap->width;
    write("width=%d\n", w);
    int num_trees = w / PATTERN_WIDTH;
    object ldata = TMX_parser.Layer_data(mymap->layers[0]->data, w);
    for (int i = 0; i < num_trees; ++i)
    {
        process_tree(i, ldata);
    } // for
    
    //write("all_patterns=\n%O\n", all_patterns);
    string output_path = sprintf("%s/%s/patterns.bin", trees_dir, course_str);
    write_binary(output_path);
    output_path = sprintf("%s/%s/pattern_offsets.txt", trees_dir, course_str);
    write_offsets(output_path);

    return 0;

} // main()

// **************************************************

void process_tree(int i, object ldata)
{
    all_bytes = ({});
    int max_w = 0;

    write("\n*** Tree #%d ***\n", i);
    int base_x = i * PATTERN_WIDTH;
    for (int r = 0; r < (PATTERN_HEIGHT-i); ++r)
    {
        int tiles_found = 0;
        int tile_count = 0;
        for (int c = base_x; c < (base_x+PATTERN_WIDTH); ++c)
        {
            int id = ldata->read(c, r);
            if (id) { tiles_found = 1; }
            if (!id && tiles_found) {
                //write("%02x\n", END_OF_ROW);
                //all_bytes += ({ END_OF_ROW });
                break;
            }
            else if (!id && !tiles_found) {
                write("%02x ", BLANK_TILE);
                all_bytes += ({ BLANK_TILE });
                ++tile_count;
            }
            else {
                write("%02x ", (id-1));
                all_bytes += ({ id-1 });
                ++tile_count;
            } // if ... else
        } // for

        write("%02x\n", END_OF_ROW);
        all_bytes += ({ END_OF_ROW });

        max_w = max(max_w, tile_count);
    } // for
    write("%02x\n", END_OF_PATTERN);
    all_bytes += ({ END_OF_PATTERN });
    all_bytes = ({ max_w }) + all_bytes;
    //write_file(i);
    all_patterns += ({ all_bytes });

    write("[width] %02d: %d\n", i, max_w);

} // process_tree()

// **************************************************

void write_binary(string path)
{
    array arr = allocate(PATTERN_BLOCK_SIZE, 0);
    array src = Array.flatten(all_patterns);
    for (int i = 0; i < sizeof(src); ++i)
    {
        arr[i] = src[i];
    } // for

    object fout = Stdio.File(path, "wct");
    fout->write("%s", (string) arr);
    fout->close();

} // write_binary()

// **************************************************

void write_offsets(string path)
{
    array(int) offsets = allocate(NUM_PATTERNS, 0);
    int current_offset = 0;

    for (int i = 0; i < sizeof(all_patterns); ++i)
    {
        offsets[i] = current_offset;
        current_offset += sizeof(all_patterns[i]);
    } // for

    object fout = Stdio.File(path, "wct");
    string s = map(offsets, lambda(int x) { return sprintf("%d", x); }) * ",";
    fout->write("%s", s);
    fout->close();

} // write_offsets()


