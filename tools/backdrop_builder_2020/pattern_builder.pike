// Copyright 2020 Matthew Clarke

constant ON_PATTERN = 0b01;
constant END_OF_ROW_BYTE = 0xfe;
constant END_OF_PATTERN_BYTE = 0xfd;
// NOTE: this is actually just a dummy address. Data will be loaded directly
// into the 'backdrop' module.
constant DEST_ADDR = 0xe000;
// 'Backdrop' and then five 'distant objects'.
constant MAX_OBJECTS = 6;
constant MAX_DISTANT_OBJECTS = MAX_OBJECTS - 1;
constant PATTERN_BLOCK_SIZE = 288;
constant MAX_BACKDROP_COLUMNS = (40 / 8) + 1;
constant MAX_TILES = 48;
array(string) COURSES = ({
    "beechings", "sierre", "alab", "hokusai", "carver", "newyork" });

array(int) patterns = ({});
array(int) masks = ({});
int course_index;
object my_tile_set = ADT.Set();
array(int) my_tiles_ordered;
// This array (of arrays) will hold the tiles in their 64-bit integer form.
array(array(int)) uncompressed_patterns = ({});
array(array(int)) compressed_patterns = allocate(MAX_OBJECTS);
// Need to collect this info as we extract tiles.
array(int) dobj_widths = ({});
array(int) dobj_heights = ({});
array(int) start_rows = allocate(MAX_OBJECTS, 0);
// Backdrop pattern must be drawn repeatedly until it fills the width of 
// the screen.  Record columns it should be drawn at here.  Use (-1) to 
// indicate end of list.
array(int) backdrop_columns = allocate(MAX_BACKDROP_COLUMNS, 0xff);
array(int) tile_data_as_bytes;
array(int) sprite_config_bytes;
array(array(int)) all_angles = ({});
string backdrops_dir;


int main(int argc, array(string) argv)
{
    if (argc != 3)
    {
        werror("Usage: pike pattern_builder.pike <course index> <base dir>\n");
        exit(1);
    } // if

    course_index = (int) argv[1];
    backdrops_dir = sprintf("%s/assets/backdrops", argv[2]);
    string mydir = sprintf("%s/%s", backdrops_dir, COURSES[course_index]);

    // Get a list of all PNG files from this directory (sorted!).
    array png_files = sort(filter(get_dir(mydir),
        lambda(string s) { return has_suffix(s, "png"); }));

    // Extract set of tiles from these images.
    foreach (png_files, string s)
    {
        string path = sprintf("%s/%s", mydir, s);
        extract_tiles(path);
    } // foreach

    int n = sizeof(my_tile_set);
    write("How many tiles? %d [%d bytes]\n", n, n*8);
    my_tiles_ordered = indices(my_tile_set);
    //write("%O\n", my_tiles_ordered);

    // Now build the compressed patterns that will be loaded into the game.
    // First, make sure that each element of compressed_patterns is 
    // initialized (since they won't necessarily all be used).
    for (int i = 0; i < sizeof(compressed_patterns); ++i)
    {
        compressed_patterns[i] = ({});
    } // for

    for (int i = 0; i < sizeof(uncompressed_patterns); ++i)
    {
        build_pattern(i);
    } // for
    //write("%O\n", compressed_patterns);

    init_start_rows();
    //write("%O\n", start_rows);
    init_backdrop_columns();
    //write("%O\n", backdrop_columns);

    create_offsets_file(mydir);

    string angles_path = sprintf("%s/angles.txt", mydir);
    load_angles(angles_path);

    output_tile_data_as_bytes();
    process_sprite_config(mydir);
    write_output_binary(mydir);

    return 0;

} // main()

// **************************************************

void extract_tiles(string path)
{
    string data = Image.load_file(path);
    object myimg = Image.PNG.decode(data);
    //write("%d*%d\n", myimg->xsize(), myimg->ysize());
    int num_rows = myimg->ysize() / 8;
    int num_cols = myimg->xsize() / 8;
    dobj_widths += ({ num_cols });
    dobj_heights += ({ num_rows });

    // Record which tiles this object is made up of in this array.  It will
    // hold the tiles in their 64-bit int form.  Then add this array to the
    // global 'uncompressed_patterns' array. 
    array(int) object_tiles = ({});

    for (int i = 0; i < num_rows; ++i)
    {
        for (int j = 0; j < num_cols; ++j)
        {
            array bytes = ({});
            for (int k = 0; k < 8; ++k)
            {
                int x = j * 8;
                int y = i * 8 + k;
                bytes += ({ get_byte(myimg, x, y) });
            } // for k

            int tile64 = add_tile(bytes);
            object_tiles += ({ tile64 });
        } // for j
    } // for i
    
    uncompressed_patterns += ({ object_tiles });

} // extract_tiles()

// **************************************************

int get_byte(object img, int x, int y)
{
    int byte = 0;

    // We are going to look at eight pixels (in pairs) across the x-axis.
    for (int i = 0; i < 4; ++i)
    {
        array rgb = img->getpixel(x+(2*i), y);
        if ( !rgb[0] )  // If black (= on)...
        {
            int shift = 6 - (2 * i);
            byte += (ON_PATTERN << shift);
        } // if
    } // for

    return byte;

} // get_byte()

// **************************************************

int add_tile(array bytes)
{
    int total = 0;
    
    for (int i = 0; i < 8; ++i)
    {
        total += bytes[i] * pow(2, 8*i);
    } // for
    my_tile_set->add(total);

    return total;

} // add_tile()

// **************************************************

void build_pattern(int i)
{
    int my_w = dobj_widths[i];
    int my_h = dobj_heights[i];

    // NOTE: width of pattern (in chars) is the first element of the pattern
    // data.
    array(int) all_data = ({ my_w });

    for (int j = 0; j < my_h; ++j)
    {
        int zero_count = 0;
        for (int k = 0; k < my_w; ++k)
        {
            int tile_i = lookup(uncompressed_patterns[i][j*my_w + k]);
            if ( !tile_i )
            {
                ++zero_count;
            }
            else
            {
                // A non-zero (i.e. non-blank) tile.  Add however many
                // blanks are recorded in 'zero_count' before this one
                // (and reset 'zero_count' to 0.
                while (zero_count)
                {
                    all_data += ({ 0 });
                    --zero_count;
                } // while
                all_data += ({ tile_i });
            } // if ... else
        } // for
        all_data += ({ END_OF_ROW_BYTE });
    } // for
    all_data += ({ END_OF_PATTERN_BYTE });

    compressed_patterns[i] = all_data;

} // build_pattern()

// **************************************************

// Return index of given tile value.
int lookup(int tile_bytes)
{
    int i = search(my_tiles_ordered, tile_bytes);
    if (i < 0)
    {
        werror("Cannot find this tile: %d\n", tile_bytes);
        exit(1);
    } // if

    return i;

} // lookup()

// **************************************************

void init_start_rows()
{
    for (int i = 0; i < sizeof(dobj_heights); ++i)
    {
        start_rows[i] = 16 - dobj_heights[i];
    } // for

} // init_start_rows()

// **************************************************

void init_backdrop_columns()
{
    int width = dobj_widths[0];
    int i = 0;

    while ( (i*width) < 39 )
    {
        backdrop_columns[i] = i * width;
        ++i;
    } // while

} // init_backdrop_columns()

// **************************************************

void write_output_binary(string mydir)
{
    string path = sprintf("%s/backdrop.bin", mydir);
    object fout = Stdio.File(path, "wct");

    // (dummy) destination address.
    fout->write("%s", (string) ({ DEST_ADDR&0xff, (DEST_ADDR>>8)&0xff }));
    // Number of distant objects.
    fout->write("%s", (string) ({ sizeof(uncompressed_patterns)-1 }));
    // Columns for 'backdrop'.
    fout->write("%s", (string) backdrop_columns);
    // Start rows.
    fout->write("%s", (string) start_rows);
    // Tiles.
    fout->write("%s", (string) tile_data_as_bytes);

    // Dummy bytes that will hold the addresses of the patterns (low and high
    // bytes):
    for (int i = 0; i < (MAX_OBJECTS*2); ++i)
    {
        fout->write("%s", (string) ({ 0 }));
    } // for

    // Patterns themselves.
    // We must fill up the maximum number of bytes allowed.
    array(int) pattern_bytes = allocate(PATTERN_BLOCK_SIZE, 0);
    array everything = Array.flatten(compressed_patterns);
    write("sizeof everything=%d\n", sizeof(everything));
    for (int i = 0; i < sizeof(everything); ++i)
    {
        pattern_bytes[i] = everything[i];
    } // for

    fout->write("%s", (string) pattern_bytes);
    fout->write("%s", (string) Array.flatten(all_angles));

    // Sprite config.
    fout->write("%s", (string) sprite_config_bytes);

    fout->close();

} // write_output_binary()

// **************************************************

void output_tile_data_as_bytes()
{
    tile_data_as_bytes = allocate(MAX_TILES * 8, 0);
    for (int i = 0; i < sizeof(my_tiles_ordered); ++i)
    {
        for (int j = 0; j < 8; ++j)
        {
            int dest = (i * 8) + j;
            tile_data_as_bytes[dest] =
                (my_tiles_ordered[i] >> (j * 8)) & 0xff;
        } // for
    } // for

} // output_tile_data_as_bytes()

// **************************************************

void create_offsets_file(string mydir)
{
    array(int) offsets = allocate(MAX_OBJECTS, 0);
    int current_offset = 0;

    for (int i = 0; i < sizeof(compressed_patterns); ++i)
    {
        offsets[i] = current_offset;
        current_offset += sizeof(compressed_patterns[i]);
    } // for

    //write("OFFSETS:\n");
    //write("%O\n", offsets);

    string path = sprintf("%s/pattern_offsets.txt", mydir);
    object fout = Stdio.File(path, "wct");
    string s = map(offsets, lambda(int x) { return sprintf("%d", x); }) * ",";
    fout->write("%s", s);
    fout->close();

} // create_offsets_file()

// **************************************************

void process_sprite_config(string mydir)
{
    enum { Y_POS, X_OFFSET, PTR_FROM, PTR_TO, FRAME_RATE, /*SW_NUM, HW_NUM,*/
            HIRES, COLOR, ANIM_TYPE };
    enum { ANIM_LOOP, ANIM_PINGPONG };

    sprite_config_bytes = ({});

    string path = sprintf("%s/sprite_config.txt", mydir);
    object fin = Stdio.File(path, "r");
    object iter = fin->line_iterator();
    int section = 0;

    do
    {
        string line = String.trim_all_whites(iter->value());
        if ( !sizeof(line) || (search(line, "#") >= 0) ) { continue; }
        //write("%s\n", line);
        array int_vals = map( line/",",
                lambda(string s) { return (int) s; } );
        //write("section=%d\n", section);
        //write("%O\n", int_vals);
        if (section == X_OFFSET)
        {
            array a = allocate(MAX_DISTANT_OBJECTS * 2, 0);
            for (int i = 0; i < sizeof(int_vals); ++i)
            {
                if (int_vals[i] >= 0)
                {
                    // Positive!  High byte will remain 0.
                    a[i] = int_vals[i];
                }
                else
                {
                    // Negative!  Two's complement & extend sign bit into 
                    // high byte.
                    a[i] = 256 + int_vals[i];
                    a[i+MAX_DISTANT_OBJECTS] = 0xff;
                } // if ... else
            } // for
            sprite_config_bytes += a;
        }
        else
        {
            sprite_config_bytes += int_vals;
        } // if ... else
        ++section;
    } while (iter->next());
    fin->close();

    //write("all_bytes:\n");
    for (int i = 0; i < sizeof(sprite_config_bytes); ++i)
    {
        if ( !(i % MAX_DISTANT_OBJECTS) )
        { 
            //write("\n");
        } // if
        //write("%d\t", sprite_config_bytes[i]);
    } // for

} // process_sprite_config()

// **************************************************

void load_angles(string path)
{
    all_angles = allocate(2);
    object fin = Stdio.File(path, "r");
    object iter = fin->line_iterator();
    string myline;
    int done = 0;
    int section = 0;
    do
    {
        myline = String.trim_all_whites(iter->value());
        if (!sizeof(myline) || search(myline, "#") == 0) continue;
        all_angles[section] =
            map( myline / ",", lambda(string s) { return (int) s; });
        //done = 1;
        ++section;
    } while (/*!done &&*/ iter->next());
    fin->close();

    write("ANGLES:\n%O\n", all_angles);

} // load_angles()

// **************************************************
// **************************************************
// **************************************************
// **************************************************
// **************************************************
// **************************************************
// **************************************************
// **************************************************
// **************************************************

