// Copyright 2020 Matthew Clarke

constant ON_PATTERN = 0b01;
constant END_OF_ROW_BYTE = 0xfe;
constant END_OF_PATTERN_BYTE = 0xfd;
constant MAX_TILES = 32;
constant MAX_NUM_DISTANT_OBJECTS = 5;
constant MAX_DOBJ_PATTERN_SIZE = 56;
constant OUTPUT_DIR = "../../assets/tiles";
constant BACKDROPS_DIR = "../../assets/backdrops";
constant SPRITE_DEST = 0xcbc0;
array(string) COURSES = ({ "beechings", "sierre", "alab", "hokusai", 
        "carver", "newyork" });

object myset = ADT.Set();
array mytiles = ({});
// For each image, record every tile (as 64-bit integer).
array(array(int)) dobj_tiles = ({});
array dobj_widths = ({});
array dobj_heights = ({});
array(int) tile_data_as_bytes;
array(array(int)) pattern_data = ({});
int course_index;
string sprite_data_file = "";
array(int) sprite_config_data = ({});


int main(int argc, array(string) argv)
{
    course_index = (int) argv[1];
    write("course=%s\n", COURSES[course_index]);
    string mydir = sprintf("%s/%s", BACKDROPS_DIR, COURSES[course_index]);

    // Get a list of all the .png files in this directory.
    object regex = Regexp.PCRE._pcre("dobj_\\d\\d\\.png");
    array png_files = sort(filter(get_dir(mydir),
            //lambda(string s) { return has_suffix(s, "png"); }));
            lambda(string s) { return arrayp(regex->exec(s)); }));
    write("%O\n", png_files);

    // Create a list of all tiles we'll need to draw these objects.
    // At first, multiples will be allowed.
    foreach (png_files, string s)
    {
        string path = sprintf("%s/%s", mydir, s);
        extract_tiles(path);
    } // foreach

    write("size of set: %d\n", sizeof(myset));
    mytiles = indices(myset);
    write("%O\n", mytiles);

    write("dobj_tiles: size=%d\n", sizeof(dobj_tiles));
    foreach (dobj_tiles, array a)
    {
        write("-> %d\n", sizeof(a));
    } // foreach

    // Now we have got a set of tiles, need to build the patterns for each
    // image.
    for (int i = 0; i < sizeof(png_files); ++i)
    {
        build_pattern(i, png_files[i]);
    } // for

    output_tile_data_as_bytes();
    check_for_sprite_file(mydir);
    write_binary_file(mydir);

    return 0;

} // main()

// **************************************************

void extract_tiles(string path)
{
    string data = Image.load_file(path);
    object myimg = Image.PNG.decode(data);
    write("%d*%d\n", myimg->xsize(), myimg->ysize());
    int num_rows = myimg->ysize() / 8;
    int num_cols = myimg->xsize() / 8;
    dobj_widths += ({ num_cols });
    dobj_heights += ({ num_rows });
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

    dobj_tiles += ({ object_tiles });

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

void build_pattern(int i, string filename)
{
    write("%d: %s\n", i, filename);
    write("-> %d*%d\n", dobj_widths[i], dobj_heights[i]);
    int my_w = dobj_widths[i];
    int my_h = dobj_heights[i];

    array(int) all_data = ({ my_w });
    for (int j = 0; j < my_h; ++j)
    {
        int zero_count = 0;
        for (int k = 0; k < my_w; ++k)
        {
            int tile_i = lookup(dobj_tiles[i][j*my_w + k]);
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
                //zero_count = 0;
                all_data += ({ tile_i });
            } // if ... else
        } // for
        all_data += ({ END_OF_ROW_BYTE });
    } // for
    all_data += ({ END_OF_PATTERN_BYTE });

    write("all_data [%d bytes]=%O\n", sizeof(all_data), all_data);
    pattern_data += ({ all_data });

} // build_pattern()

// **************************************************

int add_tile(array bytes)
{
    int total = 0;
    for (int i = 0; i < 8; ++i)
    {
        total += bytes[i] * pow(2, 8*i);
    } // for
    myset->add(total);
    return total;

} // add_tile()

// **************************************************

// Return index of given tile value.
int lookup(int tile_bytes)
{
    int i = search(mytiles, tile_bytes);
    if (i < 0)
    {
        werror("Cannot find this tile: %d\n", tile_bytes);
        exit(1);
    } // if

    return i;

} // lookup()

// **************************************************

void output_tile_data_as_bytes()
{
    tile_data_as_bytes = allocate(MAX_TILES * 8, 0);
    for (int i = 0; i < sizeof(mytiles); ++i)
    {
        for (int j = 0; j < 8; ++j)
        {
            int dest = (i * 8) + j;
            tile_data_as_bytes[dest] = (mytiles[i] >> (j * 8)) & 0xff;
        } // for
    } // for

    for (int i = 0; i < sizeof(tile_data_as_bytes); ++i)
    {
        if ( !(i % 8) ) { write(" ***%d***\n", i/8); }
        write("%08b\n", tile_data_as_bytes[i]);
    } // for

} // output_tile_data_as_bytes()

// **************************************************

void write_binary_file(string destdir)
{
    array(int) all_bytes = ({});

    // Number of distant objects is the size of the 'pattern_data' array.
    all_bytes += ({ sizeof(pattern_data) });

    // Now all the tile data.
    all_bytes += tile_data_as_bytes;

    // And finally the pattern data.
    // Firstly, an array of 'MAX_NUM_DISTANT_OBJECTS' bytes giving the start
    // row for drawing each pattern (16 - height).
    array(int) start_rows = allocate(MAX_NUM_DISTANT_OBJECTS, 0);
    for (int i = 0; i < sizeof(pattern_data); ++i)
    {
        start_rows[i] = 16 - dobj_heights[i];
    } // for
    all_bytes += start_rows;

    // We need to write data for the maximum number of distant objects, each
    // with a size of 'MAX_DOBJ_PATTERN_SIZE' bytes.
    array(int) pattern_bytes =
            allocate(MAX_NUM_DISTANT_OBJECTS*MAX_DOBJ_PATTERN_SIZE, 0);
    for (int i = 0; i < sizeof(pattern_data); ++i)
    {
        int dest = i * MAX_DOBJ_PATTERN_SIZE;
        foreach (pattern_data[i], int byte)
        {
            pattern_bytes[dest] = byte;
            ++dest;
        } // foreach
    } // for

    all_bytes += pattern_bytes;
    all_bytes += sprite_config_data;
    write("how many altogether? %d\n", sizeof(all_bytes));

    string path = sprintf("%s/%s/dobjs.bin", BACKDROPS_DIR,
            COURSES[course_index]);
    object fout = Stdio.File(path, "wct");
    fout->write("%s", (string) all_bytes);
    fout->close();

} // write_binary_file()

// **************************************************

void check_for_sprite_file(string mydir)
{
    string path = sprintf("%s/sprites.bin", mydir);
    string outfile = sprintf("%s/s%02d.bin", mydir, course_index);
    object stat = file_stat(path);
    if (!stat)
    { 
        werror("Cannot find file: %s\n", path);
        exit(1);
    }
    else
    {
        write("Found sprite file!\n");
    } // if ... else

    // Open the file and put destination address in first two bytes.
    // (cf. PRG format)
    string data = Image.load_file(path);
    object fout = Stdio.File(outfile, "wct");
    fout->write("%s", (string) ({ SPRITE_DEST&0xff, (SPRITE_DEST>>8)&0xff }));
    fout->write("%s", data);
    fout->close();

    // Must also process sprite config file.
    process_sprite_config(mydir);

} // check_for_sprite_file()

// **************************************************

void process_sprite_config(string mydir)
{
    enum { Y_POS, X_OFFSET, PTR_FROM, PTR_TO, FRAME_RATE, SW_SPR, HW_SPR,
            HIRES };

    string path = sprintf("%s/sprite_config.txt", mydir);
    object fin = Stdio.File(path, "r");
    object iter = fin->line_iterator();
    int i = 0;
    do
    {
        string line = String.trim_all_whites( iter->value() );
        if ( !sizeof(line) || !search(line, "#") ) continue;

        array(int) arr = map( (line/","),
                lambda(string s) { return (int) s; });
        // For x-offset must specify low and high bytes.  And values may
        // be negative.
        if (i == X_OFFSET)
        {
            array(int) xoffsets = allocate(10, 0);
            for (int i = 0; i < 5; ++i)
            {
                if (arr[i] < 0)
                {
                    xoffsets[i] = arr[i] + 256;
                    xoffsets[i+5] = 0xff;
                }
                else
                {
                    xoffsets[i] = arr[i];
                } // if ... else
            } // for
            sprite_config_data += xoffsets;
        }
        else 
        {
            sprite_config_data += arr;
        } // if ... else
        ++i;

    } while (iter->next());
    fin->close();

    write("how much sprite config data? %d bytes\n", sizeof(sprite_config_data));
    for (int i = 0; i < sizeof(sprite_config_data); ++i)
    {
        if ( !(i % 5) )
        {
            write("\n");
        } // if
        write("%d  ", sprite_config_data[i]);
    } // for

} // process_sprite_config()

// **************************************************
// **************************************************
// **************************************************
// **************************************************


