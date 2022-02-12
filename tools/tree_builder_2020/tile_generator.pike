#!/usr/bin/env pike

array RGB = ({
    ({ 0,0,0 }),
    ({ 255,255,255 }),
    ({ 104,55,43 }),
    ({ 112,164,178 }),
    ({ 111,61,134 }),
    ({ 88,141,67 }),
    ({ 53,40,121 }),
    ({ 184,199,111 }),
    ({ 111,79,37 }),
    ({ 67,57,0 }),
    ({ 154,103,89 }),
    ({ 68,68,68 }),
    ({ 108,108,108 }),
    ({ 154,210,132 }),
    ({ 108,94,181 }),
    ({ 149,149,149 })
});
enum {BLACK,WHITE,RED,CYAN,VIOLET,GREEN,BLUE,YELLOW,ORANGE,BROWN,
    LIGHT_RED,GREY1,GREY2,LIGHT_GREEN,LIGHT_BLUE,GREY3 };
// NOTE: set these before use!!!  Which colors are to be used for codes
// 00 (b/g), 01 (vram upper nybble), 10 (vram lower nybble) and 11 (color RAM)?
// And should masks be generated?
array tile_colors = ({ WHITE,YELLOW,GREEN,BLACK });
array(int) PATTERNS = ({ 0b00, 0b01, 0b10, 0b11 });
array(string) COURSES = ({
    "beechings", "sierre", "alab", "hokusai", "carver", "newyork" });
constant MAX_TILES = 16;


// Output goes into these arrays.
array(int) all_bytes = ({});
array(int) all_masks = ({});
string trees_dir;

int main(int argc, array(string) argv)
{
    // First of all, check that the correct number of command-line arguments
    // have been provided.
    if (argc != 3)
    {
        werror("Usage: pike tile_generator.pike <course index> <trees dir>\n");
        exit(1);
    } // if

    int course_index = (int) argv[1];
    trees_dir = argv[2];

    // If masks are required, there should be three filenames given.
    string bytes_output_path = sprintf("%s/%s/tiles.bin", trees_dir,
            COURSES[course_index]);
    string masks_output_path = sprintf("%s/%s/masks.bin", trees_dir,
            COURSES[course_index]);

    // Input file should be given as first argument.
    string in_path = sprintf("%s/%s/tiles.png", trees_dir, COURSES[course_index]);
    string data = Image.load_file(in_path);
    object img = Image.PNG.decode(data);
    // Write dimensions to console.
    write("w*h = %d*%d\n", img->xsize(), img->ysize());
    // How many chars for rows and columns?
    int char_rows = img->ysize() / 8;
    int char_cols = img->xsize() / 8;

    for (int i = 0; i < char_rows; ++i)
    {
        for (int j = 0; j < char_cols; ++j)
        {
            process_tile(img, i, j);
        } // for
    } // for

    write_output_file(bytes_output_path, masks_output_path);

    return 0;

} // main()

void process_tile(object img, int row, int col)
{
    int start_x = col * 8;
    int start_y = row * 8;

    for (int i = 0; i < 8; ++i)
    {
        process_byte(img, start_x, start_y+i);
    } // for

} // process_tile()

void process_byte(object img, int xpos, int ypos)
{
    int byte = 0;
    int mask = 0;

    for (int i = 0; i < 4; ++i)
    {
        array rgb = img->getpixel(xpos+(i*2), ypos);
        int c = lookup_color(rgb);
        // 'c' should be a member of the 'tile_colors' array.
        int color_code = search(tile_colors, c);
        if (color_code < 0)
        {
            werror("Found color not in given set: %d\n", c);
            exit(1);
        } // if

        byte += (PATTERNS[color_code]<<(6-(i*2)));
        if (c == GREEN)
        {
            mask += (0b11<<(6-(i*2)));
        } // if
    } // for

    write("%08b [%02x] --> { %08b }\n", byte, byte, mask);

    all_bytes += ({ byte });
    all_masks += ({ mask });

} // process_byte()

int lookup_color(array rgb)
{
    for (int i = 0; i < sizeof(RGB); ++i)
    {
        if (equal(rgb, RGB[i]))
        {
            return i;
        } // if
    } // for

    werror("Can't find color: %O\n", rgb);
    exit(1);

} // lookup_color()

void write_output_file(string bytes, string masks)
{
    array tile_bytes = allocate(MAX_TILES*8, 0);
    array mask_bytes = allocate(MAX_TILES*8, 0);
    for (int i = 0; i < sizeof(all_bytes); ++i)
    {
        tile_bytes[i] = all_bytes[i];
        mask_bytes[i] = all_masks[i];
    } // for

    Stdio.File fout = Stdio.File(bytes, "wct");
    fout->write("%s", (string) tile_bytes);
    fout->close();

    fout = Stdio.File(masks, "wct");
    fout->write("%s", (string) mask_bytes);
    fout->close();

} // write_output_file()




