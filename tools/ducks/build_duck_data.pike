enum { BLACK,WHITE,RED,CYAN,VIOLET,GREEN,BLUE,YELLOW,ORANGE,BROWN,LIGHT_RED,
    GREY1,GREY2,LIGHT_GREEN,LIGHT_BLUE,GREY3 };
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
constant SPRITES_DIR = "../../assets/sprites";
mapping(int:int) COLOR_CODES = ([ ORANGE:0, YELLOW:1, WHITE:2, LIGHT_BLUE:3 ]);
array(array(int)) TO_COMPARE = ({
    ({ 0,1 }),
    ({ 1,2 }),
    ({ 2,3 }),
    ({ 3,4 }),
    ({ 4,5 }),
    ({ 5,4 }),
    ({ 4,3 }),
    ({ 3,2 }),
    ({ 2,1 })
});
array(array(int)) all_bytes;




int main()
{
    string path = sprintf("%s/ducks2.png", SPRITES_DIR);
    object img = Image.PNG.decode( Image.load_file(path) );
    write("size=%d*%d\n", img->xsize(), img->ysize());

    int num_frames = img->ysize() / 8;
    write("num_frames=%d\n", num_frames);
    all_bytes = allocate(num_frames);

    for (int i = 0; i < num_frames; ++i)
    {
        process_frame(i, img);
    } // for

    display();
    write_output_file();

    /*
    foreach (TO_COMPARE, array pair)
    {
        write("\t%d -> %d\n", pair[0], pair[1]);
        find_diffs(pair[0], pair[1]);
    } // foreach
    */

    return 0;

} // main()

// **************************************************

int lookup_color(array(int) rgb)
{
    for (int i = 0; i < sizeof(RGB); ++i)
    {
        array a = RGB[i];
        if ( equal(a, rgb) )
        {
            return i;
        } // if
    } // for

    werror("Invalid color! %O\n", rgb);
    exit(1);

} // lookup_color()

// **************************************************

void process_frame(int i, object img)
{
    array my_bytes = ({});
    int first_row = i * 8;
    int one_past_end_row = first_row + 8;

    for (int c = 0; c < 2; ++c)
    {
        for (int r = first_row; r < one_past_end_row; ++r)
        {
            int byte = get_byte(c*8, r, img);
            my_bytes += ({ byte });
        } // for
    } // for

    all_bytes[i] = my_bytes;

} // process_frame()

// **************************************************

int get_byte(int col, int row, object img)
{
    int byte = 0;

    for (int i = 0; i < 4; ++i)
    {
        array rgb = img->getpixel( col+(i*2), row );
        int color = lookup_color(rgb);
        byte += ( COLOR_CODES[color]<<(6-(i*2)) );
    } // for

    return byte;

} // get_byte()

// **************************************************

void display()
{
    foreach (all_bytes, array a)
    {
        foreach (a, int x) { write("%02x [%08b]\n", x, x); }
        write("\n\n");
    } // foreach

} // display()

// **************************************************

void find_diffs(int from, int to)
{
    array a = all_bytes[from];
    array b = all_bytes[to];

    for (int i = 0; i < sizeof(a); ++i)
    {
        if (a[i] != b[i])
        {
            write("%d: %d\n", i, b[i]);
        } // if
    } // for

} // find_diffs()

// **************************************************

void write_output_file()
{
    string path = sprintf("%s/duck_frames.bin", SPRITES_DIR);
    //string path = "duck_frames.bin";
    object fout = Stdio.File(path, "wct");
    fout->write("%s", (string) Array.flatten(all_bytes));
    fout->close();

} // write_output_file()

// **************************************************
// **************************************************
// **************************************************
// **************************************************
// **************************************************
// **************************************************
// **************************************************
// **************************************************
// **************************************************
// **************************************************
// **************************************************
// **************************************************
// **************************************************
// **************************************************
// **************************************************
