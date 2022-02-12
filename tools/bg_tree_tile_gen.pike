#!/usr/bin/env pike

constant RGB_ON = 136;
array(int) results = ({});
array(int) masks = ({});

constant ON_PATTERN = 0b01;
constant DEFAULT_BYTE = 0;
array BIT_PAIRS = ({ 0b11000000, 0b00110000, 0b00001100, 0b00000011 });


int main(int argc, array(string) argv)
{
    if (argc != 4)
    {
        werror("Usage: pike tile_gen.pike <infile> <outfile1> <outfile2>\n");
        exit(1);
    } // if

    string infile = argv[1];
    string outfile1 = argv[2];
    string outfile2 = argv[3];

    string data = Image.load_file(infile);
    object myimg = Image.PNG.decode(data);
    write("%d*%d\n", myimg->xsize(), myimg->ysize());

    // NOTE: tiles are sized 4*3 chars.
    int num_tiles = myimg->xsize() / 16;
    write("num_tiles=%d\n", num_tiles);

    for (int i = 0; i < num_tiles; ++i) {
        process_tile(i, myimg);
    } // for

    write("how many? %d\n", sizeof(results));

    // Write output file.
    Stdio.File fout = Stdio.File(outfile1, "wct");
    fout->write("%s", (string) results);
    fout->close();

    fout = Stdio.File(outfile2, "wct");
    fout->write("%s", (string) masks);
    fout->close();

    //dump_results();

    return 0;

} // main()

void process_tile(int i, object myimg)
{
    // There will be 4*3*8=96 bytes.
    // One char at a time...
    int start_x = 16 * i;

    for (int char_row = 0; char_row < 2; ++char_row)
    {
        for (int char_col = 0; char_col < 2; ++char_col)
        {
            process_char(char_row, char_col, start_x, myimg);
        } // for
    } // for

    /*
    // One row at a time.
    for (int r = 0; r < 8; ++r)
    {
        int byte = DEFAULT_BYTE;
        for (int c = i*8; c < (i*8)+8; c += 2)
        {
            array rgb = myimg->getpixel(c, r);
            int on = Array.all(rgb, lambda(int e) { return e==RGB_ON; });
            if (!on) 
            {
                int x = c % 8;
                byte |= OFF_PATTERN<<(6-x);
            //if ( Array.all(rgb, lambda(int e) { return e==RGB_ON; }) )
            //{
            //    int x = c % 8;
            //    byte += PATTERN<<(6-x);
            } // if
        } // for
        results += ({ byte });
    } // for
    */

} // process_char()

void dump_results()
{
    for (int i = 0; i < sizeof(results)/8; ++i)
    {
        write("*** %d ***\n", i);
        for (int j = 0; j < 8; ++j)
        {
            write("    %08b\n", results[i*8+j]);
        } // for
    } // for

} // dump_results()

void process_char(
    int char_row,
    int char_col,
    int start_x,
    object myimg)
{
    int x = start_x + (char_col * 8);
    int y = char_row * 8;

    for (int r = 0; r < 8; ++r)
    {
        int mybyte = 0;
        int mymask = 0xff;
        for (int bp = 0; bp < 4; ++bp)
        {
            array rgb = myimg->getpixel(x+(bp*2), y+r);
            if (rgb[0] == RGB_ON)
            {
                mybyte |= (ON_PATTERN<<(6-(bp*2)));
                mymask ^= BIT_PAIRS[bp];
            } // if
        } // for
        results += ({ mybyte });
        masks += ({ mymask });
    } // for

} // process_char()




