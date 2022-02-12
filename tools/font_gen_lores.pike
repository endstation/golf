#!/usr/bin/env pike80

constant RGB_ON = 85;
array(int) results = ({});

constant ON_PATTERN = 0b01;
constant OFF_PATTERN = 0b11;
// Default is all on:
constant DEFAULT_BYTE = 0b10101010;
// Patterns for in-game font.
//constant ON_PATTERN = 0b11;
//constant OFF_PATTERN = 0b00;
//constant DEFAULT_BYTE = 0;

int main(int argc, array(string) argv)
{
    if (argc != 3)
    {
        werror("Usage: pike font_gen.pike <infile> <outfile>\n");
        exit(1);
    } // if

    string infile = argv[1];
    string outfile = argv[2];

    string data = Image.load_file(infile);
    object myimg = Image.PNG.decode(data);
    write("%d*%d\n", myimg->xsize(), myimg->ysize());

    int num_chars = myimg->xsize() / 8;
    write("num_chars=%d\n", num_chars);

    for (int i = 0; i < num_chars; ++i) {
        process_char(i, myimg);
    } // for

    // Write output file.
    Stdio.File fout = Stdio.File(outfile, "wct");
    fout->write("%s", (string) results);
    fout->close();

    //dump_results();

    return 0;

} // main()

void process_char(int i, object myimg)
{
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

