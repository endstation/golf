#!/usr/bin/env pike

constant RGB_ON = 85;
array(int) results = ({});

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

    return 0;

} // main()

void process_char(int i, object myimg)
{
    // One row at a time.
    for (int r = 0; r < 8; ++r)
    {
        int byte = 0;
        for (int c = i*8; c < (i*8)+8; ++c)
        {
            array rgb = myimg->getpixel(c, r);
            if ( Array.all(rgb, lambda(int e) { return e==RGB_ON; }) )
            {
                int x = c % 8;
                byte += 1<<(7-x);
            } // if
        } // for
        results += ({ byte });
    } // for

} // process_char()

