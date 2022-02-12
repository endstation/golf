// Copyright 2020 Matthew Clarke

// NOTE: Output file = bd_8x5.bin, in the same directory as the input files.

constant ON_PATTERN = 0b01;
constant BACKDROPS_DIR = "../../assets/backdrops";
// NOTE: this is actually just a dummy address. Data will be loaded directly
// into the 'backdrop' module.
constant DEST_ADDR = 0xe000;
constant WIDTH_IN_CHARS = 8;
constant HEIGHT_IN_CHARS = 5;
array(string) COURSES = ({
    "beechings", "sierre", "alab", "hokusai", "carver", "newyork" });

array(int) patterns = ({});
array(int) masks = ({});
int course_index;


int main(int argc, array(string) argv)
{
    if (argc != 2)
    {
        werror("Usage: pike bdrop_tiles_builder.pike <course index>\n");
        exit(1);
    } // if

    course_index = (int) argv[1];
    string output_file = sprintf("%s/%s/bd_8x5.bin", BACKDROPS_DIR,
            COURSES[course_index]);
    string infile = sprintf("%s/%s/bdrop_8x5.png", BACKDROPS_DIR,
            COURSES[course_index]); 

    string data = Image.load_file(infile);
    object myimg = Image.PNG.decode(data);
    //write("%d*%d\n", myimg->xsize(), myimg->ysize());

    if (myimg->xsize() != (WIDTH_IN_CHARS*8)
        || myimg->ysize() != (HEIGHT_IN_CHARS*8))
    {
        werror("Image is invalid size!\n");
        exit(1);
    } // if

    for (int i = 0; i < HEIGHT_IN_CHARS; ++i)
    {
        for (int j = 0; j < WIDTH_IN_CHARS; ++j)
        {
            process_char(i, j, myimg);
        } // for
    } // for
    
    //write("how many? %d\n", sizeof(patterns));

    object fout = Stdio.File(output_file, "wct");
    fout->write("%s", (string) ({ DEST_ADDR&0xff, (DEST_ADDR>>8)&0xff }));
    fout->write("%s", (string) masks);
    fout->write("%s", (string) patterns);
    fout->close();

    dump_results();

    return 0;

} // main()

void process_char(int myrow, int mycol, object myimg)
{
    //write("row=%d, col=%d\n", myrow, mycol);

    // One PIXEL row at a time.
    int pixel_row = myrow * 8;
    for (int r = pixel_row; r < (pixel_row+8); ++r)
    {
        int byte = 0;
        int mask = 0;
        // NOTE: each byte is divided up into four (double-width) pixels.
        // Iterate over PIXEL columns...
        for (int c = (mycol*8); c < ((mycol*8)+8); c += 2)
        {
            int x = c % 8;
            array rgb = myimg->getpixel(c, r);
            if ( !rgb[0] )
            {
                byte += (ON_PATTERN << (6-x));
            }
            else 
            {
                mask += (0b11 << (6-x));
            } // if ... else
        } // for

        patterns += ({ byte });
        masks += ({ mask });
    } // for

} // process_char()

void dump_results()
{
    for (int i = 0; i < sizeof(patterns)/8; ++i)
    {
        write("*** %d ***\n", i);
        for (int j = 0; j < 8; ++j)
        {
            write("    %08b [%08b]\n", patterns[i*8+j], masks[i*8+j]);
        } // for
    } // for

} // dump_results()



