int chars_w;
int chars_h;
array(array(int)) all_chars = ({});
array(int) current_char;
constant PATTERN = 0b01;

int main(int argc, array(string) argv)
{
    string file_in = argv[1];
    string data = Image.load_file(file_in);
    object img = Image.PNG.decode(data);
    write("w*h=%d*%d\n", img->xsize(), img->ysize());
    chars_w = img->xsize() / 8;
    chars_h = img->ysize() / 8;
    current_char = allocate(8);
    int count = 0;

    for (int i = 0; i < chars_h; ++i)
    {
        for (int j = 0; j < chars_w; ++j)
        {
            //write("-> %d\n", count);
            process_char(i, j, img);
            all_chars += ({ copy_value(current_char) });
        } // for
    } // for
    
    //write("how many? %d\n", sizeof(all_chars));
    foreach (all_chars, array a) {
        for (int i = 0; i < 8; ++i) {
            write("%08b\n", a[i]);
        } // for
        write("\n");
    } // foreach

    write_output_file(argv[2]);

    return 0;

} // main()

void process_char(int row, int col, object img)
{
    // Find place in PNG image.
    int x_pixels = col*8;
    int y_pixels = row*8;

    for (int i = 0; i < 8; ++i)
    {
        int byte = process_byte(x_pixels, y_pixels+i, img);
        current_char[i] = byte;
    } // for
    //write("\n\n");

} // process_char()

int process_byte(int x, int y, object img)
{
    int byte = 0;

    for (int i = 0; i < 4; ++i)
    {
        // Look at bit pairs.
        int xx = x + (i * 2);
        array rgb = img->getpixel(xx, y);
        if ( Array.all(rgb, lambda(int c) { return !c; }) )
        {
            byte += (PATTERN << (6-(i*2)));
        } // if
    } // for
    
    //write("%08b\n", byte);
    //all_bytes += ({ byte });
    return byte;

} // process_byte()

void write_output_file(string file_out)
{
    Stdio.File fout = Stdio.File(file_out, "wct");
    fout->write("%s", (string) Array.flatten(all_chars));
    fout->close();

} // write_output_file()


