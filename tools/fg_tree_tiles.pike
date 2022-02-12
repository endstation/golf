// Process PNG file for foreground tree tiles (*5).

constant FILENAME = "/home/matthew/hp2_progs/programming/6510assembly/commodore_golf/assets/mc_tree_tiles.png";
constant PIXEL_PATTERN = 0b11;
array(int) byte_array = ({});
enum { RED, GREEN, BLUE };

int main()
{
    string data = Image.load_file(FILENAME);
    Image.Image img = Image.PNG.decode(data);
    write("dims=%d*%d\n", img->xsize(), img->ysize());
    return 0;

    array(int) xs = ({ 0,8,16,24,32,40,48,56,64 });

    for (int i = 0; i < 2; ++i)
    {
        int base_x = xs[i];
        for (int r = 0; r < 8; ++r)
        {
            int mybyte = 0;
            for (int c = 0; c < 4; ++c)
            {
                // Get pixel.
                array rgb = img->getpixel(base_x+c*2, r);
                // White, green or blue?
                if (!rgb[RED])
                {
                    mybyte += 0b11<<(6-c*2);
                }
                //else if (rgb[BLUE] && !rgb[RED])
                //{
                //    mybyte += 0b10<<(6-c*2);
                //} // if ... else
            } // for
            write("%08b\n", mybyte);
            byte_array += ({ mybyte });
        } // for
    } // for


/*
    for (int i = 0; i < 4; ++i) {
        int my_base_x = base_x[i];
        int my_base_y = base_y[i];
        for (int j = 0; j < 8; ++j) {
            // Get byte.
            int my_byte = 0;
            for (int k = 0; k < 4; ++k) {
                array a = img->getpixel(my_base_x+k*2, my_base_y+j);
                if (!a[0]) {
                    // A black pixel.
                    my_byte += PIXEL_PATTERN<<(6-k*2);
                } 
                else if (i >= 2 && j == 7) {
                    //if (j == 6) {
                    //    my_byte += 0b10<<(6-k*2);
                    //}
                    //else if (j == 7) {
                        my_byte += 0b01<<(6-k*2);
                    //} // if ... else
                } // if ... else
            } // for
            write("%08b\n", my_byte);
            byte_array += ({ my_byte });
        } // for
    } // for
*/
    write("how many? %d\n", sizeof(byte_array));
    write_file();

    return 0;

} // main()

void write_file()
{
    Stdio.File fout = Stdio.File("fg_tree_tiles.bin", "wct");
    fout->write("%s", (string) byte_array);
    fout->close();

} // write_file





