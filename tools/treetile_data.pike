constant FILENAME = "/home/matthew/hp2_progs/programming/6510assembly/commodore_golf/assets/trees_tile.png";
constant PIXEL_PATTERN = 0b10;
array(int) byte_array = ({});

int main()
{
    string data = Image.load_file(FILENAME);
    Image.Image img = Image.PNG.decode(data);
    write("dims=%d*%d\n", img->xsize(), img->ysize());

    array(int) base_x = ({ 0, 8, 0, 8 });
    array(int) base_y = ({ 0, 0, 8, 8 });

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

    write("how many? %d\n", sizeof(byte_array));
    write_file();

    return 0;

} // main()

void write_file()
{
    Stdio.File fout = Stdio.File("tree_tiles.bin", "wct");
    fout->write("%s", (string) byte_array);
    fout->close();

} // write_file





