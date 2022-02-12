// Process PNG file for foreground tree tiles (*5).

constant FILENAME = "/home/matthew/hp2_progs/programming/6510assembly/commodore_golf/assets/mc_tree_tiles2.png";
constant PIXEL_PATTERN = 0b11;
array(int) byte_array = ({});
enum { RED, GREEN, BLUE };
int tiles_per_row;
int tiles_num_rows;

int main()
{
    string data = Image.load_file(FILENAME);
    Image.Image img = Image.PNG.decode(data);
    write("dims=%d*%d\n", img->xsize(), img->ysize());
    tiles_per_row = img->xsize() / 8;
    tiles_num_rows = img->ysize() / 8;
    write("tiles=%d*%d\n", tiles_per_row, tiles_num_rows);

    array(int) xs = ({ 0,8,16 });
    array(int) ys = ({ 0,8,16,24,32,40,48 });

    for (int r = 0; r < tiles_num_rows; ++r)
    {
        int base_y = ys[r];
        for (int c = 0; c < tiles_per_row; ++c)
        {
            write("\n");
            int base_x = xs[c];
            for (int i = 0; i < 8; ++i)
            {
                int mybyte = 0;
                for (int j = 0; j < 4; ++j)
                {
                    array rgb = img->getpixel(base_x+j*2, base_y+i);
                    if (rgb[0]==88 && rgb[1]==141 && rgb[2]==67)
                    {
                        mybyte += 0b10<<(6-j*2);
                    }
                    else if (rgb[0]==184 && rgb[1]==199 && rgb[2]==111)
                    {
                        mybyte += 0b01<<(6-j*2);
                    }
                    else if (rgb[0]==104 && rgb[1]==55 && rgb[2]==43)
                    {
                        mybyte += 0b11<<(6-j*2);
                    } // if ... else
                } // for
                write("%08b\n", mybyte);
                byte_array += ({ mybyte });
            } // for
        } // for
    } // for

    write("how many? %d\n", sizeof(byte_array));
    write_file();

    return 0;

} // main()

void write_file()
{
    Stdio.File fout = Stdio.File("mc_tree_tiles.bin", "wct");
    fout->write("%s", (string) byte_array);
    fout->close();

} // write_file





