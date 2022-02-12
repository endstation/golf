enum { BLACK,WHITE,RED,CYAN,VIOLET,GREEN,BLUE,YELLOW,ORANGE,BROWN,LIGHT_RED,
    GREY1,GREY2,LIGHT_GREEN,LIGHT_BLUE,GREY3 };
array(string) color_names = ({ "black","white","red","cyan","violet","green",
    "blue","yellow","orange","brown","light red","grey1","grey2","light green",
    "light blue","grey3" });
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
constant ASSETS_DIR = "../../assets/powerarc";

int byte_index = 0;
//mapping COLOR_CODES = ([ BROWN:0, GREY1:3, GREY2:1, GREY3:2 ]);
//mapping COLOR_CODES = ([ BROWN:0, GREY1:3, WHITE:1, LIGHT_BLUE:2 ]);
mapping COLOR_CODES = ([ ORANGE:0, BLACK:3, WHITE:1, GREY3:2 ]);
string outfile = "base_patterns.txt";
array(int) all_bytes = ({});

int main()
{
    string path = sprintf("%s/base_icon.png", ASSETS_DIR);
    string data = Image.load_file(path);
    object img = Image.PNG.decode(data);
    write("%d*%d\n", img->xsize(), img->ysize());

    int base_y = 0;
    for (int i = 0; i < 6; ++i) 
    {
        for (int j = 0; j < 6; ++j)
        {
            for (int k = 0; k < 8; ++k)
            {
                int x = j * 8;
                //int y = base_y + k;
                int y = (i * 8) + k;
                process_byte(x, y, img);
            } // for k
        } // for j
        base_y += 8;
    } // for i

    write_output_file();

    return 0;

} // main()

void process_byte(int x, int y, object img)
{
    //write("x,y = %d,%d\n", x, y);
    int color0 = lookup_color(img->getpixel(x, y));
    int color1 = lookup_color(img->getpixel(x+2, y));
    int color2 = lookup_color(img->getpixel(x+4, y));
    int color3 = lookup_color(img->getpixel(x+6, y));
    int byte = (COLOR_CODES[color0]<<6)|(COLOR_CODES[color1]<<4)|
        (COLOR_CODES[color2]<<2)|(COLOR_CODES[color3]);
    //write("%d\n", byte);
    all_bytes += ({ byte });

} // process_byte()

int lookup_color(array(int) rgb)
{
    for (int i = 0; i < sizeof(RGB); ++i)
    {
        array a = RGB[i];
        if ( equal(a, rgb) ) //rgb[0]==a[0] && rgb[1]==a[1] && rgb[2]==a[2] )
        {
            return i;
        } // if
    } // for

    werror("Invalid color! %O\n", rgb);
    exit(1);

} // lookup_color()

void write_output_file()
{
    object fout = Stdio.File(outfile, "wct");
    foreach (all_bytes, int b)
    {
        fout->write("%d\n", b);
    } // foreach
    fout->close();

} // write_output_file()





