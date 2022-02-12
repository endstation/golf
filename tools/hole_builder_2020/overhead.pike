constant TILE_SIZE = 10;
array(array) all_quads = ({});
object myimg;
enum { BLACK,WHITE,RED,CYAN,VIOLET,GREEN,BLUE,YELLOW,ORANGE,BROWN,LIGHT_RED,
    GREY1,GREY2,LIGHT_GREEN,LIGHT_BLUE,GREY3 };
mapping(string:int) TERRAINS = ([ "rough":ORANGE, "bunker":YELLOW,
    "fairway":GREEN, "water":CYAN]);
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
constant BACKGROUND_COLOR = ORANGE;
constant BORDER_COLOR = BLACK;
constant FLAG_COLOR = BLACK;

string tmx_filename;
string outfile;
string outfile_big;

int main(int argc, array(string) argv)
{
    if (argc != 4)
    {
        werror("Usage: pike overhead.pike <infile> <outfile> <outfile-big>\n");
        exit(1);
    } // if

    tmx_filename = argv[1];
    outfile = argv[2];
    outfile_big = argv[3];
    //string tmx_filename = argv[1];
    //write("tmx_filename=%s\n", tmx_filename);
    //object tmxmap = TMX_parser.TMX_parser(tmx_filename)->get_map();
    //write("dims=%d*%d\n", tmxmap->width, tmxmap->height);
    //int hole_number = (int) argv[1];
    //outfile = sprintf("%s/topholemap%02d.png", HOLES_DIR2, hole_number);
    //outfile_big = sprintf("%s/topholemap%02d_original.png", HOLES_DIR2, hole_number);
    //string tmx_filename = sprintf("%s/top_hole_%02d.tmx", HOLES_DIR, hole_number);
    object tmxmap = TMX_parser.TMX_parser(tmx_filename)->get_map();

    // Look for the 'quads' layer (- actually, 'object_group'!!!).
    int found = -1;
    for (int i = 0; i < sizeof(tmxmap->object_groups); ++i)
    {
        write("%s\n", tmxmap->object_groups[i]->name);
        if (tmxmap->object_groups[i]->name == "quads")
        { 
            found = i;
            break;
        } // if
    } // for
    //write("found=%d\n", found);
    collect_quads(tmxmap->object_groups[found]);

    create_base_image();
    draw_quads();
    draw_border();
    draw_flag((int) tmxmap->properties["target_x"],
            (int) tmxmap->properties["target_y"]);
    write_image();
    write_big_image();

    return 0;

} // main()

void collect_quads(object og)
{
    foreach (og->tmx_objects, object tmxo)
    {
        string terrain = tmxo->properties["type"];
        /*
        write("terrain=%s\n", terrain);
        write("%f,%f [%f*%f]\n", tmxo->x_float, tmxo->y_float,
                tmxo->width_float, tmxo->height_float);
        */
        int x = (int) ((tmxo->x_float / TILE_SIZE) + 0.5) ;
        int y = (int) ((tmxo->y_float / TILE_SIZE) + 0.5);

        // NOTE: these will be 'one-past-the-end'.
        int x2 = (int) (((tmxo->x_float + tmxo->width_float) / TILE_SIZE) + 0.5);
        int y2 = (int) (((tmxo->y_float + tmxo->height_float) / TILE_SIZE) + 0.5);

        //write("(%d,%d) -> (%d,%d)\n", x, y, x2, y2);

        all_quads += ({ ({ TERRAINS[terrain],x,y,x2,y2 }) });
    } // foreach

} // collect_quads()

void create_base_image()
{
    myimg = Image.Image(4*8, 6*8, @RGB[BACKGROUND_COLOR]);

} // create_base_image()

void write_image()
{
    object fout = Stdio.File(outfile, "wct");
    string data = Image.PNG.encode(myimg);
    fout->write("%s", data);
    fout->close();

} // write_image()

void draw_quads()
{
    foreach (all_quads, array a)
    {
        myimg->box( a[1], a[2], a[3]-1, a[4]-1, @RGB[ a[0] ] );
    } // foreach

} // draw_quads()

void draw_flag(int x, int y)
{
    // x should be a multiple of 2.
    if (x % 2) { --x; }
    //write("draw flag @ (%d,%d)\n", x, y);

    //array a = ({ x,y, x+1,y, x,y-1, x+1,y-1, x,y-2, x+1,y-2, x+2,y-2, x+3,y-2 });
    //for (int i = 0; i < sizeof(a); i += 2)
    //{
    //    myimg->setpixel(a[i], a[i+1], @RGB[FLAG_COLOR]);
    //} // for
    myimg->setpixel(x, y, @RGB[FLAG_COLOR]);
    myimg->setpixel(x+1, y, @RGB[FLAG_COLOR]);

} // draw_flag()

void draw_border()
{
    myimg->line(0,0, 4*8,0, @RGB[BORDER_COLOR]);
    myimg->line(0,6*8-1, 4*8,6*8-1, @RGB[BORDER_COLOR]);
    myimg->line(0,0, 0,6*8, @RGB[BORDER_COLOR]);
    myimg->line(1,0, 1,6*8, @RGB[BORDER_COLOR]);
    myimg->line(4*8-2,0, 4*8-2,6*8, @RGB[BORDER_COLOR]);
    myimg->line(4*8-1,0, 4*8-1,6*8, @RGB[BORDER_COLOR]);

} // draw_border()

void write_big_image()
{
    object bigimg = Image.Image(320, 200, @RGB[BACKGROUND_COLOR]);
    bigimg->paste(myimg, 0, 0);

    object fout = Stdio.File(outfile_big, "wct");
    string data = Image.PNG.encode(bigimg);
    fout->write("%s", data);
    fout->close();

} // write_big_image()

