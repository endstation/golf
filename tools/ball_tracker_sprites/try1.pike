constant ASSETS_DIR = "../../assets/sprites/overhead";
array(array(int)) all_results = ({});

int main()
{
    array all_files = sort( get_dir(ASSETS_DIR) );
    all_files = filter(all_files, lambda(string s) { return has_suffix(s, "png"); });
    for (int i = 0; i < sizeof(all_files); ++i)
    {
        write("%s\n", all_files[i]);
        process(all_files[i]);
    } // foreach

    for (int i = 0; i < sizeof(all_results); ++i)
    {
        write("---> %03d\n", i * 30);
        display_sprite(all_results[i]);
    } // for

    build_binary();

    return 0;

} // main()

void process(string filename)
{
    string path = sprintf("%s/%s", ASSETS_DIR, filename);
    write("path=%s\n", path);
    object img = Image.PNG.decode( Image.load_file(path) );
    write("size=%d*%d\n", img->xsize(), img->ysize());
    array arr = ({});

    int byte_count = 0;
    for (int r = 0; r < 21; ++r)
    {
        for (int c = 0; c < 3; ++c)
        {
            int mybyte = process_byte(r, c, img);
            arr += ({ mybyte });
            ++byte_count;
        } // for
    } // for

    //write("byte_count=%d\n", byte_count);
    all_results += ({ arr });

} // process()

int process_byte(int r, int c, object img)
{
    int xpos = c * 8;
    int byte = 0;

    for (int i = 0; i < 8; ++i)
    {
        array rgb = img->getpixel(xpos+i, r);
        if ( Array.any(rgb, `>, 0) )
        {
            byte += ( 1<<(7-i) );
        } // if
    } // for
    
    return byte;

} // process_byte()

void display_sprite(array a)
{
    for (int i = 0; i < sizeof(a); ++i)
    {
        if (!(i%3)) { write("\n"); }
        write("%08b  ", a[i]);
    } // for

    write("\n\n");

} // display_sprite()

void build_binary()
{
    array everything = ({});
    for (int i = 0; i < sizeof(all_results); ++i)
    {
        array a = all_results[i];
        for (int j = 0; j < sizeof(a); ++j)
        {
            if (a[j] > 0)
            {
                everything += ({ j, a[j] });
            } // if
        } // for
    } // for

    write("How big is everything? %d\n", sizeof(everything));

} // build_binary()



