constant ASSETS_DIR = "../../assets/powerarc";
constant INFILE = "precision_steps.png";
constant OUTFILE = "precision_steps.bin";
constant NUM_ROWS = 48;
constant PIXEL_CODE = 0b11;
constant RESTORE_CODE = 0b01;
constant REPEAT_ROW_CODE = 0;   //NUM_ROWS;
constant ROW_OFFSET = 17; //*8;
constant ENTRY_LEN = 5;
constant SHADE_STEP = 5;
constant X_OFFSET_CHARS = 4;

array(int) base_patterns = ({});
// Key = 'step'; value = array of arrays (of ints).
mapping(int:array(array(int))) results = ([]);
int num_steps;

int main()
{
    load_base_patterns();

    string path = sprintf("%s/%s", ASSETS_DIR, INFILE);
    object img = Image.PNG.decode( Image.load_file(path) );
    write("size=%d*%d\n", img->xsize(), img->ysize());
    for (int i = 0; i < NUM_ROWS; ++i)
    {
        process_row(i, img);
    } // for

    num_steps = sizeof(results);
    write("how many? %d\n", num_steps);

    //write("RESULTS:\n%O\n", results);
    write_file();

    return 0;

} // main()


void process_row(int i, object img)
{
    // Each row consists of 6 bytes.
    // Each byte has 4 pixels (pixels are double-width).
    for (int j = 0; j < 6; ++j)
    {
        process_byte(i, j, img);
    } // for

} // process_row()


void process_byte(int row, int byte, object img)
{
    int pattern = 0;
    int restore_pattern = 0;
    int last_step = -1;
    // 'c' keeps track of the column (= x-coordinate).
    int c = byte*8;

    for (int p = 0; p < 4; ++p)
    {
        // Reading pixels left-to-right.
        array rgb = img->getpixel(c+p*2, row);
        // Is this pixel a shade of red?
        if (rgb[0])
        {
            int step = (255 - rgb[0]) / SHADE_STEP;
            // Reset pattern if step has changed.
            if (step != last_step)
            {
                if (pattern)
                {
                    add_entry(row, byte, pattern, last_step, restore_pattern);
                    //write("[%d,%d]:%d (%d)\n", row, byte, pattern, last_step);
                } // if

                pattern = 0;
                restore_pattern = 0;
                last_step = step;
            } // if
            pattern += PIXEL_CODE<<(6-(p*2));
            // FIXME: need to look at original bitmap to determine the
            // 'restore' pattern...
            int offset = ((row / 8) * (6*8)) + (byte * 8) + (row % 8);
            //write("%d,%d: %d\n", row, byte, offset);
            int base_patt = base_patterns[offset]; 
            int mask = 0b11 << (6-(p*2));
            base_patt = base_patt & mask;
            //restore_pattern += RESTORE_CODE<<(6-(p*2));
            restore_pattern += base_patt;
        } // if

        if (rgb[1])
        {
            int step = (255 - rgb[1]) / SHADE_STEP;
            // Reset pattern if step has changed.
            if (step != last_step)
            {
                if (pattern)
                {
                    add_entry(row, byte, pattern, last_step, restore_pattern);
                } // if

                pattern = 0;
                restore_pattern = 0;
                last_step = step;
            } // if
            pattern += PIXEL_CODE<<(6-(p*2));
            // FIXME: need to look at original bitmap to determine the
            // 'restore' pattern...
            int offset = ((row / 8) * (6*8)) + (byte * 8) + (row % 8);
            //write("%d,%d: %d\n", row, byte, offset);
            int base_patt = base_patterns[offset]; 
            int mask = 0b11 << (6-(p*2));
            base_patt = base_patt & mask;
            //restore_pattern += RESTORE_CODE<<(6-(p*2));
            restore_pattern += base_patt;
        } // if

    } // for

    if (pattern)
    {
        add_entry(row, byte, pattern, last_step, restore_pattern);
        //write("[%d,%d]:%d (%d)\n", row, byte, pattern, last_step);
    } // if

} // process_byte()


void add_entry(
    int row,
    int byte,
    int pattern,
    int step,
    int restore_pattern)
{
    // FIXME: maybe put this back in (modified) later!
    /*
    if ((step < 8) || (step == 14)) {
        restore_pattern = 0;
    } // if
    */

    // NOTE: remember to include the MASK!!!
    array a = ({ row, byte, 255-pattern, pattern, restore_pattern });
    write("STEP:%d ***%d,%d\n", step, row, byte);
    write("%08b  %08b  %08b\n", a[2], a[3], a[4]);
    if (zero_type(results[step]))
    {
        results[step] = ({ a });
    }
    else
    {
        results[step] += ({ a });
    } // if ... else

} // add_entry()


void write_file()
{
    string path = sprintf("%s/%s", ASSETS_DIR, OUTFILE);
    Stdio.File fout = Stdio.File(path, "wct");
    // A 'dummy' entry to begin with.
    //array output = ({ 5,0,255,0,0,0xff });
    array output = ({});

    for (int i = 0; i < num_steps; ++i)
    {
        array arr = results[i];
        //write("%d: %O\n", i, arr);
        write("%d:\n", i);

        int last_row = (-1);
        for (int b = 0; b < sizeof(arr); ++b)
        {
            int row = arr[b][0] / 8;
            int col = arr[b][1];
            int mask = arr[b][2];
            int pattern = arr[b][3];
            int restore_pattern = arr[b][4];

            if (row == last_row) {
                output += ({ REPEAT_ROW_CODE });
            }
            else {
                output += ({ (row+ROW_OFFSET)*8 });
                last_row = row;
            } // if ... else
            // NOTE: integer division!
            int offset = (col*8) + (arr[b][0]%8) + X_OFFSET_CHARS*8;
            output += ({ offset, mask, pattern, restore_pattern });
            //write("%O\n", output);
        } // for

        // Print out details for this step.
        int begin = sizeof(output) - ENTRY_LEN*sizeof(arr);
        int counter = 0;
        for (int j = begin; j < sizeof(output); ++j)
        {
            if (counter && (!(counter%ENTRY_LEN))) { write("\n"); }
            write("%3d [%02x] ", output[j], output[j]);
            ++counter;
        } // for
        write("\n");

        // End-of-step marker.
        output += ({ 0xff });

        //fout->write("%d: %O\n", i, output);
    } // for

    // Add end-of-table marker.
    output += ({ 0xfe });

    // Create an entry to go at top of file.  Row & byte from first entry,
    // followed by $ff,$00,$00.
    array dummy_entry = ({ output[0], output[1], 0xff, 0, 0 });

    fout->write("%s", (string) dummy_entry);
    fout->write("%s", (string) output);
    fout->close();

} // write_file()

void load_base_patterns()
{
    Stdio.File fin = Stdio.File("base_patterns.txt", "r");
    Iterator iter = fin->line_iterator();
    do {
        string line = iter->value();
        line = (string) String.trim_all_whites(line);
        base_patterns += ({ ((int) line) });
    } while (iter->next());
    fin->close();

} // load_base_patterns()


