constant NUM_STEPS = 36;
constant MIN_RGB = 80;
constant RGB_STEP = 5;
constant ASSETS_DIR = "../../assets/powerarc";
array(array(array(int))) cells;
array PIXEL_PATTERNS = ({ 0b11000000,0b00110000,0b00001100,0b00000011 });
mapping(int:int) patterns_running = ([]);
array(mapping(int:int)) final_results = ({});
array(mapping(int:int)) final_results2 = ({});
array(int) base_patterns = ({});
array(int) table_data = ({});
constant X_OFFSET_CHARS = 4;
array(int) ADDRESSES = ({ 0xf560, 0xf6a0, 0xf7e0, 0xf920, 0xfa60, 0xfba0 });

int main()
{
    load_base_patterns();

    cells = allocate(NUM_STEPS);
    for (int i = 0; i < NUM_STEPS; ++i) {
        cells[i] = ({});
    } // for

    string path = sprintf("%s/power_steps.png", ASSETS_DIR);
    string data = Image.load_file(path);
    object img = Image.PNG.decode(data);
    write("dims=%d*%d\n", img->xsize(), img->ysize());

    // NOTE: 48 'pixel' rows, 6 'char' columns...
    for (int i = 0; i < 48; ++i)
    {
        for (int j = 0; j < 6; ++j)
        {
            examine_byte(i, j, img);
        } // for j
    } // for i

    //write("%O\n", cells);
    analyze_cells();
    output_table();
    //apply_base_patterns();

    /*
    for (int i = 0; i < sizeof(final_results); ++i)
    {
        write("STEP %d\n", i);
        write("%O\n", final_results[i]);
    } // for
    */
    display_table();
    // Create binary file.
    //Stdio.File fout = Stdio.File("powsteps2.bin", "wct");
    path = sprintf("%s/power_steps.bin", ASSETS_DIR);
    Stdio.File fout = Stdio.File(path, "wct");
    fout->write("%s", (string) table_data);
    fout->close();

    /*
    int i = 0;
    foreach (base_patterns, int bp) {
        write("%3d: %d\n", i, bp);
        ++i;
    } // foreach
    */
    array STEPS = ({ 5,10,15,20,25,30 });
    array(int) putting_data = ({});
    foreach (STEPS, int mystep)
    {
        putting_data += get_putting_power_step_data(mystep);
    } // foreach
    write("how many? %d\n", sizeof(putting_data));
    display_putting_data(putting_data);

    return 0;

} // main()

// Look at this byte on the 'steps' image and if it's got any purple in it,
// (i.e. it's part of the 'filling-up' graphic), record which indices it
// belongs to - in other words, which 'step' in the 36 (?) steps that make up
// the whole arc.
void examine_byte(int row, int col, object img)
{
    int xx = col*8;
    for (int i = 0; i < 4; ++i)
    {
        array rgb = img->getpixel(xx+(i*2), row);
        if (!rgb[1])
        {
            // This is a 'coloured-in' cell...
            int rb = rgb[0];
            //int index = 43 - ((rb - 40) / 5);
            int index = (NUM_STEPS-1) - ((rb - MIN_RGB) / RGB_STEP);
            cells[index] += ({ ({row, xx+(i*2)}) });
            //write("%d @ %d,%d\n", rb, row, xx+(i*2));
        } // if
    } // for

} // examine_byte()

void analyze_cells()
{
    for (int i = 0; i < NUM_STEPS; ++i)
    {
        process_step(i);
    } // for

} // analyze_cells()

void process_step(int i)
{
    mapping(int:int) m = ([]);
    // Size of 'arr' is the number of bytes that are involved in drawing each
    // step of the powering up arc.
    array(array(int)) arr = cells[i];
    int len = sizeof(arr);
    write("step=%d [%d]\n", i, len);
    for (int i = 0; i < len; ++i)
    {
        array arr2 = arr[i];
        // arr2 holds row and column.
        // First find the index of the byte.
        int row = arr2[0];
        int col = arr2[1];
        int byte_index = (row/8)*(6*8) + (col/8)*8 + (row-((row/8)*8));
        int pattern = (col - ((col/8)*8)) / 2;
        //write("  %d -> %d\n", byte_index, pattern);
        if ( zero_type(patterns_running[byte_index]) ) {
            m[byte_index] = PIXEL_PATTERNS[pattern];
            patterns_running[byte_index] = PIXEL_PATTERNS[pattern];
        }
        else {
            m[byte_index] = patterns_running[byte_index];
            m[byte_index] |= PIXEL_PATTERNS[pattern];
            patterns_running[byte_index] |= PIXEL_PATTERNS[pattern];
        } // if ... else
        write("before=%08b\n", m[byte_index]);
        m[byte_index] |= base_patterns[byte_index];
        write(" after=%08b\n", m[byte_index]);
    } // for

    array keys = sort(indices(m));
    /*
    foreach (keys, int k) {
        write("    %3d: %08b\n", k, m[k]);
    } // foreach
    */
    final_results += ({ m });

} // process_step()

void output_table()
{
    int last_row = 10000000;
    for (int i = 0; i < sizeof(final_results); ++i)
    {
        mapping m = final_results[i];
        array keys = sort(indices(m));
        foreach (keys, int k) {
            int row = 136 + 8 * (k / (6*8));
            int offset = (k % (6*8)) + X_OFFSET_CHARS*8;
            if (row != last_row)
            {
                table_data += ({ row,offset,m[k] });
                last_row = row;
            }
            else
            {
                // Row hasn't changed so enter 0.
                table_data += ({ 0,offset,m[k] });
            } // if ... else
        } // foreach
        // End of 'step' indicated by $ff.
        table_data += ({ 0xff });
    } // for
    // End of table indicated by another $ff (i.e. two together).
    table_data += ({ 0xff });

} // output_table()

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

/*
void apply_base_patterns()
{
    foreach (final_results, mapping m)
    {
        array keys = indices(m);
        foreach (keys, int k)
        {
            int bg_pattern = base_patterns[k];
        } // foreach
    } // foreach

} // apply_base_patterns()
*/

void display_table()
{
    write("%O\n", table_data);

} // display_table()

array(int) get_putting_power_step_data(int step)
{
    array(array(int)) a = cells[step];
    write("how many=%d\n", sizeof(a));
    int n = sizeof(a);
    mapping(int:int) results = ([]);

    for (int i = 0; i < n; ++i)
    {
        array b = a[i];
        write("%d: %O\n", i, b);
        int myrow = b[0];
        int mycol = b[1];
        // We must combine pixels that are written into the same byte.  So 
        // find byte index for this pixel and add it to a mapping, where 
        // byte index is the key.
        int byte_index = (myrow/8)*(6*8) + ((mycol/8)*8) + (myrow%8);
        write("byte index=%d\n", byte_index);
        // Get the mask pattern that corresponds to this byte.
        int mask = PIXEL_PATTERNS[ (mycol%8)/2 ];
        write("mask=%08b\n", mask);

        if ( zero_type(results[byte_index]) )
        {
            results[byte_index] = mask;
        }
        else
        {
            results[byte_index] |= mask;
        } // if ... else

        /*
        int myrow = b[0];
        int mycol = b[1];
        // NOTE: integer division!
        int char_row = myrow / 8;
        int char_col = mycol / 8;
        int col_offset = myrow % 8;
        write("and in chars: (%d,%d) [%d]\n", char_row, char_col, col_offset);
        int address = ADDRESSES[char_row] + (char_col * 8) + col_offset;
        write("address=%04x\n", address);
        int byte_index = (myrow/8)*(6*8) + (mycol/8)*8 + (myrow-((myrow/8)*8));
        write("byte_index=%d\n", byte_index);

        // Find the (mask for the) pattern that should be written at this
        // address.
        int pix_i = (mycol - (8*char_col)) / 2;
        int mask = PIXEL_PATTERNS[pix_i];
        if (zero_type(results[address]))
        {
            // Not present!
            results[address] = mask;
        }
        else
        {
            results[address] |= mask;
        } // if ... else
        */
    } // for

    // We now have a collection of byte indices and mask patterns that 
    // together constitute the current power step.
    // Each byte index must now be converted into a bitmap row (multiple of 8)
    // and an offset.  Remember to take into account the fact that the power-
    // arc is drawn in the 4th column from the left (counting from zero)...
    write("\n\n\n");
    array(int) all_data = ({});
    int last_row = (-1);
    foreach ( sort(indices(results)), int key )
    {
        write("%d: %08b\n", key, results[key]);
        int char_row = key / (6*8);
        write("char_row=%d\n", char_row);
        int remainder = key % (6*8);
        int char_col = remainder / 8;
        write("char_col=%d\n", char_col);
        int offset = (char_col*8) + (remainder%8);
        write("offset=%d\n", offset);
        offset += X_OFFSET_CHARS*8; 
        int bitmap_row = (char_row*8) + (17*8);
        write("bitmap_row=%d\n", bitmap_row);
        if (bitmap_row == last_row)
        {
            all_data += ({ 0, offset, results[key] });
        }
        else
        {
            all_data += ({ bitmap_row, offset, results[key] });
            last_row = bitmap_row;
        } // if ... else
    } // foreach

    all_data += ({ 0xff });
    return all_data;

} // get_putting_power_step_data()

void display_putting_data(array arr)
{
    array table_offsets = ({ 0 });
    int current_step = 5;
    int i = 0;
    write("\n\n\nSTEP: %d [%d]\n", current_step, i);

    while (1)
    {
        int row = arr[i];
        if (row == 0xff)
        {
            current_step += 5;
            if (current_step > 30)
            {
                break;
            }
            else
            {
                ++i;
                write("STEP: %d [%d]\n", current_step, i);
                table_offsets += ({ i });
                continue;
            } // if ... else
        } // if
        
        write("  %3d, %3d: %08b\n", row, arr[i+1], arr[i+2]);
        i += 3;
    } // while

    write("Table offsets:\n%O\n", table_offsets);

    string path = sprintf("%s/putting_markers.bin", ASSETS_DIR);
    object fout = Stdio.File(path, "wct");
    fout->write("%s", (string) arr);
    fout->close();

} // display_putting_data()





