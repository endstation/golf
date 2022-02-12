array(int) bitmap_data = ({});
mapping(int:int) distribution = ([]);
array(array) pixel_groups = ({});
array(int) compressed_data = ({});

int main()
{
    object fin = Stdio.File("test_bitmap.asm", "r");
    object iter = fin->line_iterator();
    string line;
    object digit_regexp = Regexp.PCRE._pcre("(\\d)");

    do 
    {
        line = iter->value();
        line = String.trim_all_whites(line);
        if (sizeof(line))
        {
            array a = digit_regexp->exec(line);
            line = line[ a[0].. ];
            write("---> %s\n", line);
            process_line(line);
            //write("%O\n", a);
        } // if
    } while (iter->next());

    fin->close();

    write("how many? %d\n", sizeof(bitmap_data));
    int j = search(bitmap_data, 2);
    write("j=%d\n", j);

    // Compress here.
    int i = 0;
    int current_byte = bitmap_data[i];
    int count = 1;
    ++i;

    while (i < 8000)
    {
        if (bitmap_data[i] == current_byte)
        {
            ++count;
        }
        else
        {
            //write("%d [%d]\n", current_byte, count);
            pixel_groups += ({ ({current_byte,count}) });
            current_byte = bitmap_data[i];
            count = 1;
        } // if ... else
        ++i;
    } // while
    //write("%d [%d]\n", current_byte, count);
    pixel_groups += ({ ({current_byte,count}) });

    write("\n\n\n\n\n\n");
    //write("%O\n", distribution);
    write("%O\n", pixel_groups);

    int escape_code = choose_escape_code();
    write("escape_code=%d\n", escape_code);

    write_output(escape_code);
    write("how many compressed? %d\n", sizeof(compressed_data));

    return 0;

} // main()

void process_line(string line)
{
    array items = line / ",";
    foreach (items, string s)
    {
        bitmap_data += ({ (int) s });
        int x = (int) s;
        if (zero_type(distribution[x]))
        {
            distribution[x] = 1;
        }
        else
        {
            ++distribution[x];
        } // if ... else
    } // foreach

} // process_line()

// Pick a byte value that doesn't appear anywhere in the bitmap.
int choose_escape_code()
{
    int i = 0;
    while (i < 256)
    {
        if (zero_type(distribution[i]))
        {
            break;
        } // if
        ++i;
    } // while
    return i;

} // choose_escape_code()

void write_output(int escape_code)
{
    compressed_data += ({ escape_code });
    foreach (pixel_groups, array a)
    {
        int n = a[1];
        if (n <= 256)
        {
            compress(escape_code, a[0], n);
        }
        else
        {
            int n1 = 256;
            int n2 = n - 256;
            compress(escape_code, a[0], n1);
            compress(escape_code, a[0], n2);
        } // if ... else
    } // foreach

    // End sequence:
    compressed_data += ({ escape_code, 0, 0 });

} // write_output()

void compress(int escape_code, int byte, int n)
{
    // Special case if n is 256.
    if (n == 256) { n = 0; }
    // Only compress if more than three consecutive bytes.
    if (n > 0 && n <= 3)
    {
        for (int i = 0; i < n; ++i) {
            compressed_data += ({ byte });
        } // for
    }
    else
    {
        compressed_data += ({ escape_code, byte, n });
    } // if ... else

} // compress()

