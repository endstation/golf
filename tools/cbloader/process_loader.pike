constant PRG_ADDR = 0x59c;
constant INITLOADER_ADDR = 0x3000;
constant HEADER_SIZE = 2;
constant CORE_DIR = "/home/matthew/hp2_progs/programming/6510assembly/golfgit/commodore-golf/src/core";

array(int) main_code = ({});
array(int) init_code = ({});




int main()
{
    string data = Image.load_file("myloader.bin");
    write("size=%d\n", sizeof(data));

    // Offset to 'initloader'.
    int offset_to_init = INITLOADER_ADDR - PRG_ADDR + HEADER_SIZE;

    get_main_code(data);
    get_init_code(data, offset_to_init);

    // Write the output binary files.
    string filename = sprintf("%s/loader_main.bin", CORE_DIR);
    object fout = Stdio.File(filename, "wct");
    fout->write("%s", (string) main_code);
    fout->close();
    filename = sprintf("%s/loader_init.bin", CORE_DIR);
    fout = Stdio.File(filename, "wct");
    fout->write("%s", (string) init_code);
    fout->close();

    return 0;

} // main()

void get_main_code(string data)
{
    constant MAX_FF_RUN = 16;

    // When this gets to 16 (?!), assume we're done.
    int ff_run = 0;
    int i = 2;

    while (ff_run < MAX_FF_RUN)
    {
        int x = (int) data[i];
        if (x == 0xff)
        {
            ++ff_run;
        }
        else
        {
            ff_run = 0;
        } // if ... else
        main_code += ({ x });
        ++i;
    } // while

    write("i=%d [%x]\n", i, i);

} // get_main_code()

void get_init_code(string data, int start)
{
    int i = start;

    while (i < sizeof(data))
    {
        int x = (int) data[i];
        init_code += ({ x });
        ++i;
    } // while

} // get_init_code()

