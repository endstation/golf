import HexString;

constant MAX_NAME_LEN = 10;
constant SPACE_CHAR = 32;
int dest_addr;

int main(int argc, array(string) argv)
{
    dest_addr = get_dest_addr(argv[1]);
    array(string) all_files = filter(get_dir(), lambda(string s) {
        return has_prefix(s, "best") && has_suffix(s, ".txt"); });
    //write("all_files=%O\n", all_files);

    foreach (all_files, string f)
    {
        process_file(f);
    } // foreach

    return 0;

} // main()

int get_dest_addr(string line)
{
    // NOTE: assume 4-digit hex number.
    int i = search(line, "$");
    string addr_str = line[i+1 .. i+4];
    return hex_str_to_int(addr_str);

} // get_dest_addr()

void process_file(string filename)
{
    // Filename is of form 'bestXX.txt', where XX is course ID, counting
    // form 0.  Output file has same name but change suffix to '.bin'.
    //string prg_file = sprintf("%sbin", filename[0..6]);
    string prg_file = sprintf("%sprg", filename[0..6]);

    object fin = Stdio.File(filename, "r");
    object iter = fin->line_iterator();
    string s1 = String.trim_all_whites( iter->value() );
    iter->next();
    string s2 = String.trim_all_whites( iter->value() );
    fin->close();

    array(int) all_data = allocate(33, SPACE_CHAR);
    array names = s1 / ",";
    array scores = s2 / ",";
    for (int i = 0; i < sizeof(names); ++i)
    {
        int base = i*10;
        for (int j = 0; j < sizeof(names[i]); ++j)
        {
            all_data[base+j] = names[i][j];
        } // for
    } // for

    // Now the scores:
    for (int i = 0; i < sizeof(scores); ++i)
    {
        all_data[30+i] = (int) scores[i];
    } // for
    
    all_data = ({ dest_addr % 256, dest_addr / 256 }) + all_data;

    // Write data to bin file.
    object fout = Stdio.File(prg_file, "wct");
    fout->write("%s", (string) all_data);
    fout->close();

} // process_file()





