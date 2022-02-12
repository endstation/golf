constant BASE_DIR = "/home/matthew/hp2_progs/programming/6510assembly/golfgit/commodore-golf";
constant SRC_DIR = BASE_DIR + "/src";

int main()
{
    string path = sprintf("%s/core/zeropage_template.txt", SRC_DIR);
    string outfile = sprintf("%s/core/zeropage.asm", SRC_DIR);
    //write("path=%s\n", path);
    object fout = Stdio.File(outfile, "wct");
    object fin = Stdio.File(path, "r");
    object iter = fin->line_iterator();
    int counter = 2;

    do
    {
        string s = iter->value();
        string strimmed = String.trim_all_whites(s);
        int n = sizeof(strimmed);
        int is_const = search(strimmed, ";");
        if (!n || !is_const)
        {
            fout->write("%s\n", s);
        }
        else
        {
            fout->write("%s = $%02x\n", strimmed, counter);
            /*
            if (strimmed == "ZP_LOADER_01")
            {
                // This is piped into the 'build_loader' script.
                write("%d", counter);
            } // if
            */
            ++counter;
        } // if ... else
        //write("--> %s\n", s);
    } while (iter->next());

    fout->write("ZP_END = $%02x\n", counter);

    fin->close();
    fout->close();

    return 0;

} // main()

