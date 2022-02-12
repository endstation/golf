constant SRC_DIR = "./";
array(string) all_results = ({});
mapping(string:int) hex_vals = ([
    "0":0,"1":1,"2":2,"3":3,"4":4,"5":5,"6":6,"7":7,"8":8,"9":9,
    "a":10,"b":11,"c":12,"d":13,"e":14,"f":15
]);
//mapping(string:int) mod_sizes = ([]);
array(mapping(string:int|string)) mod_sizes = ({});

int main()
{
    //Regexp.PCRE._pcre regex = Regexp.PCRE._pcre(".+\\.asm$");
    Regexp.PCRE._pcre regex = Regexp.PCRE._pcre( "=\\$([a-zA-Z0-9]+)" );

    //array all_files = get_dir(SRC_DIR);
    //array asm_files = filter(all_files, 
    //    lambda(string s) { return has_suffix(s, ".asm"); });
    //foreach (asm_files, string f) {
    //    write("%s\n", f);
    //} // foreach

    string path = sprintf("%s/labels.txt", SRC_DIR);
    Stdio.File fin = Stdio.File(path, "r");
    Iterator iter = fin->line_iterator();
    do {
        string s = iter->value();
        //write("%s\n", s);
        int i = search(s, "_c_SIZE  =$");
        if (i >= 0) {
            all_results += ({ s });
        } // if
    } while (iter->next());

    process_results(regex);
    display();

    return 0;

} // main()

void process_results(object regex)
{
    foreach (all_results, string s)
    {
        array a = regex->exec(s);
        int bytes = hex2dec( s[ a[2]..(a[3]-1) ] );
        int i = search(s, "_c_SIZE");
        string module_name = s[0..(i-1)];
        //write("%s [%s -> %d]\n", s, module_name, bytes);
        mapping m = ([ "name":module_name, "size":bytes ]);
        mod_sizes += ({ m });
    } // foreach

} // process_results()

int hex2dec(string s)
{
    s = lower_case(s);
    int n = sizeof(s);
    int col = 0;
    int value = 0;
    for (int i = (n-1); i >= 0; --i)
    {
        string digit = s[i..i];
        value += (hex_vals[digit] * pow(16, col));
        ++col;
    } // for
    return value;

} // hex2dec()

void display()
{
    array a = Array.sort_array(mod_sizes,
        lambda(mapping m1, mapping m2) { return m1["size"] < m2["size"]; });
    int total_bytes = 0;
    foreach (a, mapping m)
    {
        write("%10s [ %5d   %fkb]\n", m["name"], m["size"], m["size"]/1024.0);
        total_bytes += m["size"];
    } // foreach

    /*
    foreach ( indices(mod_sizes), string key )
    {
        write("%10s [ %5d   %fk]\n", key, mod_sizes[key], mod_sizes[key]/1024.0);
    } // foreach

    int total_bytes = Array.sum( values(mod_sizes) );
    */
    write("TOTAL: %5d   %fkb\n", total_bytes, total_bytes/1024.0);

} // display()





