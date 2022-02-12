constant NUM_ROWS = 25;
constant NUM_COLS = 40;
constant SPACE_CHAR = 32;
array(int) video_matrix;
mapping(string:int) char_codes = ([
    "a":1,"b":2,"c":3,"d":4,"e":5,"f":6,"g":7,"h":8,"i":9,"j":10,"k":11,
    "l":12,"m":13,"n":14,"o":15,"p":16,"q":17,"r":18,"s":19,"t":20,"u":21,
    "v":22,"w":23,"x":24,"y":25,"z":26,"&":38,"!":33,":":58,"%":69,"^":70,
    "*":71,"$":72,"+":73,"[":74,"=":75,"#":76,"1":49,"2":50,"3":51,"4":52
]);
int current_row;

int main()
{
    current_row = 0;
    video_matrix = allocate(NUM_ROWS*NUM_COLS, SPACE_CHAR);
    Stdio.File fin = Stdio.File("titles_layout.txt", "r");
    Iterator iter = fin->line_iterator();
    string myline;
    do
    {
        myline = iter->value();
        //myline = String.trim_all_whites(myline);
        //if (sizeof(myline))
        //{
            process_line(myline);
        //} // if
        ++current_row;
    } while (iter->next());
    fin->close();

    //display_matrix();
    write_binary();

    return 0;

} // main()

void write_cell(int r, int c, int value)
{
    video_matrix[r*NUM_COLS+c] = value;

} // write_cell()

void process_line(string line)
{
    //write("%d: %s\n", current_row, line);
    for (int i = 0; i < sizeof(line); ++i)
    {
        string char = line[i..i];
        int code;
        if (code = char_codes[char])
        {
            write_cell(current_row, i, code);
        } // if
    } // for

} // process_line()

void display_matrix()
{
    for (int r = 0; r < NUM_ROWS; ++r)
    {
        for (int c = 0; c < NUM_COLS; ++c)
        {
            write("%2d ", video_matrix[r*NUM_COLS+c]);
        } // for
        write("\n");
    } // for

} // display_matrix()

void write_binary()
{
    Stdio.File fout = Stdio.File("titles_layout.bin", "wct");
    fout->write("%s", (string) video_matrix);
    fout->close();

} // write_binary()

