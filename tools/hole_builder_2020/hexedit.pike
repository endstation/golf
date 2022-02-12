int main(int argc, array(string) argv)
{
    string filename = argv[1];
    string data = Image.load_file(filename);
    write("size=%d\n", sizeof(data));

    data[0] = 0x88;
    data[1] = 0x8c;

    object fout = Stdio.File(filename, "wct");
    fout->write("%s", data);
    fout->close();

    return 0;

} // main()

