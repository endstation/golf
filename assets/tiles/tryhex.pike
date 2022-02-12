int main(int argc, array(string) argv)
{
    string data = Image.load_file("b01.bin");

    data[0] = 0x01;
    data[1] = 0x02;
    write("%s", data);

    return 0;

} // main()

