import HexString;

int main(int argc, array(string) argv)
{
    string dest = argv[1];
    int i = search(dest, "$");
    string addr_str = dest[i+1 .. i+4];
    write("addr_str=%s\n", addr_str);
    int myaddr = hex_str_to_int(addr_str);
    write("as int=%d\n", myaddr);
    return 0;

} // main()

