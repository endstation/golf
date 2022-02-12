constant BASE = 0xe000;

int main(int argc, array(string) argv)
{
    int x = ((int) argv[1]) & 0xff;
    int y = ((int) argv[2]) & 0xff;
    

    int byte = BASE + ((y/8)*40*8) + (y%8) + ((x/4)*8); 
    int byte2 = BASE + ((y & 248) * 40) + (y & 7) + (2 * x & 504);

    write("byte=%d [$%0x]\n", byte, byte);
    write("byte2=%d [$%0x]\n", byte2, byte2);

    return 0;

} // main()

