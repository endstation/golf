constant BASE = 0xe000;

int main()
{
    for (int i = 17, j = 0; i < 23; ++i, ++j)
    {
        write("%d: %04x\n", j, BASE+(i*320)+(4*8));
    } // for

    return 0;

} // main()

