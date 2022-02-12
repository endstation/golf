array TEST_MASKS = ({ 0xc0, 0x30, 0x0c, 0x03 });
array MASK_OUT   = ({ 0x3f, 0xcf, 0xf3, 0xfc });

int main()
{
    string data = Image.load_file("tree_tiles_class00.bin");
    //write("%s\n", data);
    //write("len? %d\n", sizeof(data));
    int len = sizeof(data);

    for (int i = 0; i < len; ++i)
    {
        int byte = (int) data[i];
        int mask = 0xff;
        for (int j = 0; j < 4; ++j)
        {
            if ((byte&0xff) & TEST_MASKS[j])
            {
                mask &= MASK_OUT[j];
            } // if
        } // for
        write("%02x:  %02x -> %02x    [%08b -> %08b]\n", i, byte, mask, byte, mask);
    } // for

    return 0;

} // main()

