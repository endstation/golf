constant N = 15;
constant BASE_DIR = "/home/matthew/hp2_progs/programming/6510assembly/commodore_golf/assets/trees";

int main()
{
    for (int i = 0; i < N; ++i)
    {
        string png_file = sprintf("%s/tree_tiles_class%02d.png", BASE_DIR, i);
        write("%s\n", png_file);
        string cmd = sprintf("%s %s tree_tiles_class%02d.bin tree_masks_class%02d.bin",
            "tile_generator.pike", png_file, i, i); 
        //write("cmd=%s\n", cmd);
        Process.run(cmd);

        // Move .bin files into assets/trees.
        string file0 = sprintf("tree_tiles_class%02d.bin", i);
        string file1 = sprintf("tree_masks_class%02d.bin", i);
        string dest = sprintf("%s/%s", BASE_DIR, file0);
        int s = mv(file0, dest);
        write("OK? = %d\n", s);
        dest = sprintf("%s/%s", BASE_DIR, file1);
        s = mv(file1, dest);
        write("OK? = %d\n", s);
    } // for

    return 0;

} // main()

