array velocities = ({
    8600, 8300, 7950, 7600, 7000, 6550, 6100, 5750, 5700, 5575, 5200, 4100 });
array angles = ({
    10.0, 9.5, 9.5, 11.0, 12.0, 12.0, 13.5, 14.0, 15.5, 15.2, 18.0, 18.5 });
array vzs = ({});
array vys = ({});

int main(int argc, array(string) argv)
{
    for (int i = 0; i < sizeof(velocities); ++i)
    {
        float v = (float) velocities[i];
        float a = angles[i];
        a = Math.convert_angle(a, "deg", "rad");
        int vz = (int) (v * cos(a));
        int vy = (int) (v * sin(a));
        write("%d,%d\n", vz, vy);
        vzs += ({ vz });
        vys += ({ vy });
    } // for
    // Add putter!
    vzs += ({ 1480 });
    vys += ({ 0 });

    object fout = Stdio.File("trajectories.asm", "wct");
    fout->write("clubs_l_VZ_LO\n");
    foreach (vzs, int x) { fout->write("    !byte <%d\n", x); }
    fout->write("clubs_l_VZ_HI\n");
    foreach (vzs, int x) { fout->write("    !byte >%d\n", x); }
    fout->write("clubs_l_VY_LO\n");
    foreach (vys, int x) { fout->write("    !byte <%d\n", x); }
    fout->write("clubs_l_VY_HI\n");
    foreach (vys, int x) { fout->write("    !byte >%d\n", x); }
    fout->close();

    /*
    float hyp = (float) argv[1];
    float theta = (float) argv[2];
    theta = Math.convert_angle(theta, "deg", "rad");

    int vz = (int) (hyp * cos(theta));
    int vy = (int) (hyp * sin(theta));
    write("%d,%d\n", vz, vy);

    string path = "/home/matthew/programming/6510assembly/golfgit/commodore-golf/src/clubv.bin";
    object fout = Stdio.File(path, "wct");
    array a = allocate(54, 0);
    a[0]    = 0xd6;
    a[1]    = 0x3a;
    a[2]    = vz&0xff;
    a[2+13] = (vz&0xff00)>>8;
    a[2+26] = vy&0xff;
    a[2+39] = (vy&0xff00)>>8;
    fout->write("%s", (string) a);
    fout->close();
    */

    return 0;

} // main()

