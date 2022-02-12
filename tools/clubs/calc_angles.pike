array(float) power_increase = ({
    1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 });
enum { DR, W3, W5, I3, I4, I5, I6, I7, I8, I9, PW, SW };
array names = ({ "DR","3W","5W","3I","4I","5I","6I","7I","8I","9I","PW","SW" });
array vz;
array vy;
array angles = ({ 9.0,10.0,11.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,25.0,32.0 });
array angles_rad;
array vz_new;
array vy_new;

int main(int argc, array(string) argv)
{
    string club = argv[1];
    float powinc = (float) argv[2];
    int club_i = search(names, club);
    //write("%s, %f, %i\n", club, powinc, club_i);
    write("%s %f %d\n", club, powinc, club_i);

    angles_rad = map(angles, lambda(float x) { return Math.convert_angle(x,"deg","rad"); });
    write("angles_rad=%O\n", angles_rad);
    object fin = Stdio.File("club_velocities2.txt", "r");
    object iter = fin->line_iterator();

    string vz_str = String.trim_all_whites( iter->value() );
    iter->next();
    string vy_str = String.trim_all_whites( iter->value() );
    fin->close();

    write("%s\n%s\n", vz_str, vy_str);
    vz = map( vz_str/",", lambda(string x) { return (float) x; });
    vy = map( vy_str/",", lambda(string x) { return (float) x; });
    write("vz=%O\n", vz);
    write("vy=%O\n", vy);

    fin = Stdio.File("club_velocities_evolved.txt", "r");
    iter = fin->line_iterator();
    vz_str = String.trim_all_whites( iter->value() );
    iter->next();
    vy_str = String.trim_all_whites( iter->value() );
    fin->close();
    vz_new = map( vz_str/",", lambda(string x) { return (int) x; });
    vy_new = map( vy_str/",", lambda(string x) { return (int) x; });

    //for (int i = 0; i < sizeof(vz); ++i)
    //{
    //    float theta = atan( vy[i] / vz[i] );
    //    write("-> %f\n", Math.convert_angle(theta, "rad", "deg"));
    //} // for

    power_up(club_i, powinc);
    write_evolved();

    return 0;

} // main()

void power_up(int club_i, float power)
{
    // Hypotenuse:
    float hyp = sqrt(vz[club_i]*vz[club_i] + vy[club_i]*vy[club_i]);
    hyp *= power;
    float myvz = hyp * cos(angles_rad[club_i]);
    float myvy = hyp * sin(angles_rad[club_i]);
    write("%d,%d [%f,%f] -> [%04x,%04x]\n", (int)myvz, (int)myvy, myvz, myvy, (int)myvz, (int)myvy);
    write("angle=%f\n", Math.convert_angle(atan(myvy/myvz),"rad","deg"));
    write("original=%f,%f\n", vz[club_i], vy[club_i]);

    // Write values to the 'evolved' array.
    vz_new[club_i] = (int) myvz;
    vy_new[club_i] = (int) myvy;

} // power_up()

void write_evolved()
{
    // Text & binary files.
    string txtfile = "club_velocities_evolved.txt";
    string binfile = "club_velocities_evolved.bin";

    // Text:
    object fout = Stdio.File(txtfile, "wct");
    fout->write("%s\n", join(vz_new));
    fout->write("%s\n", join(vy_new));
    fout->close();

    // Now binary:
    array myarr = allocate(54);
    // Load address first.
    myarr[0] = 0xb5;
    myarr[1] = 0x3a;
    for (int i = 0; i < sizeof(vz_new); ++i)
    {
        int vz = vz_new[i];
        int vy = vy_new[i];
        myarr[ 2+i ]        = vz&0xff;          // vz-lo
        myarr[ 2+13+i ]     = (vz&0xff00)>>8;   // vz-hi
        myarr[ 2+2*13+i ]   = vy&0xff;          // vy-lo      
        myarr[ 2+3*13+i ]   = (vy&0xff00)>>8;   // vy-hi
    } // for
    fout = Stdio.File(binfile, "wct");
    fout->write("%s", (string) myarr);
    fout->close();

    string dest = "/home/matthew/hp2_progs/programming/6510assembly/golfgit/commodore-golf/src/newclubs.bin";
    int result = Stdio.cp(binfile, dest);
    write("result=%d\n", result);

} // write_evolved()

string join(array arr)
{
    int n = sizeof(arr);
    string s = "";
    for (int i = 0; i < n; ++i)
    {
        string sep = (i < (n-1)) ? "," : "";
        s += sprintf("%d%s", arr[i], sep);
    } // for
    return s;

} // join()

