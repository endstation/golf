array(string) CLUB_STRS = ({
    "3W", "5W", "3I", "4I", "5I", "6I", "7I", "8I", "9I", "PW", "SW" });
array(int) vz_vals = ({});
array(int) vy_vals = ({});
array(float) speeds = ({});
array(float) launch_angles = ({});
array(float) new_vz_vals = ({});
array(float) new_vy_vals = ({});
array(int) new_vz_as_ints = ({});
array(int) new_vy_as_ints = ({});
string output_file = sprintf("punch_club_velocities.asm");
float angle_reduction;
float speed_reduction;

int main(int argc, array(string) argv)
{
    if (argc != 3) 
    {
        werror("Usage: pike punch_creator <speed reduction> <angle reduction>\n");
        werror("e.g. 0.75 = 3/4 speed; 0.66 = 2/3 normal angle");
        exit(1);
    } // if
    speed_reduction = (float) argv[1];
    angle_reduction = (float) argv[2];
    /*write("reduction=%f, angle=%f [%f]\n", speed_reduction, angle,
        Math.convert_angle(angle, "deg", "rad"));*/
    // Angle to radians, as required by sin & cos functions.
    //angle = Math.convert_angle(angle, "deg", "rad");

    object fin = Stdio.File("club_velocities.txt", "r");
    object iter = fin->line_iterator();
    int line_num = 0;

    do
    {
        string s = iter->value();
        if ( search(s, "#") < 0 )
        {
            //write("---> %s\n", s);
            array vals = s / ",";
            //write("vals=%O\n", vals);
            foreach (vals, string s2)
            {
                if (!line_num) {
                    vz_vals += ({ (int) s2 });
                }
                else {
                    vy_vals += ({ (int) s2 });
                } // if ... else
            } // foreach
            ++line_num;
        } // if
    } while (iter->next());

    fin->close();

    display_normal_launch_angles();

    //write("vz:\n%O\n", vz_vals);
    //write("vy:\n%O\n", vy_vals);

    // Calculate speed for each velocity.
    for (int i = 0; i < sizeof(vz_vals); ++i)
    {
        float vz = ((vz_vals[i] >> 8) & 0xff) + ((vz_vals[i] & 0xff) / 256.0);
        float vy = ((vy_vals[i] >> 8) & 0xff) + ((vy_vals[i] & 0xff) / 256.0);
        //write("%d: %f,%f\n", i, vz, vy);
        speeds += ({ sqrt( vz*vz + vy*vy ) });
    } // for

    //write("%O\n", speeds);

    // Now calculate the new values for each club based on speeds.
    for (int i = 0; i < sizeof(vz_vals); ++i)
    {
        float theta = launch_angles[i] * angle_reduction;
        float new_vz = (speeds[i]*speed_reduction) * cos(theta);
        float new_vy = (speeds[i]*speed_reduction) * sin(theta);
        new_vz_vals += ({ new_vz });
        new_vy_vals += ({ new_vy });
    } // for

    //write("new_vz:\n%O\n", new_vz_vals);
    //write("new_vy:\n%O\n", new_vy_vals);

    calc_new_ints();
    write_final_output();

    return 0;

} // main()

void calc_new_ints()
{
    // NOTE: put dummy value in slot 0 for driver.
    new_vz_as_ints += ({ 0 });
    new_vy_as_ints += ({ 0 });

    for (int i = 0; i < sizeof(new_vz_vals); ++i)
    {
        // Cast to integer to discard fractional part.
        int integer_part = (int) new_vz_vals[i];
        int fractional_part = (int) ((new_vz_vals[i] - integer_part) * 256); 
        //write("%d, %d\n", integer_part, fractional_part);
        new_vz_as_ints += ({ integer_part*256 + fractional_part });

        integer_part = (int) new_vy_vals[i];
        fractional_part = (int) ((new_vy_vals[i] - integer_part) * 256);
        //write("%d, %d\n", integer_part, fractional_part);
        new_vy_as_ints += ({ integer_part*256 + fractional_part });
    } // for

    // And dummy slot for putter.
    new_vz_as_ints += ({ 0 });
    new_vy_as_ints += ({ 0 });

} // calc_new_ints()

void write_final_output()
{
    object fout = Stdio.File(output_file, "wct");

    array str_arr = map( new_vz_as_ints, lambda(int x) {return sprintf("<%d",x);} );
    string s = str_arr * ",";
    fout->write("clubs_l_PUNCH_VZ_LO !byte %s\n", s);

    str_arr = map( new_vz_as_ints, lambda(int x) {return sprintf(">%d",x);} );
    s = str_arr * ",";
    fout->write("clubs_l_PUNCH_VZ_HI !byte %s\n", s);

    str_arr = map( new_vy_as_ints, lambda(int x) {return sprintf("<%d",x);} );
    s = str_arr * ",";
    fout->write("clubs_l_PUNCH_VY_LO !byte %s\n", s);

    str_arr = map( new_vy_as_ints, lambda(int x) {return sprintf(">%d",x);} );
    s = str_arr * ",";
    fout->write("clubs_l_PUNCH_VY_HI !byte %s\n", s);

    fout->close();

} // write_final_output()

void display_normal_launch_angles()
{
    for (int i = 0; i < sizeof(vz_vals); ++i)
    {
        float theta = atan( (vy_vals[i]*1.0) / vz_vals[i] );
        launch_angles += ({ theta });
        write("%s: %f [%f]\n", CLUB_STRS[i], Math.convert_angle(theta, "rad", "deg"),
            Math.convert_angle(theta*angle_reduction, "rad", "deg"));
    } // for

} // display_normal_launch_angles()



