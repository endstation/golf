constant PIXELS_PER_YARD = 21.0;
constant CY = 64.0;
constant CZ = -110.0;
constant PZ = -200.0;
// This is tree height: 20ft (*7=140).
constant TH = 6.0*7;
// Add this to projected Y-position.
constant PLATE_DY = 64.0;
// i.e. 150yds.
constant MAX_VISIBLE_Z = 200*21.0;

int main()
{
    int step = 5*21;
    float oz = 0.0;

    while (oz <= MAX_VISIBLE_Z)
    {
        write("oz=%f\n", oz);
        
        // Tree base.
        float py = 0.0 + ((oz - PZ) * (CY - 0.0) / (oz - CZ));
        float tbase = py + PLATE_DY;
        // Tree top.
        py = TH + ((oz - PZ) * (CY - TH) / (oz - CZ));
        float ttop = py + PLATE_DY;

        write("Distance=%fyds\n", (oz/PIXELS_PER_YARD));
        write("base-y=%f\n", tbase);
        write(" top-y=%f\n", ttop);
        write("-->overall height=%f (%d)\n\n\n", (tbase-ttop),
            (int) ((tbase-ttop)/8));
        /*
        write("%f,%f\n", (oz/PIXELS_PER_YARD), (tbase-ttop));
        */

        oz += step;
    } // while

    return 0;

} // main()

