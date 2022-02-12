constant CAMZ = 76.0;
constant CAMY = 32.0;
constant PPZ = 200.0; // i.e. projection plane
constant FUDGE_DELTA = 96.0;

int main()
{
    array(int) heights = ({ 144, 128, 64, 48 });
    array(int) distances = ({ 82, 164, 328, 656, 1312, 2625, 5250 });

    for (int i = 0; i < sizeof(heights); ++i)
    {
        write("HEIGHT=%d\n", heights[i]);
        for (int j = 0; j < sizeof(distances); ++j)
        {
            int oz = distances[j];
            int oy = 0;
            float y_base = oy + ((PPZ + oz) * (CAMY - oy)) / (CAMZ + oz);
            y_base += FUDGE_DELTA;
            oy = heights[i];
            float y_top = oy + ((PPZ + oz) * (CAMY - oy)) / (CAMZ + oz);
            y_top += FUDGE_DELTA;
            write("  DISTANCE=%d -> %f\n", distances[j], (y_base - y_top));
        } // for
    } // for

    return 0;

} // main()

