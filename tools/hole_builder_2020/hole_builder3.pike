constant PIXELS_PER_YARD = 21;
constant CELL_SIZE = 10;
constant MAX_TREES = 80;
constant MAX_QUADS = 25;
//enum { TREE_BY_BUNKER, TREE_BY_WATER };
enum { TREE_BY_WATER=1, TREE_BY_BUNKER=3 };
constant BOUNDARY_MARGIN = 25*PIXELS_PER_YARD;
constant LOAD_ADDRESS = 0xe000;

// NOTE: -1 because center of sprite is offset (2,2) from top-left.
constant OVERHEAD_OFFSET_X = 35*8+24-1;
constant OVERHEAD_OFFSET_Y = 17*8+50-1;
int tee_x_offset;
int tee_y_offset;

// As measured in C64 pixels.
float map_w_px;
float map_h_px;
// Tiled 'units'.  From now on, these are constant.
constant MAP_W_TMX = 32;
constant MAP_H_TMX = 48;
// OUTPUT.
// Size of outer array is number of quads.
// Inner arrays are all of length 9.
array(array(int)) quads = ({});
float tee_x;
float tee_z;
float target_x;
float target_z;
mapping(string:int) TYPE_MAPPING =
    ([ "fairway":2, "green":2, "bunker":1, "water":3 ]);
array(string) quads_x_lo = ({});
array(string) quads_x_hi = ({});
array(string) quads_z_lo = ({});
array(string) quads_z_hi = ({});
array(int) quads_type = ({});
array(int) quads_triangle_index = ({});
array(int) quads_n_per_hole = ({});
string file_out;
int par;
int distance_yds;
array(array(int)) waypoints = ({});
array(array(int)) trees = ({});

int current_triangle_index = 0;
array(int) triangles_vertices_to_omit = ({});
array(int) triangles_slope = ({});
array(int) triangles_z_intercept = ({});

int topmost_vertex = 0;
int leftmost_vertex = Int.NATIVE_MAX;
int rightmost_vertex = 0;

float yards_per_cell;
float pixels_per_cell;
//int hazard_type;


int main(int argc, array(string) argv)
{
    if (argc != 3)
    {
        werror("Usage: pike hole_builder.pike <input.tmx> <output.bin>\n");
        exit(1);
    } // if

    string file_in = argv[1];
    file_out = argv[2];
    TMX_parser.TMX_map mymap = TMX_parser.TMX_parser(file_in)->get_map();

    find_scale(mymap);

    map_w_px = MAP_W_TMX * pixels_per_cell;
    map_h_px = MAP_H_TMX * pixels_per_cell;
    write("in pixels: %f*%f\n", map_w_px, map_h_px);

    record_tee_and_target(mymap);
    //string hztype = mymap->properties["hazard_type"];
    //hazard_type = ((hztype == "water") ? TREE_BY_WATER : TREE_BY_BUNKER);
    //write("tee @ (%d,%d)\n", tee_x, tee_z);
    //write("target @ (%d,%d)\n", target_x, target_z);

    // The quads are stored in an object group.
    // There's only one so just take the first element of the array.
    foreach (mymap->object_groups, object og)
    {
        if (og->name == "quads")
        {
            foreach (og->tmx_objects, object tmxo)
            {
                process_quad(tmxo);
            } // foreach
        }
        else if (og->name == "waypoints")
        {
            foreach (og->tmx_objects, object tmxo)
            {
                process_waypoint(tmxo);
            } // foreach
        }
        else if (og->name == "trees")
        {
            process_trees(og);
        }
        else if (og->name == "triangles")
        {
            process_triangles(og);
        } // if ... else
    } // foreach
    add_final_waypoint();

    //object og = mymap->object_groups[0];
    //write("%s\n", og->name);
    //foreach (og->tmx_objects, object tmxo)
    //{
    //    process_quad(tmxo);
    //} // foreach

    /*
    for (int i = 0; i < sizeof(quads); ++i)
    {
        //write("%O\n", quads[i]);
        display_quad( quads[i] );
    } // for
    */

    quads_n_per_hole += ({ sizeof(quads) });

    //array arr = Array.sort_array( quads, quad_compare );
    quads = Array.sort_array( quads, quad_compare );
    //write("%O\n", arr);
    foreach (quads, array a)
    {
        //display_quad(a);
    } // foreach
    //display_output();
    
    write("num trees=%d\n", sizeof(trees));
    write("num quads=%d\n", sizeof(quads));

    if (sizeof(trees) > MAX_TREES)
    {
        werror("Too many trees! %d\n", sizeof(trees));
        return 1;
    }
    else if (sizeof(quads) > MAX_QUADS)
    {
        werror("Too many quads! %d\n", sizeof(quads));
        return 1;
    } // if ... else

    set_boundaries();
    write("before leftmost=%d\n", leftmost_vertex);
    output2();
    write("topmost_vertex=%d\n", topmost_vertex);
    write("leftmost=%d\n", leftmost_vertex);
    write("rightmost=%d\n", rightmost_vertex);

    return 0;

} // main()


void process_quad(object tmxo, void|int triangle_index)
{
    write("*** %s ***\n", tmxo->properties["type"]);
    write("%d,%d %d*%d\n", tmxo->x, tmxo->y, tmxo->width, tmxo->height);
    // Store vertices in format: x1,z1, x2,z2, x3,z3, x4,z4
    // 9th element is the quad type.
    // 10th is vertex to omit for triangles or (-1) if this isn't a triangle.
    array(int) vertices = allocate(10);

    // Find top-left vertex first.
    // NOTE: quad coords & dimensions are stored in terms of tile/cell size,
    // so need to divide by that first.
    int x = (int) ((tmxo->x * pixels_per_cell / CELL_SIZE) - tee_x);
    int z = (int) (((MAP_H_TMX - (tmxo->y / CELL_SIZE)) * pixels_per_cell)
            - tee_z);

    // What are the dimensions of the quad in C64 pixels?
    int w64 = (int) (tmxo->width * pixels_per_cell / CELL_SIZE);
    int h64 = (int) (tmxo->height * pixels_per_cell / CELL_SIZE);

    vertices[0] = x;
    vertices[1] = z;
    vertices[2] = x;
    vertices[3] = z - h64;
    vertices[4] = x + w64;
    vertices[5] = z - h64;
    vertices[6] = x + w64;
    vertices[7] = z;

    vertices[8] = TYPE_MAPPING[ tmxo->properties["type"] ];

    // Is this a triangle?!
    if (zero_type(triangle_index))
    {
        // Ceci n'est pas un triangle...
        // So negative 1 (i.e. 255).
        vertices[9] = 255;
    }
    else
    {
        vertices[9] = triangle_index;
    } // if ... else

    write("%O\n", vertices);
    quads += ({ vertices });

    if (z > topmost_vertex) { topmost_vertex = z; }
    if (x < leftmost_vertex) { leftmost_vertex = x; }
    if (vertices[4] > rightmost_vertex) { rightmost_vertex = vertices[4]; }

} // process_quad()

void record_tee_and_target(object the_map)
{
    // Tee position represents the ORIGIN of the world.
    int x = (int) the_map->properties["tee_x"];
    int z = (int) the_map->properties["tee_y"];

    tee_x_offset = x + OVERHEAD_OFFSET_X;
    tee_y_offset = z + OVERHEAD_OFFSET_Y;

    // Position in C64 pixels relative to bottom-left of map.
    tee_x = x * pixels_per_cell;
    tee_z = (MAP_H_TMX - z) * pixels_per_cell;

    x = (int) the_map->properties["target_x"];
    z = (int) the_map->properties["target_y"];
    target_x = (x * pixels_per_cell) - tee_x;
    target_z = ((MAP_H_TMX - z) * pixels_per_cell) - tee_z;

    par = (int) the_map->properties["par"];
    distance_yds = (int) (sqrt( target_x*target_x + target_z*target_z ));
    write("tee=(%f,%f)\n", tee_x, tee_z);
    write("target=(%f,%f)\n", target_x, target_z);
    write("distance_yds=%d\n", distance_yds);

} // record_tee_and_target()

void display_output()
{
    foreach (quads, array a)
    {
        string x_lo = sprintf("<%s%d%s",
            (a[0] < 0 ? "(" : ""), a[0], (a[0] < 0 ? ")" : ""));
        string x_hi = sprintf(">%s%d%s",
            (a[0] < 0 ? "(" : ""), a[0], (a[0] < 0 ? ")" : ""));
        string z_lo = sprintf("<%d", a[1]);
        string z_hi = sprintf(">%d", a[1]);
        quads_x_lo += ({ x_lo });
        quads_x_hi += ({ x_hi });
        quads_z_lo += ({ z_lo });
        quads_z_hi += ({ z_hi });

        x_lo = sprintf("<%s%d%s",
            (a[2] < 0 ? "(" : ""), a[2], (a[2] < 0 ? ")" : ""));
        x_hi = sprintf(">%s%d%s",
            (a[2] < 0 ? "(" : ""), a[2], (a[2] < 0 ? ")" : ""));
        z_lo = sprintf("<%d", a[3]);
        z_hi = sprintf(">%d", a[3]);
        quads_x_lo += ({ x_lo });
        quads_x_hi += ({ x_hi });
        quads_z_lo += ({ z_lo });
        quads_z_hi += ({ z_hi });

        x_lo = sprintf("<%s%d%s",
            (a[4] < 0 ? "(" : ""), a[4], (a[4] < 0 ? ")" : ""));
        x_hi = sprintf(">%s%d%s",
            (a[4] < 0 ? "(" : ""), a[4], (a[4] < 0 ? ")" : ""));
        z_lo = sprintf("<%d", a[5]);
        z_hi = sprintf(">%d", a[5]);
        quads_x_lo += ({ x_lo });
        quads_x_hi += ({ x_hi });
        quads_z_lo += ({ z_lo });
        quads_z_hi += ({ z_hi });

        x_lo = sprintf("<%s%d%s",
            (a[6] < 0 ? "(" : ""), a[6], (a[6] < 0 ? ")" : ""));
        x_hi = sprintf(">%s%d%s",
            (a[6] < 0 ? "(" : ""), a[6], (a[6] < 0 ? ")" : ""));
        z_lo = sprintf("<%d", a[7]);
        z_hi = sprintf(">%d", a[7]);
        quads_x_lo += ({ x_lo });
        quads_x_hi += ({ x_hi });
        quads_z_lo += ({ z_lo });
        quads_z_hi += ({ z_hi });

        quads_type += ({ a[8] });
        quads_triangle_index += ({ a[9] });
    } // foreach 

    write("quads_x_lo !byte ");
    foreach (quads_x_lo, string s)
    {
        write("%s,", s);
    } // foreach
    write("\n");
    write("quads_x_hi !byte ");
    foreach (quads_x_hi, string s) { write("%s,", s); }
    write("\n");
    write("quads_z_lo !byte ");
    foreach (quads_z_lo, string s) { write("%s,", s); }
    write("\n");
    write("quads_z_hi !byte ");
    foreach (quads_z_hi, string s) { write("%s,", s); }
    write("\n");
    write("quads_type !byte ");
    foreach (quads_type, int t) { write("%d,", t); }
    write("\n");
    write("quads_n_per_hole !byte ");
    foreach (quads_n_per_hole, int n) { write("%d,", n); }
    write("\n");

} // display_output()

// o1 and o2 are of type array(int).
// Second element (index=1) is the top-left z-position.
// Because we want DESCENDING order, reverse the normal operation of 'cmp'.
int quad_compare(object o1, object o2)
{
    int smaller = 0;
    smaller = o1[1] < o2[1];
    return smaller;

} // quad_compare()

void display_quad(array(int) arr)
{
    write("type: %d, triangle index: %d\n", arr[8], arr[9]);
    write("(%d,%d)  (%d,%d)  (%d,%d)  (%d,%d)\n\n", arr[0], arr[1], arr[2],
            arr[3], arr[4], arr[5], arr[6], arr[7]);

} // display_quad()

void output2()
{
    // Output binary file.
    Stdio.File fout = Stdio.File(file_out, "wct");

    // Load address (because this has to be a PRG file).
    fout->write("%s", (string) ({ LOAD_ADDRESS&0xff, (LOAD_ADDRESS>>8)&0xff }));

    // Target.  Assume x=0.
    //fout->write("%s", (string) ({0,0}));
    int tgtx = (int) target_x;
    int tgtz = (int) target_z;
    write("target=%d,%d\n", tgtx, tgtz);
    fout->write("%s", (string) ({ tgtx&0xff, (tgtx>>8)&0xff }));
    fout->write("%s", (string) ({ tgtz&0xff, (tgtz>>8)&0xff }));

    // Distance in pixels.
    fout->write("%s", (string) ({ distance_yds&0xff, (distance_yds>>8)&0xff }));

    // Width.
    //fout->write("%s", (string) ({ map_w_px&0xff, (map_w_px>>8)&0xff }));
    // Height.
    //fout->write("%s", (string) ({ map_h_px&0xff, (map_h_px>>8)&0xff }));
    // Boundaries (TOP,LEFT,RIGHT).
    fout->write("%s", (string) ({ topmost_vertex&0xff, (topmost_vertex>>8)&0xff }));
    // NOTE: LEFT boundary will always be negative.
    leftmost_vertex += 65536;
    fout->write("%s", (string) ({ leftmost_vertex&0xff, (leftmost_vertex>>8)&0xff }));
    fout->write("%s", (string) ({ rightmost_vertex&0xff, (rightmost_vertex>>8)&0xff }));

    // Par.
    fout->write("%s", (string) ({ par }));
    //fout->write("%s", (string) ({ hazard_type }));

    // Overhead map origin.
    fout->write("%s", (string) ({ tee_x_offset&0xff, (tee_x_offset>>8)&0xff })); 
    fout->write("%s", (string) ({ tee_y_offset })); 
    // Scale - one pixel of overhead map : world pixels.
    int scale_as_int = (int) pixels_per_cell;
    fout->write("%s", (string) ({ scale_as_int&0xff, (scale_as_int>>8)&0xff }));

    // Number of quads.
    fout->write("%s", (string) ({ sizeof(quads) }));

    // Now the quads themselves.  Must add 2^16 to any negative values before
    // splitting them into low/high bytes.
    for (int i = 0; i < sizeof(quads); ++i)
    {
        // First the type.
        fout->write("%s", (string) ({quads[i][8]}));
        // Then triangle index.
        fout->write("%s", (string) ({quads[i][9]}));

        // For each vertex, four bytes: x-lo, x-hi, z-lo, z-hi.
        // Vertex A.
        int x = quads[i][0];
        int z = quads[i][1];
        if (x < 0) { x += 65536; }
        if (z < 0) { z += 65536; }
        fout->write("%s", (string) ({ x&0xff,(x>>8)&0xff,z&0xff,(z>>8)&0xff }));

        // Vertex B.
        x = quads[i][2];
        z = quads[i][3];
        if (x < 0) { x += 65536; }
        if (z < 0) { z += 65536; }
        fout->write("%s", (string) ({ x&0xff,(x>>8)&0xff,z&0xff,(z>>8)&0xff }));

        // Vertex C.
        x = quads[i][4];
        z = quads[i][5];
        if (x < 0) { x += 65536; }
        if (z < 0) { z += 65536; }
        fout->write("%s", (string) ({ x&0xff,(x>>8)&0xff,z&0xff,(z>>8)&0xff }));

        // Vertex D.
        x = quads[i][6];
        z = quads[i][7];
        if (x < 0) { x += 65536; }
        if (z < 0) { z += 65536; }
        fout->write("%s", (string) ({ x&0xff,(x>>8)&0xff,z&0xff,(z>>8)&0xff }));
    } // for

    // Number of triangles and then list the 'vertices to omit' values.
    fout->write("%s", (string) ({ sizeof(triangles_vertices_to_omit) }));
    for (int i = 0; i < sizeof(triangles_vertices_to_omit); ++i)
    {
        int v = triangles_vertices_to_omit[i];
        fout->write("%s", (string) ({ v }));
        // Now the slope.
        // Record in low/high byte format.
        int slope = triangles_slope[i];
        //write("write slope to file: %d,%d\n", (slope>>8)&0xff, slope&0xff);
        fout->write("%s", (string) ({ slope&0xff, (slope>>8)&0xff }));
        // And z-intercept...
        int z_intercept = triangles_z_intercept[i];
        fout->write("%s", (string) ({ z_intercept&0xff, (z_intercept>>8)&0xff }));
    } // for

    // Number of waypoints.
    fout->write("%s", (string) ({ sizeof(waypoints) }) );
    for (int i = 0; i < sizeof(waypoints); ++i)
    {
        array(int) buf = waypoints[i];
        for (int i = 0; i < sizeof(buf); ++i)
        {
            int value = buf[i];
            if (value < 0) { value += 65536; }
            fout->write("%s", (string) ({ value&0xff,(value>>8)&0xff }));
        } // for
    } // for

    // Number of trees.
    fout->write("%s", (string) ({ sizeof(trees) }));
    for (int i = 0; i < sizeof(trees); ++i)
    {
        array(int) buf = trees[i];
        //for (int i = 0; i < sizeof(buf); ++i)
        for (int i = 0; i < 2; ++i)
        {
            int value = buf[i];
            if (value < 0) { value += 65536; }
            fout->write("%s", (string) ({ value&0xff,(value>>8)&0xff }));
        } // for
        //fout->write("%s", (string) ({ buf[2] }));
    } // for

    fout->close();

} // output2()

void process_waypoint(object tmxo)
{
    array(int) buffer = allocate(6);
    int x = (int) ((tmxo->x * pixels_per_cell / CELL_SIZE) - tee_x);
    int z = (int) (((MAP_H_TMX - (tmxo->y / CELL_SIZE)) *
            pixels_per_cell) - tee_z);
    int w = (int) (tmxo->width * pixels_per_cell / CELL_SIZE);
    int h = (int) (tmxo->height * pixels_per_cell / CELL_SIZE);
    buffer[0] = x;
    buffer[1] = z;
    buffer[2] = x + w;
    buffer[3] = z - h;

    // Actual waypoint.
    // NOTE: special case where both wpx and wpz are 0 (meaning no rotation).
    int wpx = (int) tmxo->properties["wpx"];
    int wpz = (int) tmxo->properties["wpz"];
    if (wpx || wpz)
    {
        wpx = (int) ((wpx * pixels_per_cell) - tee_x);
        wpz = (int) (((MAP_H_TMX - wpz) * pixels_per_cell) - tee_z);
    } // if
    buffer[4] = wpx;
    buffer[5] = wpz;

    waypoints += ({ buffer });
    write("waypoint:\n");
    write("%O\n", buffer);

} // process_waypoint()

void add_final_waypoint()
{
    array(int) buffer = allocate(6, 0);
    // 'Dummy' box of all 0's (already set).
    // Actual point is same as target.
    buffer[4] = (int) target_x;
    buffer[5] = (int) target_z;
    waypoints += ({ buffer });
    write("waypoint:\n");
    write("%O\n", buffer);

} // add_final_waypoint()

void process_trees(object tree_group)
{
    foreach (tree_group->tmx_objects, object tmxo)
    {
        int x = tmxo->x;
        int z = tmxo->y;
        x = (int) ((x * pixels_per_cell / CELL_SIZE) - tee_x);
        z = (int) (((MAP_H_TMX - (z / CELL_SIZE)) * pixels_per_cell) - tee_z);

        // If there's a 'type' property, this tree should be recorded as 
        // being by WATER; else by BUNKER.
        //int tree_location = TREE_BY_BUNKER;
        //if ( !zero_type(tmxo->properties["type"]) )
        //{
        //    write("tree by water!\n");
        //    tree_location = TREE_BY_WATER;
        //} // if

        array(int) coords = ({ x, z/*, tree_location*/ });
        trees += ({ coords });
        write("tree:\n");
        write("%O\n", coords);
    } // foreach

} // process_trees()

void process_triangles(object triangles_group)
{
    foreach (triangles_group->tmx_objects, object tmxo)
    {
        write("triangle index=%d\n", current_triangle_index);
        // Store as a quad but with one extra piece of information: index
        // into 'triangles' table...
        int vertex_to_omit = (int) tmxo->properties["omit"];
        write("TRIANGLE: vertex_to_omit=%d\n", vertex_to_omit); 
        process_quad(tmxo, current_triangle_index);
        triangles_vertices_to_omit += ({ vertex_to_omit });

        // Calculate and store the slope.
        // It must be negative if omitting vertex #1 or #3.
        float mf = tmxo->height_float / tmxo->width_float;
        write("mf=%f\n", mf);
        // Break down into integer and fractional parts.
        int whole = (int) mf;
        int fractional = (int) ((mf - whole) * 256);
        int slope = (whole<<8) + fractional;
        triangles_slope += ({ slope });
        write("slope=%d\n", slope);
        int z_intercept = 0;
        if (vertex_to_omit == 1 || vertex_to_omit == 3)
        {
            write("height=%f\n", tmxo->height_float);
            z_intercept = (int) (tmxo->height_float / CELL_SIZE * pixels_per_cell);
        } // if
        triangles_z_intercept += ({ z_intercept });
        write("z-intercept=%d\n", z_intercept);

        ++current_triangle_index;
    } // foreach

} // process_triangles()

void set_boundaries()
{
    topmost_vertex += BOUNDARY_MARGIN;
    leftmost_vertex -= BOUNDARY_MARGIN;
    rightmost_vertex += BOUNDARY_MARGIN;

} // set_boundaries()

void find_scale(object mymap)
{
    float tee_x = (float) mymap->properties["tee_x"];
    float tee_y = (float) mymap->properties["tee_y"];
    float target_x = (float) mymap->properties["target_x"];
    float target_y = (float) mymap->properties["target_y"];

    float dx = abs(tee_x - target_x);
    float dy = abs(tee_y - target_y);
    float hyp = sqrt(dx*dx + dy*dy);

    // hyp tells us how many cells there are from the tee to the target.
    // Divide distance in yards by this value (hyp) to find out how many
    // yards each cell should represent.
    yards_per_cell = ((float) mymap->properties["yards"]) / hyp;
    pixels_per_cell = yards_per_cell * PIXELS_PER_YARD;
    write("yards_per_cell=%f\n", yards_per_cell);
    write("pixels_per_cell=%f\n", pixels_per_cell);

} // find_scale()


