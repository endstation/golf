/*
 TMX_parser
 Copyright 2013 Matthew Clarke

 This file is part of TMX_parser.

 TMX_parser is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 TMX_parser is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with TMX_parser.  If not, see <http://www.gnu.org/licenses/>.
*/


#pragma strict_types


// --------------------------------------------------
// INNER CLASSES
class Tileset
{
    int    first_gid     = 0;
    string name          = "";
    int    tile_width    = 0;
    int    tile_height   = 0;
    string image_source  = "";
    int    image_width   = 0;
    int    image_height  = 0;

    string to_string()
    {
        string s = sprintf( "first_gid=%d\nname=%s\ntile_width=%d\n"
                "tile_height=%d\nimage_source=%s\nimage_width=%d\n"
                "image_height=%d\n", first_gid, name, tile_width, tile_height,
                image_source, image_width, image_height );
        return s;

    } // to_string()

} // class Tileset

// --------------------------------------------------

class Layer
{
    string                 name       = "";
    int                    width      = 0;
    int                    height     = 0;
    array(int)             data       = ({});
    mapping(string:string) properties = ([]);

    string to_string()
    {
        string s = sprintf( "name=%s\nwidth=%d\nheight=%d\ndata=%O\n"
                "properties=%O\n", name, width, height, data, properties );
        return s;

    } // to_string()

} // class Layer

// --------------------------------------------------

class Object_group
{
    string                 name        = "";
    int                    width       = 0;
    int                    height      = 0;
    mapping(string:string) properties  = ([]);
    array(TMX_object)      tmx_objects = ({});

    string to_string()
    {
        string s = sprintf( "name=%s\nwidth=%d\nheight=%d\nproperties=%O\n"
                "tmx_objects=%O\n", name, width, height, properties,
                tmx_objects );
        return s;

    } // to_string()

} // class Object_group

// --------------------------------------------------

class TMX_object
{
    string                 name;
    string                 type;
    int                    x;
    int                    y;

    float                  x_float;
    float                  y_float;
    array(float)           poly_points = ({});

    int                    width;
    int                    height;
    float                  width_float;
    float                  height_float;

    mapping(string:string) properties = ([]);

    string to_string() 
    {
        string s = sprintf( "name=%s\ntype=%s\nx=%d\ny=%d\nwidth=%d\n"
                "height=%d\nproperties=%O\n", name, type, x, y, width, height,
                properties );
        return s;

    } // to_string()

} // class TMX_object




// --------------------------------------------------
// PUBLIC DATA
public string                 version;
public string                 orientation;
public int                    width;
public int                    height;
public int                    tile_width;
public int                    tile_height;
public mapping(string:string) properties    = ([]);
public array(Tileset)         tilesets      = ({});
public array(Layer)           layers        = ({});
public array(Object_group)    object_groups = ({});


// --------------------------------------------------
// PUBLIC METHODS
public void display_to_console()
{
    write( "version=%s\n", version );
    write( "orientation=%s\n", orientation );
    write( "width=%d\n", width );
    write( "height=%d\n", height );
    write( "tile_width=%d\n", tile_width );
    write( "tile_height=%d\n", tile_height );
    write( "properties=%O\n", properties );
    write( "TILESETS:\n" );
    foreach( tilesets, Tileset t )
    {
        write( "%s\n", t->to_string() );
    } // foreach
    write( "LAYERS:\n" );
    foreach( layers, Layer l )
    {
        write( "%s\n", l->to_string() );
    } // foreach
    write( "OBJECT GROUPS:\n" );
    foreach( object_groups, Object_group og )
    {
        write( "%s\n", og->to_string() );
    } // foreach

} // display_to_console()




// --------------------------------------------------
// PROTECTED DATA


// --------------------------------------------------
// PROTECTED METHODS
protected void create()
{
    // Your code here!

} // create()




// --------------------------------------------------
// PRIVATE DATA


// --------------------------------------------------
// PRIVATE METHODS



