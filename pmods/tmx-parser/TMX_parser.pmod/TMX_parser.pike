/*
 TMX Parser
 Copyright 2013 Matthew Clarke

 This file is part of TMX Parser.

 TMX Parser is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 TMX Parser is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with TMX Parser.  If not, see <http://www.gnu.org/licenses/>.
*/


#pragma strict_types


// --------------------------------------------------
// PUBLIC DATA


// --------------------------------------------------
// PUBLIC METHODS
public .TMX_map get_map() 
{
    return my_map;

} // get_map()




// --------------------------------------------------
// PROTECTED DATA


// --------------------------------------------------
// PROTECTED METHODS
protected void create( string tmx_file )
{
    Parser.XML.Tree.Node root = Parser.XML.Tree.parse_file( tmx_file );
    root->walk_preorder( process_node );
    clean_up();

} // create()




// --------------------------------------------------
// PRIVATE DATA
private ADT.Stack tag_stack = ADT.Stack();
private .TMX_map my_map = .TMX_map();
private .TMX_map.Tileset current_tileset;
private .TMX_map.Layer current_layer;
private .TMX_map.Object_group current_object_group;
private .TMX_map.TMX_object current_tmx_object;


// --------------------------------------------------
// PRIVATE METHODS
private void process_node( Parser.XML.Tree.AbstractSimpleNode node )
{
    if ( node->get_node_type() == Parser.XML.Tree.XML_ELEMENT )
    {
        process_element( node );
    }
    else if ( node->get_node_type() == Parser.XML.Tree.XML_TEXT )
    {
        process_text( node );
    } // if

} // process_node()

// --------------------------------------------------

private void process_element( Parser.XML.Tree.AbstractSimpleNode node )
{
    string name = (string) node->get_tag_name();
    tag_stack->push( name );

    if ( name == "map" )
    {
        process_map( node );
    }
    else if ( name == "property" )
    {
        process_property( node );
    }
    else if ( name == "tileset" )
    {
        process_tileset( node );
    }
    else if ( name == "image" )
    {
        process_image( node );
    }
    else if ( name == "layer" )
    {
        process_layer( node );
    }
    else if ( name == "data" )
    {
        // Nothing of interest.
    }
    else if ( name == "objectgroup" )
    {
        process_objectgroup( node );
    }
    else if ( name == "object" )
    {
        process_object( node );
    }
    else if ( name == "polygon" || name == "polyline" )
    {
        process_polygon( node );
    } // if ... else

} // process_element()

// --------------------------------------------------

private void process_map( Parser.XML.Tree.AbstractSimpleNode node )
{
    mapping(string:string) attrs = (mapping(string:string))
            node->get_attributes();
    my_map->version = attrs["version"];
    my_map->orientation = attrs["orientation"];
    my_map->width = (int) attrs["width"];
    my_map->height = (int) attrs["height"];
    my_map->tile_width = (int) attrs["tilewidth"];
    my_map->tile_height = (int) attrs["tileheight"];

} // process_map()

// --------------------------------------------------

private void process_property( Parser.XML.Tree.AbstractSimpleNode node ) 
{
    // This is a leaf tag, so:
    tag_stack->pop();

    // Where does this property belong?  Parent element will be 
    // <properties>.  Element above that will tell us where to store it.
    // Properties can belong to: map, layer, objectgroup, object.
    string parent = get_parent_tag();

    mapping(string:string) attrs = (mapping(string:string)) 
            node->get_attributes();

    if ( parent == "map" )
    {
        my_map->properties[ attrs["name"] ] = attrs["value"];
    }
    else if ( parent == "layer" )
    {
        current_layer->properties[ attrs["name"] ] = attrs["value"];    
    }
    else if ( parent == "objectgroup" )
    {
        current_object_group->properties[ attrs["name"] ] = attrs["value"];
    }
    else if ( parent == "object" )
    {
        current_tmx_object->properties[ attrs["name"] ] = attrs["value"];
    }
    else
    {
        werror( "Unknown parent for <property>: %s\n", parent );
    } // if ... else

} // process_property()

// --------------------------------------------------

private void process_tileset( Parser.XML.Tree.AbstractSimpleNode node ) 
{
    // Does an existing tileset need to be added to the TMX_map?
    if ( current_tileset )
    {
        my_map->tilesets += ({ current_tileset });
    } // if
    
    current_tileset = .TMX_map.Tileset();
    mapping(string:string) attrs = (mapping(string:string)) 
            node->get_attributes();
    current_tileset->first_gid = (int) attrs["firstgid"];
    current_tileset->name = attrs["name"];
    current_tileset->tile_width = (int) attrs["tilewidth"];
    current_tileset->tile_height = (int) attrs["tileheight"];

} // process_tileset()

// --------------------------------------------------

private void process_image( Parser.XML.Tree.AbstractSimpleNode node ) 
{
    // This is a leaf node, so:
    tag_stack->pop();
    
    mapping(string:string) attrs = (mapping(string:string))
            node->get_attributes();

    // What parent node does this <image> belong to?
    string parent = (string) tag_stack->top();
    if ( parent == "tileset" )
    {
        current_tileset->image_source = attrs["source"];
        current_tileset->image_width  = (int) attrs["width"];
        current_tileset->image_height = (int) attrs["height"];
    }
    else
    {
        werror( "Unknown parent for <image>: %s\n", parent );
    } // if ... else
        
} // process_image()

// --------------------------------------------------

private void process_layer( Parser.XML.Tree.AbstractSimpleNode node )
{
    if ( current_layer )
    {
        my_map->layers += ({ current_layer });
    } // if

    current_layer = .TMX_map.Layer();
    mapping(string:string) attrs = (mapping(string:string)) 
            node->get_attributes();
    current_layer->name   = attrs["name"];
    current_layer->width  = (int) attrs["width"];
    current_layer->height = (int) attrs["height"];

} // process_layer()

// --------------------------------------------------

private void process_objectgroup( Parser.XML.Tree.AbstractSimpleNode node ) 
{
    if ( current_tmx_object )
    {
        current_object_group->tmx_objects += ({ current_tmx_object });
        // NOTE: delete TMX object so it doesn't carry over to next object
        // group (which may actually be empty).
        current_tmx_object = 0;
    } // if

    if ( current_object_group )
    {
        my_map->object_groups += ({ current_object_group });
    } // if

    current_object_group = .TMX_map.Object_group();
    mapping(string:string) attrs = (mapping(string:string)) 
            node->get_attributes();
    current_object_group->name   = attrs["name"];
    current_object_group->width  = (int) attrs["width"];
    current_object_group->height = (int) attrs["height"];

} // process_objectgroup()

// --------------------------------------------------

private void process_object( Parser.XML.Tree.AbstractSimpleNode node ) 
{
    // If we've only just entered a new objectgroup, don't want to process
    // a previous object.  It will already have been dealt with when the new
    // objectgroup was started.
    string parent = get_parent_tag();
    if (parent != "objectgroup")
    {
        if ( current_tmx_object )
        {
            current_object_group->tmx_objects += ({ current_tmx_object });
        } // if
    } // if

    current_tmx_object = .TMX_map.TMX_object();
    mapping(string:string) attrs = (mapping(string:string)) 
            node->get_attributes();
    current_tmx_object->name   = attrs["name"];
    current_tmx_object->type   = attrs["type"];
    current_tmx_object->x      = (int) attrs["x"];
    current_tmx_object->y      = (int) attrs["y"];

    current_tmx_object->x_float = (float) attrs["x"];
    current_tmx_object->y_float = (float) attrs["y"];

    current_tmx_object->width  = (int) attrs["width"];
    current_tmx_object->height = (int) attrs["height"];
    current_tmx_object->width_float = (float) attrs["width"];
    current_tmx_object->height_float = (float) attrs["height"];

} // process_object()

// --------------------------------------------------

private void process_text( Parser.XML.Tree.AbstractSimpleNode node )
{
    string text = (string) node->get_text();
    text = String.trim_all_whites( text );
    if ( sizeof(text) )
    {
        // Only text we should find is the layer data.
        tag_stack->pop();
        array(string) data_as_str = text / ",";
        array(int) data_as_int = ({});
        foreach ( data_as_str, string s )
        {
            data_as_int = (array(int)) Array.push( data_as_int, (int) s );
        } // foreach
        current_layer->data = data_as_int;
    } // if

} // process_text()

// --------------------------------------------------

private void clean_up()
{
    // Any cleaning up to do?  Make sure TMX_objects are handled before
    // Object_groups.
    if ( current_tmx_object )
    {
        current_object_group->tmx_objects += ({ current_tmx_object });
    } // if

    if ( current_object_group )
    {
        my_map->object_groups += ({ current_object_group });
    } // if

    if ( current_layer )
    {
        my_map->layers += ({ current_layer });
    } // if

    if ( current_tileset )
    {
        my_map->tilesets += ({ current_tileset });
    } // if

} // clean_up()

// --------------------------------------------------

private string get_parent_tag()
{
    string tmp = (string) tag_stack->pop();
    string parent = (string) tag_stack->top();
    tag_stack->push(tmp);
    return parent;

} // get_parent_tag()

// --------------------------------------------------

private void process_polygon( Parser.XML.Tree.AbstractSimpleNode node )
{
    //write("POLYGON!!!\n");
    // Polygon has one attribute: 'points'.
    mapping(string:string) attrs = (mapping(string:string)) 
            node->get_attributes();
    string s = attrs["points"];
    //write("s=%s\n", s);
    array(string) points = s / " ";
    //write("%O\n", points);
    array(float) poly_points = ({});
    foreach (points, string p)
    {
        array(string) parts = p / ",";
        poly_points += ({ (float) parts[0] });
        poly_points += ({ (float) parts[1] });
    } // foreach

    //write("%O\n", poly_points);
    current_tmx_object->poly_points = poly_points;

} // process_polygon()

// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------


