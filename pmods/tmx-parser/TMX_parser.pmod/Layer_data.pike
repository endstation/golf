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


// A helper class.  Represents a TMX tile layer as an array of ints (as stored
// in a TMX_map object).  Provides interface to read values from array given a
// 2D index.  


#pragma strict_types


// --------------------------------------------------
// PUBLIC DATA


// --------------------------------------------------
// PUBLIC METHODS
public int read( int col, int row )
{
    return m_data[ (row * m_width) + col ];

} // read()

// --------------------------------------------------

public void write(int col, int row, int value)
{
    m_data[(row * m_width) + col] = value;

} // write()

// --------------------------------------------------

public int get_width()  { return m_width;  }
public int get_height() { return m_height; }

// --------------------------------------------------

public array(int) get_array_copy()
{
    return copy_value( m_data );

} // get_array_copy()




// --------------------------------------------------
// PROTECTED DATA


// --------------------------------------------------
// PROTECTED METHODS
protected void create( array(int) data, int width )
{
    m_data = data;
    m_width  = width;
    m_height = sizeof(data) / width;

} // create()




// --------------------------------------------------
// PRIVATE DATA
private array(int) m_data = ({});
private int m_width;
private int m_height;


// --------------------------------------------------
// PRIVATE METHODS



