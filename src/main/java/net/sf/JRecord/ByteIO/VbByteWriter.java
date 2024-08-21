/*
 * @Author Bruce Martin
 * Created on 18/03/2007
 *
 * Purpose: Write Mainframe / GnuCobol VB (variable length record files with 
 * the length preceding the data) files
 */
/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.ByteIO;

/**
 * Write Array of bytes to either a Mainframe VB file or a GNU Cobol VB file. A VB file consists of
 * 
 * {RDW 1}Data for the Line 1{RDW 1}Data for the Line...
 *
 *
 * Where RDW (Record Descriptor Word) is
 * 
 *  2 Byte Record Length (Big Endian)
 *  2 Bytes - hex zero.
 *
 * <b>Note:</b> Mainframe RDW includes the RDW length (4 bytes) GNU cobol's does not 
 * 
 * @author Bruce Martin
 *
 */
public class VbByteWriter extends BinaryByteWriter {

    /**
     *
     */
    public VbByteWriter() {
        super(true, true, null);
    }
    
    public VbByteWriter(boolean addRdwToLength) {
    	super(true, addRdwToLength, null);
    }
}
