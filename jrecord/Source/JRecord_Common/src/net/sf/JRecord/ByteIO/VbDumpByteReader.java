/*
 * @Author Bruce Martin
 * Created on 18/03/2007
 *
 * Purpose:
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
 * ByteReader for Mainframe VB File that also include Block descriptor Word
 * i.e. The file consists of
 * 
 *   [Block Descriptor - 4 bytes containing the 2 byte block Length]
 *       [Line Descriptor - 4 bytes containing the 2 byte block Length] Line Data
 *           ................................
 *       [Line Descriptor - 4 bytes containing the 2 byte block Length] Line Data
 *   [Block Descriptor - 4 bytes containing the 2 byte block Length]
 *       [Line Descriptor - 4 bytes containing the 2 byte block Length] Line Data
 *           ................................
 *       [Line Descriptor - 4 bytes containing the 2 byte block Length] Line Data
 *     ..............
 *     
 *       
 *       
 * @author Bruce Martin
 *
 */
public class VbDumpByteReader extends VbByteReader {

    /**
     * ByteReader for Mainframe VB File that also include Block descriptor Word
     */
    public VbDumpByteReader() {
        super(true);
    }
}
