/*
 * @author Bruce Martin
 * Created on 26/08/2005
 *
 * Purpose: Writing Record Orientated files
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
      
package net.sf.JRecord.charIO;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.Writer;


/**
 * This abstract class is the base class for all <b>Byte~Writer</b>
 * classes
 *
 * @author Bruce Martin
 *
 */
public abstract class BaseCharWriter implements ICharWriter {

    public static final String NOT_OPEN_MESSAGE = "File has not been opened";

	protected Writer w = null;
	

    /**
     * Open file for input
     *
     * @param fileName filename to be opened
     *
     * @throws IOException any IOerror
     */
    public void open(String fileName) throws IOException {
        open(new FileOutputStream(fileName)); 
    }


    /**
     * Read one line from the input file
     *
     * @param bytes line to write to the output file
     *
     * @throws IOException any IOerror
     */
    public void write(String s) throws IOException {
    	write(s.toCharArray());
    }


    /**
     * Closes the file
     *
     * @throws IOException any IOerror
     */
    public void close() throws IOException {
    	if (w != null) {
    		w.close();
    		w = null;
    	}
    }
}
