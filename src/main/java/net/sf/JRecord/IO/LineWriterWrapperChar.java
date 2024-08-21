/*
 * @Author Bruce Martin
 * Created on 19/03/2007
 *
 * Purpose:
 */
/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
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

package net.sf.JRecord.IO;

import java.io.IOException;
import java.io.OutputStream;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.charIO.CharIOProvider;
import net.sf.JRecord.charIO.ICharWriter;

/**
 * This class creates a <b>LineWriter</b> from a Low Level <b>Byte-Writer</b>.
 * A ByteWriter writes a Byte Array to a File as a Line
 *
 * There a Byte Writers for <ol compact>
 *   <li>Fixed Line Length files
 *   <li>Length Based lines
 * </ol>
 *
 * @author Bruce Martin
 *
 */
public class LineWriterWrapperChar extends AbstractLineWriter {

	private static final CharIOProvider ioProvider = new CharIOProvider();
	
    private ICharWriter writer = null;
    private final int fileStructure;
    private OutputStream outputStream = null;

    /**
     *
     */
    public LineWriterWrapperChar(int fileStructure) {
        super();
        this.fileStructure = fileStructure;
    }

    /**
     * @see net.sf.JRecord.IO.AbstractLineWriter#open(java.io.OutputStream)
     */
    public void open(OutputStream outputStream) throws IOException {
    	this.outputStream = outputStream;
    }

    /**
     * @see net.sf.JRecord.IO.AbstractLineWriter#write(net.sf.JRecord.Details.AbstractLine)
     */
    public void write(AbstractLine line) throws IOException {
    	if (writer == null) {
    		LayoutDetail layout = line.getLayout();
			writer = ioProvider.getWriter(
					fileStructure, 
					layout.getFontName(), layout.getEolString(), layout.getMaximumRecordLength());
    		writer.open(outputStream);
    	}
        writer.write(line.getFullLine());
    }


    /**
     * @see net.sf.JRecord.IO.AbstractLineWriter#close()
     */
    public void close() throws IOException {
    	if (writer != null) {
    		writer.close();
    	} 
    	if (outputStream != null) {
    		outputStream.close();
    	}
    	writer = null;
    	outputStream = null;
    }
}
