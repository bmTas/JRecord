/*
 * @Author Bruce Martin
 * Created on 26/08/2005
 *
 * Purpose: Writing Record Orientated files
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

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;


/**
 * This abstract class is the base class for all <b>Line~Writer</b>
 * classes. A LineWriter writes a series of AbstractLines to an external file.
 *
 * <pre>
 * <b>Usage:</b>
 * 
 *         CopybookLoader loader = <font color="brown"><b>new</b></font> RecordEditorXmlLoader();
 *         LayoutDetail layout = loader.loadCopyBook(copybookName, 0, 0, "", 0, 0, <font color="brown"><b>null</b></font>).asLayoutDetail();
 *        
 *         <b>AbstractLineWriter</b> writer = LineIOProvider.getInstance().getLineWriter(layout.getFileStructure());
 * </pre>
 *
 * @author Bruce Martin
 *
 */
public abstract class AbstractLineWriter {

    public static final String NOT_OPEN_MESSAGE = "File has not been opened";


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
     * Open file for input
     *
     * @param outputStream input stream to write
     *
     * @throws IOException any IOerror
     */
    public abstract void open(OutputStream outputStream)
    throws IOException;


    /**
     * Read one line from the input file
     *
     * @param line line to write to the output file
     *
     * @throws IOException any IOerror
     */
    public abstract void write(AbstractLine line) throws IOException;


    /**
     * Closes the file
     *
     * @throws IOException any IOerror
     */
    public abstract void close() throws IOException;

    /**
     * Set the Record Layout
     * @param layout record layout to set
     */
    public void setLayout(LayoutDetail layout) {

    }
}
