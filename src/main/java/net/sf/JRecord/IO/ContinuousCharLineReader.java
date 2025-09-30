/*
 * Purpose: Record oriented reading of Binary files
 *
 * @Author Bruce Martin
 * Created on 27/08/2005
 *
 * # Version 0.60 Bruce Martin 2007/02/16
 *   - Started work on seperating Record section out, so removing
 *     all reference to the Common module and used a new Constants
 *     module
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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LineProvider;


/**
 * This class provides record oriented reading of a binary File
 * using the <b>Line</b> class to decide the record length.
 *
 * @author Bruce Martin
 *
 */
public class ContinuousCharLineReader extends AbstractLineReader {


	private static final int BUFFER_SIZE = 16384;

// 	private BufferedInputStream stream = null;
 	private BufferedReader reader;
	//private LayoutDetail recordLayout;

	private int maxSize;
	private int lineNumber = 0;
	long totalCharactersRead=0;

	private char[] buffer;

	protected int[] lengths;
	private AbstractLine tmpLine;
	
	private ICalculateLineLength calculateLineLength;


	//private ArrayList lineBuffer = new ArrayList();

//	private int lineNumber = 0;



	/**
	 * This class provides record oriented reading of Binary files.
	 * It uses a 'Line' to decide the length of each record
	 */
	public ContinuousCharLineReader() {
	    super();
	}


	/**
	 * This class provides record oriented reading of Binary files.
	 * It uses a 'Line' to decide the length of each record.
	 * It also uses a Line-Provider to create the lines.
	 *
	 * @param provider line provider
	 */
	public ContinuousCharLineReader(final LineProvider provider) {
	    super(provider);
	}


	public void setCalculateLineLength(ICalculateLineLength calculateLineLength) {
		this.calculateLineLength = calculateLineLength;
	}


    /**
     * @throws UnsupportedEncodingException 
     * @see net.sf.JRecord.IO.AbstractLineReader#open(java.io.InputStream, net.sf.JRecord.Details.LayoutDetail)
     */
    public void open(InputStream inputStream, LayoutDetail layout) throws UnsupportedEncodingException {

        int i;

        setLayout(layout);
        
        if (calculateLineLength == null) {
        	calculateLineLength = new CalculateLineLength("chars");
        }


		lengths = new int[layout.getRecordCount()];

		maxSize = 0;

		for (i = 0; i < lengths.length; i++) {
			lengths[i] = layout.getRecord(i).getLength();

			maxSize = java.lang.Math.max(maxSize, lengths[i]);
		}
        reader = new BufferedReader(
        		new InputStreamReader(inputStream, layout.getFontName()), 
        		Math.max(BUFFER_SIZE, maxSize+1));
		
		tmpLine = getLine("");

		buffer = new char[maxSize];
    }



    /**
     * @see net.sf.JRecord.IO.AbstractLineReader#read()
     */
    public AbstractLine readImplementation() throws IOException {
        AbstractLine ret = null;
        int recordSize;

        if (reader == null) {
            throw new IOException(AbstractLineReader.NOT_OPEN_MESSAGE);
        }
        lineNumber += 1;

        reader.mark(maxSize+1);
        int charsInBuffer = 0;
        
        charsInBuffer = readChars(buffer);

        if (charsInBuffer <= 0) {
            return null;
        }
        char[] rec = buffer;
        if (charsInBuffer < buffer.length) {
        	charsInBuffer += 1;
        	rec = new char[charsInBuffer];
        	System.arraycopy(buffer, 0, rec, 0, charsInBuffer);
        }
        
        tmpLine.setData(new String(rec));
        calculateLineLength.setFilePosition(lineNumber, totalCharactersRead);
        recordSize = calculateLineLength.findLength(tmpLine, charsInBuffer);
        reader.reset();
        rec = new char[recordSize];

        readChars(rec);
        ret = getLine(new String(rec));

        totalCharactersRead += recordSize;
        return ret;
    }


	private int readChars(char[] rec) throws IOException {
		int charsRead, charsInBuffer = 0, charsToRead = rec.length;
		while (charsInBuffer <= charsToRead && (charsRead = reader.read(rec, charsInBuffer, charsToRead - charsInBuffer)) > 0) {
        	charsInBuffer += charsRead;
		}

		return charsInBuffer;
	}



    /**
     * @see net.sf.JRecord.IO.AbstractLineReader#close()
     */
    public void close() throws IOException {

    	reader.close();
        buffer = null;
        reader = null;
    }

}
