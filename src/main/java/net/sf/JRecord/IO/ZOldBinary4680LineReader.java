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

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

import net.sf.JRecord.Common.Constants;
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
public class ZOldBinary4680LineReader extends AbstractLineReader {


	private static final int BUFFER_SIZE = 16384;

 	private BufferedInputStream stream = null;
	//private LayoutDetail recordLayout;

	private int maxSize;

	private byte[] buffer;

	private byte[] recSep;

	private int[] lengths;
	private int[] lengthsSorted;
	private AbstractLine tmpLine;

	//private ArrayList lineBuffer = new ArrayList();

//	private int lineNumber = 0;



	/**
	 * This class provides record oriented reading of Binary files.
	 * It uses a 'Line' to decide the length of each record
	 */
	public ZOldBinary4680LineReader() {
	    super();
	}


	/**
	 * This class provides record oriented reading of Binary files.
	 * It uses a 'Line' to decide the length of each record.
	 * It also uses a Line-Provider to create the lines.
	 *
	 * @param provider line provider
	 */
	public ZOldBinary4680LineReader(final LineProvider provider) {
	    super(provider);
	}


    /**
     * @see net.sf.JRecord.IO.AbstractLineReader#open(java.io.InputStream, net.sf.JRecord.Details.LayoutDetail)
     */
    public void open(InputStream inputStream, LayoutDetail layout) {

        int i;

        stream = new BufferedInputStream(inputStream, BUFFER_SIZE);
        setLayout(layout);

		lengthsSorted = new int[layout.getRecordCount()];
		lengths = new int[lengthsSorted.length];

		recSep = layout.getRecordSep();
		maxSize = 0;

		for (i = 0; i < lengthsSorted.length; i++) {
			lengthsSorted[i] = layout.getRecord(i).getLength();
			lengths[i] = layout.getRecord(i).getLength();

			maxSize = java.lang.Math.max(maxSize, lengthsSorted[i]);
		}

		tmpLine = getLine(new byte[maxSize]);

		Arrays.sort(lengthsSorted);

		//lineBuffer.clear();

		buffer = new byte[maxSize];
    }



    /**
     * @see net.sf.JRecord.IO.AbstractLineReader#read()
     */
    public AbstractLine readImplementation() throws IOException {
        AbstractLine ret = null;
        int recordSize, bytesRead;
        byte[] rec;

        if (stream == null) {
            throw new IOException(AbstractLineReader.NOT_OPEN_MESSAGE);
        }

        stream.mark(maxSize);
        bytesRead = readBuffer(stream, buffer);

        if (bytesRead <= 0) {
            return null;
        }
        if (bytesRead < buffer.length) {
            bytesRead += 1;
        }
        recordSize = findLength(bytesRead);
        stream.reset();
        rec = new byte[recordSize];

        readBuffer(stream, rec);
        ret = getLine(rec);


        return ret;
    }


	/**
	 * work out the record length
	 *
	 * @param maxLength length of the buffer
	 *
	 * @return the length of the next length
	 */
	private int findLength(int maxLength) {
		int i, rl, pref;
		int recLen = 1;
		int start = 0;
		boolean search;

		search = true;
//		lineNumber += 1;
//		//System.out.print("Line " + lineNumber + " " + start);

		tmpLine.replace(buffer, start, maxLength);
		pref = tmpLine.getPreferredLayoutIdx();
		if (pref != Constants.NULL_INTEGER) {
		    //System.out.print(" pref " + pref);
		    rl = lengths[pref];
		    if (isEqual(buffer, start + rl + 1)) {
		        recLen = rl;
		        search = false;
		        //System.out.print("  found 1 " + recLen + " " + maxLength);
		    }
		}

		i = 0;
		while (search && (i < lengthsSorted.length)) {
		    if ((start + lengthsSorted[i] < maxLength)
		    && isEqual(buffer, start + lengthsSorted[i] + 1)) {
		        search = false;
		        recLen = lengthsSorted[i];
		        //System.out.print("  found 2 " + recLen + " " + maxLength);
		    } else {
		        i += 1;
		    }
		}

		if (search) {
		    recLen = maxLength;
		    if (recSep.length > 0) {
		        for (i = recSep.length; (i <= maxLength && recLen == maxLength); i++) {
		        	if (isEqual(buffer, start + i)) {
		        	    recLen = i - 1;
		        	    break;
		        	}
		        }
		    }

		    //System.out.print("  found 3 " + recLen + " " + maxLength);
		}

		//System.out.println();
		return recLen;
	}



	/**
	 * compares part of rec with the record seperator string
	 *
	 * @param rec record details
	 * @param fin end of the logical record
	 *
	 * @return wether the record ends with the record seporator
	 */
	private boolean isEqual(final byte[] rec, final int fin) {
		int i;
		int st = fin - recSep.length - 1;
		boolean ret = st >= 0;

		for (i = 0; ret && (i < recSep.length); i++) {
			ret = (recSep[i] == rec[st + i]);
		}

		return ret;
	}



    /**
     * @see net.sf.JRecord.IO.AbstractLineReader#close()
     */
    public void close() throws IOException {

        stream.close();
        buffer = null;
        stream = null;
    }

}
