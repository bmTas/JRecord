/*
 * @Author Bruce Martin
 * Created on 26/08/2005
 *
 * Purpose:  reading Record Orientated files
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

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.DefaultLineProvider;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LineProvider;
import net.sf.JRecord.Details.SpecialRecordIds;
import net.sf.JRecord.External.ExternalRecord;



/**
 * This abstract class is the base class for all <b>Line~Reader</b>
 * classes. A LineReader reads a file as a series of AbstractLines.
 *
 *<pre>
 *<b>Example:</b>
 * 
 *      {@code
 *      AbstractLineReader reader = JRecordInterface1.COBOL
 *              .newIOBuilder("file-name")
 *                  .setFileOrganization(Constants.IO_FIXED_LENGTH)
 *                  .setDialect(ICopybookDialects.FMT_FUJITSU)
 *              .newReader("Data-Filename");
 *              
 *      while ((l = reader.read()) != null) { ... }
 *      reader.closer()
 *      
 * }</pre> 
 * 
 * @author Bruce Martin
 *
 */
public abstract class AbstractLineReader implements IReadLine {

    public static final String NOT_OPEN_MESSAGE = "File has not been opened";
    
    private static final int RT_FIRST_RECORD = 1;
    private static final int RT_MIDDLE_RECORD = 2;
    private static final int RT_LAST_RECORD = 3;
    private static final int RT_FINISHED = 4;

	private LineProvider lineProvider;
	private LayoutDetail layout = null;
	private IReadLine filter = null;


	/**
	 * create Binary Line Reader
	 */
	public AbstractLineReader() {
	    this(new DefaultLineProvider());
	}


	/**
	 * create reader with a user sup[plied line provider. This allows
	 * you to use your own Classes that extend "Line"
	 *
	 * @param provider line provider
	 */
	public AbstractLineReader(final LineProvider provider) {
	    super();
	    lineProvider = provider;

	    if (provider == null) {
	    	lineProvider = new DefaultLineProvider();
	    }
	}

	/**
	 * Open a file where the layout can be built from the file contents.
	 * Possible files are:
	 * - XML files
	 * - CSV files where the field names are stored on the first line
	 * @param fileName file to be opened.
	 */
	 public void open(String fileName) throws IOException {
		 open(fileName, (LayoutDetail) null);
	 }
    /**
     * Open file for input
     *
     * @param fileName filename to be opened
     * @param pLayout record layout
     *
     * @throws IOException any IOerror
     */
    public void open(String fileName, LayoutDetail pLayout) throws IOException {
        open(new FileInputStream(fileName), pLayout);

        if (layout == null) {
            setLayout(pLayout);
        }
    }
    

    
    /**
     * Open a file using an external record Definition 
     * @param inputStream input
     * @param recordLayout recordlayout to use
     * @throws IOException any IOError that occurs
     */
    public void open(InputStream inputStream, ExternalRecord recordLayout) 
    throws IOException {
    	LayoutDetail pLayout = recordLayout.asLayoutDetail();
    	open(inputStream, pLayout);

        if (layout == null) {
        	setLayout(pLayout);
        }
    }


    /**
     * Open file for input
     *
     * @param inputStream input stream to be read
     * @param pLayout record layout
     *
     * @throws IOException any IOerror
     */
    public abstract void open(InputStream inputStream, LayoutDetail pLayout)
    throws IOException;



    /**
     * Read one line from the input file
     *
     * @return line read in
     *
     * @throws IOException io error
     */
    public final AbstractLine read() throws IOException {
    	if (filter == null) {
    		return readImplementation();
    	}
    	return filter.read();
    }

    /**
     * Read one line from the input file
     *
     * @return line read in
     *
     * @throws IOException io error
     */
    public abstract AbstractLine readImplementation() throws IOException;

    /**
     * Closes the file
     *
     * @throws IOException io error
     */
    public abstract void close() throws IOException;



	/**
	 * Read a complete buffers worth of data into buf from a input stream.
	 *
	 * @param in stream to be read.
	 * @param buf buffer to be loaded with data
	 *
	 * @return the number of bytes read
	 * @throws IOException IO Exception
	 */
	protected final int readBuffer(final InputStream in,
	        					   final byte[] buf)
				throws IOException {
	    int num;
	    int total;

	    num = in.read(buf);
	    total = num;

	    while (num >= 0 && total < buf.length) {
	        num = in.read(buf, total, buf.length - total);
	        total += num;
	    }

	    return total;
	}


	/**
	 * Create a Line using supplied details
	 *
	 * @param record record contents
	 *
	 * @return line just created
	 */
	@SuppressWarnings("deprecation")
	protected final AbstractLine getLine(byte[] record) {
	    AbstractLine ret = lineProvider.getLine(layout, record);

	    ret.setLineProvider(lineProvider);
	    return ret;
	}


	/**
	 * Create a Line using supplied details
	 *
	 * @param record record contents
	 *
	 * @return line just created
	 */
	@SuppressWarnings("deprecation")
	protected final AbstractLine getLine(String record) {
	    AbstractLine ret = lineProvider.getLine(layout, record);

	    ret.setLineProvider(lineProvider);
	    return ret;
	}


	/**
	 * get the record layout
	 * @return the layout
	 */
	public final LayoutDetail getLayout() {
	    return layout;
	}

	/**
	 * set the layout to be used
	 *
	 * @param pLayout layout to be used
	 */
    public final void setLayout(LayoutDetail pLayout) {
        this.layout = pLayout;
        
        filter = null;
        if (layout != null && layout.hasHeaderTrailerRecords()) {
        	filter = new HeaderTrailerDelagate(this, layout);
        }
    }


	/**
	 * @return the lineProvider
	 */
	public final LineProvider getLineProvider() {
		return lineProvider;
	}
	
	private static class HeaderTrailerDelagate implements IReadLine {
		private final AbstractLineReader parent;
		private int recordId = RT_FIRST_RECORD;
		private int firstRecordId = -1,
				    middleRecordId = -1,
				    lastRecordId = -1;
		private AbstractLine next = null;

		

		protected HeaderTrailerDelagate(AbstractLineReader parent, LayoutDetail l) {
			super();
			this.parent = parent;
			SpecialRecordIds sr = l.getPositionRecordId();
			firstRecordId = sr.headerId;
			middleRecordId = sr.middleId;
			lastRecordId = sr.trailerId;
		}



		/* (non-Javadoc)
		 * @see net.sf.JRecord.IO.IReadLine#read()
		 */
		@Override
		public AbstractLine read() throws IOException {
			AbstractLine ret = next;
			int id = -1;
			switch (recordId) {
			case RT_FINISHED:
			case RT_FIRST_RECORD:
				ret = parent.readImplementation();
				if (ret == null) {
					return null;
				}
				id = firstRecordId;

				recordId = RT_MIDDLE_RECORD;
				next = parent.readImplementation();
				if (next == null) {
					recordId = RT_LAST_RECORD;
				}
				break;
			case RT_MIDDLE_RECORD:
				id = middleRecordId; 
				next = parent.readImplementation();
				if (next == null) {
					recordId = RT_LAST_RECORD;
					id = lastRecordId;
				}
				break;
			case RT_LAST_RECORD:
				recordId = RT_FINISHED;
				return null;
			}
			if (id >= 0) {
				ret.setWriteLayout(id);
			}
			return ret;
		}

	}
}
