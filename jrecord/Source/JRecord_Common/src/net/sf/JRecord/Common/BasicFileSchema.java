/**
 * A minimal <i>File Schema</i>, it is basically for testing Reader's / Writers
 * int RecordEditor / JRecord
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
      
package net.sf.JRecord.Common;

/**
 * 
 * A basicSchema class for use in testing
 * 
 * @author Bruce Martin
 *
 */
public class BasicFileSchema implements IBasicFileSchema {

	public static int FT_BINARY_FILE = 2;
	public static int FT_BINARY_CSV_FILE = 3;
	public static int FT_OTHER = 4;
	
	private final int fileStructure, maximumRecordLength, layoutType;
	private final String charset, delimiter, quote;
	
	
	/**
	 * Basic Csv schema, used for Testing JRecord
	 * 
	 * @param fileStructure file structure (or organisation)
	 * @param binary wether it is a "Binary Csv" i.e. has a separator like 0x00
	 * @param charset character set
	 * @param delimiter field delimiter
	 * @param quote quote to use
	 * 
	 * @return requested basic schema
	 */
	public static BasicFileSchema newCsvSchema(int fileStructure, boolean binary, String charset, String delimiter, String quote) {
		int layoutType = FT_OTHER;
		if (binary) {
			layoutType = FT_BINARY_CSV_FILE;
		}
		return new BasicFileSchema(fileStructure, Integer.MAX_VALUE, layoutType, charset, delimiter, quote);
	}
	
	/**
	 * Create a basic FileSchema ((record-length=80, font="") - basically used in Testing JRecord)
	 * @param fileStructure File-Structure (or File-Organization) of the schema
	 * @return requested basic schema
	 */
	public static BasicFileSchema newFixedSchema(int fileStructure) {
		return newFixedSchema(fileStructure, true, 80, "");
	}
	
	/**
	 * Define basic fixed width schema
	 * 
	 * @param fileStructure  file structure (or organisation)
	 * @param binary is it a binary file ???
	 * @param recordLength record length3
	 * @param charset character set
	 * 
	 * @return requested basic schema
	 */
	public static BasicFileSchema newFixedSchema(int fileStructure, boolean binary, int recordLength, String charset) {
		int layoutType = FT_OTHER;
		if (binary) {
			layoutType = FT_BINARY_FILE;
		}
		return new BasicFileSchema(fileStructure, recordLength, layoutType, charset, "", "");
	}
	
	protected BasicFileSchema(int fileStructure, int maximumRecordLength,
			int layoutType, String charset, String delimiter, String quote) {
		super();
		this.fileStructure = fileStructure;
		this.maximumRecordLength = maximumRecordLength;
		this.layoutType = layoutType;
		this.charset = charset;
		this.delimiter = delimiter;
		this.quote = quote;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Common.IBasicFileSchema#getFileStructure()
	 */
	@Override
	public int getFileStructure() {
		return fileStructure;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Common.IBasicFileSchema#isBinary()
	 */
	@Override
	public boolean isBinary() {
		return layoutType == FT_BINARY_FILE || layoutType == FT_BINARY_CSV_FILE;
	}

//	/* (non-Javadoc)
//	 * @see net.sf.JRecord.Common.IBasicFileSchema#isBinCSV()
//	 */
//	@Override
//	public boolean isBinCSV() {
//		return layoutType == FT_BINARY_CSV_FILE;
//	}
//
//	/* (non-Javadoc)
//	 * @see net.sf.JRecord.Common.IBasicFileSchema#getRecordSep()
//	 */
//	@Override
//	public byte[] getRecordSep() {
//		return Conversion.getBytes("\n", charset);
//	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Common.IBasicFileSchema#getFontName()
	 */
	@Override
	public String getFontName() {
		return charset;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Common.IBasicFileSchema#getDelimiter()
	 */
	@Override
	public String getDelimiter() {
		return delimiter;
	}

//	/* (non-Javadoc)
//	 * @see net.sf.JRecord.Common.IBasicFileSchema#getDelimiterBytes()
//	 */
//	@Override
//	public byte[] getDelimiterBytes() {
//		return Conversion.getBytes(delimiter, charset);
//	}
//
	/* (non-Javadoc)
	 * @see net.sf.JRecord.Common.IBasicFileSchema#getQuote()
	 */
	@Override
	public String getQuote() {
		return quote;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Common.IBasicFileSchema#getMaximumRecordLength()
	 */
	@Override
	public int getMaximumRecordLength() {
		return maximumRecordLength;
	}

}
