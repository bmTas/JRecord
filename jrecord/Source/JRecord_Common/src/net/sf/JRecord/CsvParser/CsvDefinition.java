/**
 *
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
      
package net.sf.JRecord.CsvParser;

import net.sf.JRecord.Common.Conversion;

/**
 * @author mum
 *
 */
public class CsvDefinition implements ICsvDefinition {

	private static final byte UNDEFINED = -121;
	private static final byte NO = 1;
	private static final byte YES = 2;

	private final String delimiter, quote, charset;
	private final int delimiterOrganisation, numberOfFields;
	private final boolean embeddedCR;

	private byte singleByteFont = UNDEFINED;



	public CsvDefinition(String delimiter, String quote) {
		this(delimiter, quote, ICsvDefinition.NORMAL_SPLIT, -1,"", false);
	}

	public CsvDefinition(String delimiter, String quote, boolean embeddedCR) {
		this(delimiter, quote, ICsvDefinition.NORMAL_SPLIT, -1,"", embeddedCR);
	}

	public CsvDefinition(String delimiter, String quote,
			int delimiterOrganisation, boolean embeddedCR) {
		this(delimiter, quote, delimiterOrganisation, -1, "", embeddedCR);
	}


	public CsvDefinition(String delimiter, String quote,
			int delimiterOrganisation, int numberOfFields, String charset, boolean embeddedCR) {
		super();
		this.delimiter = delimiter;
		this.quote = quote;
		this.delimiterOrganisation = delimiterOrganisation;
		this.numberOfFields = numberOfFields;
		this.charset = charset;
		this.embeddedCR = embeddedCR;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.CsvParser.ILineDetails#getDelimiter()
	 */
	@Override
	public String getDelimiter() {
		return delimiter;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.CsvParser.ICsvDefinition#getQuote()
	 */
	@Override
	public String getQuote() {
		return quote;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.CsvParser.ILineDetails#getDelimiterOrganisation()
	 */
	@Override
	public int getDelimiterOrganisation() {
		return delimiterOrganisation;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.CsvParser.ICsvDefinition#getFieldCount()
	 */
	@Override
	public int getFieldCount() {
		return numberOfFields;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.CsvParser.ICsvDefinition#getFontName()
	 */
	@Override
	public String getFontName() {
		return charset;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.CsvParser.ICsvDefinition#isSingleByteFont()
	 *
	 * Is not used currently (for CsvDefinition; should check font see RecordDetail
	 */
	@Override
	public boolean isSingleByteFont() {
		if (singleByteFont == UNDEFINED) {
			try {
				singleByteFont = YES;
				if (Conversion.isMultiByte(charset)) {
					singleByteFont = NO;
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		return singleByteFont == YES;
	}


	public boolean isEmbeddedNewLine() {
		return embeddedCR;
	}
}
