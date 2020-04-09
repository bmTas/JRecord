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

import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.Types.Type;

/**
 * CSV line parser which will look for matching Quotes
 *
 *
 * @author Bruce Martin
 *
 */
public class StandardCsvLineParser extends BaseCsvLineParser  {

	private boolean textFieldsInQuotes = false;
	/**
	 * Standard CSV line parser
	 */
	public StandardCsvLineParser() {
		super(false, false);
	}

	/**
	 * Standard CSV line parser
	 * @param putTextFieldsInQuotes put Quotes around Text Fields
	 */
	public StandardCsvLineParser(boolean putTextFieldsInQuotes) {
		super(false, false);
		textFieldsInQuotes = putTextFieldsInQuotes;
	}


	/**
	 * Standard CSV line parser
	 * @param putTextFieldsInQuotes put Quotes around Text Fields
	 */
	public StandardCsvLineParser(boolean putTextFieldsInQuotes, boolean quoteInColumnNames) {
		this(putTextFieldsInQuotes, quoteInColumnNames, false);
	}

	public StandardCsvLineParser(boolean putTextFieldsInQuotes, boolean quoteInColumnNames, boolean imbeddedCr) {
		super(quoteInColumnNames, imbeddedCr);
		textFieldsInQuotes = putTextFieldsInQuotes;
	}


	/**
	 * @see net.sf.JRecord.CsvParser.ICsvCharLineParser#getField(int, String, ICsvDefinition)
	 */
	public String getField(int fieldNumber, String line, ICsvDefinition lineDef) {
		String[] lineVals = split(fieldNumber, line, lineDef);
		String ret = lineVals[1];
		if (ret == null) {
			ret = "";
		}
		return ret;
	}

	
	
	@Override
	public List<String> getFieldList(String line, ICsvDefinition csvDefinition) {
		ArrayList<String> fields = new ArrayList<String>();
		String[] lineVals;
		lineVals = split(0, line, csvDefinition);
		while (! "".equals(lineVals[2])) {
			fields.add(lineVals[1]);
			lineVals = split(1, lineVals[2], csvDefinition);
		}
		fields.add(lineVals[1]);
		return fields;
	}

	/**
	 *
	 */
	public String getField2(int fieldNumber, String line, ICsvDefinition lineDef) {
		String[] lineVals = split(fieldNumber, line, lineDef);
		return lineVals[1];
	}

	/**
	 * @see net.sf.JRecord.CsvParser.ICsvCharLineParser#setField(int, int, String, ICsvDefinition, String)
	 */
	public String setField(int fieldNumber, int fieldType, String line, ICsvDefinition lineDef, String newValue) {
		String[] lineVals = split(fieldNumber, line, lineDef);
		String s = newValue;
		if (newValue == null) {
			s = "";
		}
		String delimiter = super.getDelimFromCsvDef(lineDef);
		String quote = lineDef.getQuoteDefinition().asString();
		int quoteLength = 1;
		if (quote != null && quote.length() > 0) {
			quoteLength = quote.length();
		}

//		if (textFieldsInQuotes) {
//			System.out.print("--> " + fieldType + " " + Type.NT_NUMBER + " " + s + " --> ");
//		}
		if (quote != null && quote.length() > 0
		&& (	s.indexOf(delimiter) >= 0 || s.indexOf('\n') >= 0 || s.indexOf('\r') >= 0
			||  s.indexOf(quote) >= 0
			|| (textFieldsInQuotes && (fieldType != Type.NT_NUMBER)))) {
			StringBuffer b = new StringBuffer(s);
			int pos;
			int i = 0;

			while ((pos = b.indexOf(quote, i)) >= 0) {
				b.insert(pos, quote);
				i = pos + quoteLength * 2;
			}
			s = quote + b.toString() + quote;
		}
//		if (textFieldsInQuotes) {
//			System.out.println(s);
//		}
		return lineVals[0] + s + lineVals[2];
	}

    protected String formatField(String s, int fieldType, ICsvDefinition lineDef) {
		if (s == null) {
			s = "";
		} else {
			String delimiter = super.getDelimFromCsvDef(lineDef);
			String quote = lineDef.getQuoteDefinition().asString();
			int quoteLength = 1;
			if (quote != null && quote.length() > 0) {
				quoteLength = quote.length();
			}
	
	//		if (textFieldsInQuotes) {
	//			System.out.print("--> " + fieldType + " " + Type.NT_NUMBER + " " + s + " --> ");
	//		}
			if (quote != null && quote.length() > 0
			&& (	s.indexOf(delimiter) >= 0 || s.indexOf('\n') >= 0 || s.indexOf('\r') >= 0
				||  s.indexOf(quote) >= 0
				|| (textFieldsInQuotes & (fieldType != Type.NT_NUMBER)))) {
				StringBuffer b = new StringBuffer(s);
				int pos;
				int i = 0;
	
				while ((pos = b.indexOf(quote, i)) >= 0) {
					b.insert(pos, quote);
					i = pos + quoteLength * 2;
				}
				s = quote + b.toString() + quote;
			}
		}
		return s;
    }
	/**
	 * Split the line into 3<ol compact>
	 * <li>Line before the request field</li>
	 * <li>The Requested field</li>
	 * <li>Line after the request field</li>
	 * </ol>
	 *
	 * @param fieldNumber  field to retrieve
	 * @param line line to parse for fields
	 * @param lineDef Csv Definition
	 * @return Array containing line before the field, The requested field, The line after
	 * the request field.
	 */
	private String[] split(int fieldNumber, String line, ICsvDefinition lineDef) {
		String[] ret = new String[]{null, null, ""};
		StringBuilder pre   = new StringBuilder("");
		StringBuilder field = null;
		String s, sCh;
		boolean inQuotes = false;
		boolean lastCharDelim = true;
		boolean lastCharQuote = false;
		int currFieldNumber = 0;
		int i = 0;
		String delimiter = super.getDelimFromCsvDef(lineDef);
		String quote = lineDef.getQuoteDefinition().asString();
		int quoteLength = 1;
		if (quote != null && quote.length() > 0) {
			quoteLength = quote.length();
		}

		while (i < line.length() && currFieldNumber < fieldNumber) {
			s = line.substring(i, Math.min(i + quoteLength, line.length()));
			sCh = line.substring(i, i + 1);
			//pre.append(sCh);
			if (s.equals(quote)) {
				if (lastCharDelim) {
					inQuotes = true;
					lastCharQuote = false;
				} else  {
					lastCharQuote = ! lastCharQuote;
				}

				i += quoteLength;
				pre.append(quote);
				lastCharDelim = false;
			} else {
				pre.append(sCh);
				lastCharDelim = false;

				if (sCh.equals(delimiter)
				&& ((! inQuotes) || lastCharQuote)) {
					lastCharDelim = true;
				 	currFieldNumber += 1;
				 	lastCharQuote = false;
				 	inQuotes = false;
				}
				i += 1;
			}
		}

		if (i < line.length()) {
			field = new StringBuilder("");
			lastCharDelim = true;
			while (i < line.length()) {
				s = line.substring(i, Math.min(i + quoteLength, line.length()));
				sCh = line.substring(i, i + 1);

				//System.out.println("~ :" + quote + ": " + field + " ->" + s + "<- " + inQuotes
				//		+ " " + lastCharQuote + " " + lastCharDelim);

				if (sCh.equals(delimiter)
				&& ((! inQuotes) || (lastCharQuote))) {
					break;
				} else if (s.equals(quote)) {
					if (lastCharDelim) {
						inQuotes = true;
						lastCharQuote = false;
					} else if (lastCharQuote) {
						lastCharQuote = false;
						field.append(quote);
					} else  {
						lastCharQuote = true;
					}
					i += quoteLength;
				} else {
					if (lastCharQuote) {
						field.append(quote);
						lastCharQuote = false;
					}

					field.append(sCh);
					lastCharQuote = false;
					i += 1;
				}
				lastCharDelim = false;
			}
			ret[0] = pre.toString();
			ret[1] = field.toString();
			ret[2] = line.substring(i);
		} else {
			for (i = currFieldNumber; i < fieldNumber; i++) {
				pre.append(delimiter);
			}
			ret[0] = pre.toString();

			ret[2] = "";
		}



		return ret;
	}

}
