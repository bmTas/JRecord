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

public class CsvParser {

	/**
	 * Split the line into 3<ol compact>
	 * <li>Line before the request field</li>
	 * <li>The Requested field</li>
	 * <li>Line after the request field</li>
	 * </ol>
	 *
	 * @param fieldNumber  field to retrieve
	 * @param dataSource data-line to parse for fields
	 * @param lineDef Csv Definition
	 * @return Array containing line before the field, The requested field, The line after
	 * the request field.
	 */
	public List<String> getLine(int fieldNumber, ICharIterator dataSource, ICsvDefinition lineDef) {
		List<String> ret = new ArrayList<String>();
//		StringBuilder pre   = new StringBuilder("");
		StringBuilder field = null;
		//String s, sCh;
		char ch, delim;
		int quoteIdx = 0;
		boolean inQuotes = false;
		boolean lastCharDelim = true;
		boolean lastCharQuote = false;
//		int currFieldNumber = 0;
		//int i = 0;
		String delimiter = getDelimFromCsvDef(lineDef);
		String quote = lineDef.getQuoteDefinition().asString();
//		int quoteLength = 1;
//		if (quote != null && quote.length() > 0) {
//			quoteLength = quote.length();
//		}

		if (delimiter == null || delimiter.length() != 1) {
			throw new RuntimeException("Invalid field delimiter: " + delimiter);
		}

		delim = delimiter.charAt(0);

		field = new StringBuilder("");
		lastCharDelim = true;
		while (dataSource.hasNext()) {
			ch = dataSource.get();

			//System.out.println("~ :" + quote + ": " + field + " ->" + s + "<- " + inQuotes
			//		+ " " + lastCharQuote + " " + lastCharDelim);

			if (ch == delim
			&& ((! inQuotes) || lastCharQuote)) {
				ret.add(field.toString());
				field = new StringBuilder();
				quoteIdx = 0;
			} else if (ch == quote.charAt(quoteIdx++) && quoteIdx >= quote.length()) {
				if (lastCharDelim) {
					inQuotes = true;
					lastCharQuote = false;
				} else if (lastCharQuote) {
					lastCharQuote = false;
					field.append(quote);
				} else  {
					lastCharQuote = true;
				}
			} else {
				if (lastCharQuote) {
					field.append(quote);
					lastCharQuote = false;
				}

				field.append(ch);
				lastCharQuote = false;
			}
			lastCharDelim = false;
		}


		return ret;
	}


	/**
	 * @param lineDef
	 * @return
	 */
	protected final String getDelimFromCsvDef(ICsvDefinition lineDef) {
		String delimiter = lineDef.getDelimiterDetails().asString();
		return delimiter;
	}

}
