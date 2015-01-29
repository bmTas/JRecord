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
	 * @param line line to parse for fields
	 * @param lineDef Csv Definition
	 * @return Array containg line before the field, The requested field, The line after
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
		String delimiter = lineDef.getDelimiter();
		String quote = lineDef.getQuote();
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
				&& ((! inQuotes) || (inQuotes && lastCharQuote))) {
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

}
