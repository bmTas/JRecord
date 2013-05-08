package net.sf.JRecord.CsvParser;

import net.sf.JRecord.Types.Type;

/**
 * CSV line parser which will look for matching Quotes
 *
 *
 * @author Bruce Martin
 *
 */
public class StandardParser extends BaseCsvParser implements AbstractParser {

	private boolean textFieldsInQuotes = false;


	/**
	 * Standard CSV line parser
	 */
	public StandardParser() {
		super(false);
	}

	/**
	 * Standard CSV line parser
	 * @param putTextFieldsInQuotes put Quotes around Text Fields
	 */
	public StandardParser(boolean putTextFieldsInQuotes) {
		super(false);
		textFieldsInQuotes = putTextFieldsInQuotes;
	}


	/**
	 * Standard CSV line parser
	 * @param putTextFieldsInQuotes put Quotes around Text Fields
	 */
	public StandardParser(boolean putTextFieldsInQuotes, boolean quoteInColumnNames) {
		super(quoteInColumnNames);
		textFieldsInQuotes = putTextFieldsInQuotes;
	}



	/**
	 * @see net.sf.JRecord.CsvParser.AbstractParser#getField(int, java.lang.String, java.lang.String, java.lang.String)
	 */
	public String getField(int fieldNumber, String line, ICsvDefinition lineDef) {
		String[] lineVals = split(fieldNumber, line, lineDef);
		String ret = lineVals[1];
		if (ret == null) {
			ret = "";
		}
		return ret;
	}

	/**
	 *
	 */
	public String getField2(int fieldNumber, String line, ICsvDefinition lineDef) {
		String[] lineVals = split(fieldNumber, line, lineDef);
		return lineVals[1];
	}

	/**
	 * @see net.sf.JRecord.CsvParser.AbstractParser#setField(int, int, java.lang.String, java.lang.String, java.lang.String, java.lang.String)
	 */
	public String setField(int fieldNumber, int fieldType, String line, ICsvDefinition lineDef, String newValue) {
		String[] lineVals = split(fieldNumber, line, lineDef);
		String s = newValue;
		if (newValue == null) {
			s = "";
		}
		String delimiter = lineDef.getDelimiter();
		String quote = lineDef.getQuote();

//		if (textFieldsInQuotes) {
//			System.out.print("--> " + fieldType + " " + Type.NT_NUMBER + " " + s + " --> ");
//		}
		if (s.indexOf(delimiter) >= 0
		|| (quote != null && ! "".equals(quote) && s.indexOf(quote) >= 0)
		|| (textFieldsInQuotes & (fieldType != Type.NT_NUMBER))) {
			StringBuffer b = new StringBuffer(s);
			int pos;
			int i = 0;

			while ((pos = b.indexOf(quote, i)) >= 0) {
				b.insert(pos, quote);
				i = pos + 2;
			}
			s = quote + b.toString() + quote;
		}
//		if (textFieldsInQuotes) {
//			System.out.println(s);
//		}
		return lineVals[0] + s + lineVals[2];
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
	 * @return Array containg line before the field, The requested field, The line after
	 * the request field.
	 */
	private String[] split(int fieldNumber, String line, ICsvDefinition lineDef) {
		String[] ret = new String[]{null, null, ""};
		StringBuilder pre   = new StringBuilder("");
		StringBuilder field = null;
		String s;
		boolean inQuotes = false;
		boolean lastCharDelim = true;
		boolean lastCharQuote = false;
		int currFieldNumber = 0;
		int i = 0;
		String delimiter = lineDef.getDelimiter();
		String quote = lineDef.getQuote();


		while (i < line.length() && currFieldNumber < fieldNumber) {
			s = line.substring(i, i + 1);
			pre.append(s);
			if (s.equals(delimiter)
			&& ((! inQuotes) || (inQuotes && lastCharQuote))) {
				lastCharDelim = true;
			 	currFieldNumber += 1;
			 	lastCharQuote = false;
			 	inQuotes = false;
			} else {
				if (s.equals(quote) && lastCharDelim) {
					inQuotes = true;
					lastCharQuote = false;
				} else if (s.equals(quote)) {
					lastCharQuote = ! lastCharQuote;
				}

				lastCharDelim = false;
			}
			i += 1;
		}

		if (i < line.length()) {
			field = new StringBuilder("");
			lastCharDelim = true;
			while (i < line.length()) {
				s = line.substring(i, i + 1);

				//System.out.println("~ :" + quote + ": " + field + " ->" + s + "<- " + inQuotes
				//		+ " " + lastCharQuote + " " + lastCharDelim);

				if (s.equals(delimiter)
				&& ((! inQuotes) || (inQuotes && lastCharQuote))) {
					break;
				} else if (s.equals(quote) && lastCharDelim) {
					inQuotes = true;
					lastCharQuote = false;
				} else if (s.equals(quote) && lastCharQuote) {
					lastCharQuote = false;
					field.append(quote);
				} else if (s.equals(quote)) {
					lastCharQuote = true;
				} else if (lastCharQuote) {
					field.append(quote).append(s);
					lastCharQuote = false;
				} else {
					field.append(s);
					lastCharQuote = false;
				}
				lastCharDelim = false;
				i += 1;
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
