/*
 * @Author Bruce Martin
 * Created on 13/04/2007
 *
 * Purpose:
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import net.sf.JRecord.Common.ByteArray;
import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Types.Type;

/**
 * Basic CSV line parser. Basically
 *
 *   - If the Field start with {Quote}; the fields is ended by {Quote}{Field-Seperator}
 *   - Otherwise the field ends with a {Field-Seperator}
 *
 * <br>
 * <b>Warning</b> - This class must be kept in sync with <b>BasicCsvLineParserExtended</b> class<br\>
 * @see BasicCsvLineParserExtended
 * 
 * @author Bruce Martin
 *
 */
public final class BasicCsvByteLineParserExtended extends BaseCsvByteLineParser  {
	
	public static final byte[] EMPTY_BYTE_ARRAY = {};

    private static BasicCsvByteLineParserExtended instance = new BasicCsvByteLineParserExtended(false);

 	public final int delimiterOrganisation;
 	final boolean textFieldsInQuotes;


    public BasicCsvByteLineParserExtended(boolean quoteInColumnNames) {
    	this(quoteInColumnNames, ICsvDefinition.NORMAL_SPLIT, false, false);
    }



	public BasicCsvByteLineParserExtended(boolean quoteInColumnNames, int delimiterOrganisation) {
		this(quoteInColumnNames, delimiterOrganisation, false, false);
	}



	public BasicCsvByteLineParserExtended(boolean quoteInColumnNames, int delimiterOrganisation, boolean allowReturnInFields, boolean textFieldsInQuotes) {
		super(quoteInColumnNames, allowReturnInFields);
		this.delimiterOrganisation = delimiterOrganisation;
		this.textFieldsInQuotes = textFieldsInQuotes;
	}


//	/**
//	 * check if it is end of the fields
//	 */
//	protected boolean  endOfField(int startAt, String s, byte[] quote) {
//		int ql = quote.length;
//		int pos = s.length();
//		int i = 0;
//	
////		String t = s.substring(pos -  ql, pos);
////		System.out.print(s.substring(pos -  ql, pos) + " " + s.substring(pos -  ql, pos).equals(quote));
//		while ((pos > ql || (startAt > 0 && pos == ql))
//			&& s.substring(pos -  ql, pos).equals(quote) ) {
//			pos -= ql;
//			i += 1;
//		}
//		//System.out.println("--)) " + s.length() + " > " + (ql * i) + ", i=" + i + " " + (i % 2) + " " + (s.length() > ql));
//		return startAt + s.length() > ql * i && i % 2 == 1;
//	}


    private byte[] formatField(String s, int fieldType, ICsvDefinition lineDef) {
    	byte[] ret = EMPTY_BYTE_ARRAY;
    	if (s != null && s.length() > 0) {
    		ret = formatField(Conversion.getBytes(s, lineDef.getFontName()), fieldType, lineDef);
        }
        return ret;
    }


	@Override
	protected byte[] formatField(byte[] ret, int fieldType, ICsvDefinition lineDef) {
		String fontname = lineDef.getFontName();
		byte[] quote = lineDef.getQuoteDefinition().asBytes();
		if  (quote != null && quote.length > 0 && ret != null && ret.length > 0) {
			byte[] delim = super.getDelimFromCsvDef(lineDef);
			byte[] cr = Conversion.getBytes("\n", fontname);
			byte[] lf = Conversion.getBytes("\r", fontname);

			if (textFieldsInQuotes && (fieldType != Type.NT_NUMBER)) {
				ret = encodeField(ret, quote, delim, cr, lf);
			} else {
				for (int i = 0; i < ret.length; i++) {
					if (CommonBits.checkFor(ret, i, delim)
					|| CommonBits.checkFor(ret, i, quote)
					|| CommonBits.checkFor(ret, i, cr)
					|| CommonBits.checkFor(ret, i, lf)) {
						ret =  encodeField(ret, quote, delim, cr, lf);
						break;
					}
				}
			}
		}
		return ret;
	}



	private byte[] encodeField(byte[] ret, byte[] quote, byte[] delim, byte[] cr, byte[] lf) {
		ByteArray newFld = new ByteArray(ret.length + 10);
		int ql = quote.length - 1;
		int dl = delim.length - 1;
		newFld.add(quote);
		
		for (int j = 0; j < ret.length; j++) {
			if (CommonBits.checkFor(ret, j + dl, delim)) {
				newFld.add(delim);
				j += dl;
			} else if (CommonBits.checkFor(ret, j + ql, quote)) {
				newFld.add(quote).add(quote);
				j += ql;
			} else if (CommonBits.checkFor(ret, j, cr)) {
				newFld.add(cr);
			} else if (CommonBits.checkFor(ret, j, lf)) {
				newFld.add(lf);
			} else {
				newFld.add(ret[j]);
			}
		}
		newFld.add(quote);
		return newFld.toByteArray();
	}
	
	
	
    /**
     * Return a basic CSV line parser
     * @return Returns the instance.
     */
    public static BasicCsvByteLineParserExtended getInstance() {
        return instance;
    }
    
    /* ---------------------------------------------------------------------------------------------- */
    
	/**
     * Get the field Count
     *
     * @param line line to inspect
	 * @param lineDef Csv Definition
     * @return the number of fields
     */
    public final int getFieldCount(byte[] line, ICsvDefinition lineDef) {
        byte[][] fields = splitBytes(line, lineDef, 0, new BasicLineParserHelper(line, lineDef));

        if (fields == null) {
            return 0;
        }

        return fields.length;
    }

    /**
     * Get a specific field from a line
     *
     * @see ICsvCharLineParser#getField(int, String, ICsvDefinition)
     */
    public final String getField(int fieldNumber, byte[] line, ICsvDefinition lineDef) {
        String[] fields = split(line, lineDef, fieldNumber);

        if (fields == null  || fields.length <= fieldNumber || fields[fieldNumber] == null) {
            return null;
        }
 //       String quote = lineDef.getQuote();

 //       update4quote(fields, fieldNumber, lineDef.getQuoteDefinition().asString());
//        if (isQuote(quote)
//        && fields[fieldNumber].startsWith(quote)
//        && fields[fieldNumber].endsWith(quote)) {
//        	String v = "";
//
//        	if (fields[fieldNumber].length() >= quote.length() * 2) {
//	            int quoteLength = quote.length();
//				v = fields[fieldNumber].substring(
//						quoteLength, fields[fieldNumber].length() - quoteLength
//				);
//	        }
//        	fields[fieldNumber] = v;
//        }

        return fields[fieldNumber];
    }
    
    

    @Override
	public final List<String> getFieldList(byte[] line, ICsvDefinition csvDefinition) {
    	String[] fields = split(line, csvDefinition, 0);
    	if (fields == null) {
            return new ArrayList<String>(1);
        }
		
		return Arrays.asList(fields);
	}



	/**
     * @see ICsvCharLineParser#setField(int, int, String, ICsvDefinition, String)
     */
    public final byte[] setFieldByteLine(int fieldNumber, int fieldType, byte[] line, ICsvDefinition lineDef,
            String newValue) {

        byte[][] fields = splitBytes(line, lineDef, fieldNumber, new BasicLineParserHelper(line, lineDef));

        if (fields == null || fields.length == 0) {
            fields = initArray(fieldNumber + 1);
        }

        fields[fieldNumber] = formatField(newValue, fieldType, lineDef);
        
        return formatFieldArray(fields, lineDef);
    }
  
    /**
     * Initialize array
     * @param count array size
     * @return initialized array
     */
    private byte[][] initArray(int count) {
        byte[][] ret = new byte[count][];

        for (int i = 0; i < count; i++) {
            ret[i] = EMPTY_BYTE_ARRAY;
        }

        return ret;
    }


	/**
	 * Split a supplied line into its Fields. This only applies to
	 * Comma / Tab separated files
	 *
	 * @param line line to be split
	 * @param lineDefinition Csv Definition
	 * @param min minimum number of elements in the array
	 *
	 * @return Array of fields
	 */
	public final String[] split(byte[] line, ICsvDefinition lineDefinition, int min) {
		byte[][] values = splitBytes(line, lineDefinition, min);
		String[] ret = new String[values.length];
		String fontName = lineDefinition.getFontName();
		
		for (int i = 0; i < values.length; i++) {
			ret[i] = values[i] == null || values[i].length == 0 ? "" : Conversion.toString(values[i], fontName);
		}
		
		return ret;
	}
	
	public final byte[][] splitBytes(byte[] line, ICsvDefinition lineDefinition, int min) {
		return splitBytes(line, lineDefinition, min, new StandardLineParserHelper(line, lineDefinition));
	}
	
	private final byte[][] splitBytes(byte[] line, ICsvDefinition lineDefinition, int min, IParseLineHelper parseHelper) {
		byte[] delimiter = super.getDelimFromCsvDef(lineDefinition);
		if ((delimiter == null || line == null)
		||  (delimiter.length == 0)) {
			return null;
		}

		//int i = 0;
//		StringTokenizer tok;
		int newLength, j, idx=0;
		byte[][] temp, ret;
		byte[] quote = lineDefinition.getQuoteDefinition().asBytes();

//		tok = new StringTokenizer(line, delimiter, true);
		int count  = 1;
		int delimLengthm1 = delimiter.length - 1;
		for (int i = 0; i < line.length; i++) {
			if (CommonBits.checkFor(line, i, delimiter)) {
				count += 1;
				i += delimLengthm1;
			}
		}
		//len = tok.countTokens();
		temp = new byte[Math.max(count, min)][];

		//if (min == 4) System.out.println("}}"+ quote + "< >" + isQuote(quote)+ "{{ ");
		if (! isQuote(quote)) {
			int start = 0;

			for (int i = 0; i < line.length; i++) {
				if (CommonBits.checkFor(line, i, delimiter)) {
					if (i == start) {
						temp[idx++] = EMPTY_BYTE_ARRAY;
					} else {
						temp[idx++] = extract(line, start, i);
					}
					i += delimLengthm1;
					start = i + 1;
				}
			}
			if (line.length > start) {
				temp[idx++] = extract(line, start, line.length);
			}   
		} else {
			int start = parseHelper.start();
			parseHelper.setDetails(count, min);
		    
		    for (int i = start; i < line.length; i++) {
				if (CommonBits.checkFor(line, i, delimiter) 
				&& parseHelper.isQuote(i)) {
					i = parseHelper.newField(i);
				} else if (CommonBits.checkFor(line, i + quote.length - 1, quote)) {
					i = parseHelper.quote(i);
				} else {
					parseHelper.normalChar(i);
				}

//		    ByteArray buf = new ByteArray(Math.max(20, line.length / Math.max(1, count)));
//
//		    boolean inQuote = CommonBits.checkFor(line, 0, quote);
//		    int start = inQuote ? quote.length : 0;
//		    int quotePos = -11;
//		    
//		    for (int i = start; i < line.length; i++) {
//				if (CommonBits.checkFor(line, i, delimiter) 
//				&& (quotePos == i - quote.length || ! inQuote)) {
//					if (buf.length() == 0) {
//						temp[idx++] = EMPTY_BYTE_ARRAY;
//					} else {
//						temp[idx++] = buf.toByteArray();
//					}
//					i += delimLengthm1;
//					start = i + 1;
//					buf.clear();
//					inQuote = CommonBits.checkFor(line, i+quote.length, quote);
//					if (inQuote) {
//						i += quote.length;
//					}
//				} else if (CommonBits.checkFor(line, i + quote.length - 1, quote)) {
//					if (quotePos < i - quote.length) {
//						quotePos = i;
//					} else {
//						buf.add(quote);
//					}
//					i += quote.length - 1;
//				} else {
//					if (quotePos == i - quote.length) {
//						buf.add(quote);
//					}
//						
//					buf.add(line[i]);
//				}
		    }

		    temp = parseHelper.fields();
		    idx = parseHelper.fieldCount();
		}

		ret = temp;
		newLength = Math.max(idx, min + 1);
		if (newLength != temp.length) {
		    ret = new byte[newLength][];
		    System.arraycopy(temp, 0, ret, 0, idx);
//		    for (j = 0; j < i; j++) {
//		        ret[j] = temp[j];
//		    }
		    for (j = idx; j < newLength; j++) {
		        ret[j] = EMPTY_BYTE_ARRAY;
		    }
		}

		return ret;
	}

//	public final byte[][] splitBytes(byte[] line, ICsvDefinition lineDefinition, int min) {
//
//		byte[] delimiter = super.getDelimFromCsvDef(lineDefinition);
//		if ((delimiter == null || line == null)
//		||  (delimiter.length == 0)) {
//			return null;
//		}
//
//		//int i = 0;
////		StringTokenizer tok;
//		int newLength, j, idx=0;
//		byte[][] temp, ret;
//		byte[] quote = lineDefinition.getQuoteDefinition().asBytes();
//
////		tok = new StringTokenizer(line, delimiter, true);
//		int count  = 1;
//		int delimLengthm1 = delimiter.length - 1;
//		for (int i = 0; i < line.length; i++) {
//			if (CommonBits.checkFor(line, i, delimiter)) {
//				count += 1;
//				i += delimLengthm1;
//			}
//		}
//		//len = tok.countTokens();
//		temp = new byte[Math.max(count, min)][];
//
//		//if (min == 4) System.out.println("}}"+ quote + "< >" + isQuote(quote)+ "{{ ");
//		if (! isQuote(quote)) {
//			int start = 0;
//
//			for (int i = 0; i < line.length; i++) {
//				if (CommonBits.checkFor(line, i, delimiter)) {
//					if (i == start) {
//						temp[idx++] = EMPTY_BYTE_ARRAY;
//					} else {
//						temp[idx++] = extract(line, start, i);
//					}
//					i += delimLengthm1;
//					start = i + 1;
//				}
//			}
//			if (line.length > start) {
//				temp[idx++] = extract(line, start, line.length);
//			}   
//		} else {
//		    ByteArray buf = new ByteArray(Math.max(20, line.length / Math.max(1, count)));
//
//		    boolean inQuote = CommonBits.checkFor(line, 0, quote);
//		    int start = inQuote ? quote.length : 0;
//		    int quotePos = -11;
//		    
//		    for (int i = start; i < line.length; i++) {
//				if (CommonBits.checkFor(line, i, delimiter) 
//				&& (quotePos == i - quote.length || ! inQuote)) {
//					if (buf.length() == 0) {
//						temp[idx++] = EMPTY_BYTE_ARRAY;
//					} else {
//						temp[idx++] = buf.toByteArray();
//					}
//					i += delimLengthm1;
//					start = i + 1;
//					buf.clear();
//					inQuote = CommonBits.checkFor(line, i+quote.length, quote);
//					if (inQuote) {
//						i += quote.length;
//					}
//				} else if (CommonBits.checkFor(line, i + quote.length - 1, quote)) {
//					if (quotePos < i - quote.length) {
//						quotePos = i;
//					} else {
//						buf.add(quote);
//					}
//					i += quote.length - 1;
//				} else {
//					if (quotePos == i - quote.length) {
//						buf.add(quote);
//					}
//						
//					buf.add(line[i]);
//				}
//		    }
//
//		    if (buf.length() > 0) {
//		    	temp[idx++] = buf.toByteArray();
//		    }
//		}
//
//		ret = temp;
//		newLength = Math.max(idx, misetDetailsn + 1);
//		if (newLength != temp.length) {
//		    ret = new byte[newLength][];
//		    System.arraycopy(temp, 0, ret, 0, idx);
////		    for (j = 0; j < i; j++) {
////		        ret[j] = temp[j];
////		    }
//		    for (j = idx; j < newLength; j++) {
//		        ret[j] = EMPTY_BYTE_ARRAY;
//		    }
//		}
//
//		return ret;
//	}



	protected byte[] extract(byte[] line, int start, int i) {
		byte[] fld;
		fld = new byte[i - start];
		System.arraycopy(line, start, fld, 0, i - start);
		return fld;
	}
	/**
	 * is quote present
	 * @param quote quote string
	 * @return if it defines a quote
	 */
	private final boolean isQuote(byte[] quote) {
	    return quote != null && quote.length > 0;
	}
	

	private static interface IParseLineHelper {
		public void setDetails(int fldCount, int min);
		
		public int start();
		
		public int fieldCount();
		
		public boolean isQuote(int pos);
		
		public int newField(int pos);
		
		public int quote(int pos);
		
		public void normalChar(int pos);
		
		public byte[][] fields();
	}
	

	private abstract class BaseLineParserHelper implements IParseLineHelper {
		final byte[] line, delimiter, quote;
		final int delimLengthm1, start;
		ByteArray buf;
		byte[][] temp;
		boolean inQuote;
	    int  idx = 0 ;
	    int quotePos = -11;
		
	    private BaseLineParserHelper(byte[] line, ICsvDefinition csvDef) {
			this.line = line;
			delimiter = getDelimFromCsvDef(csvDef);
			quote = csvDef.getQuoteDefinition().asBytes();
			
			delimLengthm1 = delimiter.length - 1;
			
			if (quote == null || quote.length == 0) {
				inQuote = false;
				start = 0;
			} else {
				inQuote = CommonBits.checkFor(line, quote.length - 1, quote);
				start = inQuote ? quote.length : 0;
			}
	    }
	    
	    
		@Override
		public final int fieldCount() {
			return idx;
		}


		@Override
		public final boolean isQuote(int pos) {
			return (quotePos == pos - quote.length || ! inQuote);
		}


		@Override
		public final void setDetails(int fldCount, int min) {
			buf = new ByteArray(Math.max(20, line.length / Math.max(1, fldCount)));
			temp = new byte[Math.max(fldCount, min)][];
		}
	}

	private class BasicLineParserHelper extends  BaseLineParserHelper {
		int fldStart;
		
	    private BasicLineParserHelper(byte[] line, ICsvDefinition csvDef) {
			super(line, csvDef);
			fldStart = 0;
	    }
	    

		@Override
		public int start() {
			return 0;
		}


		@Override
		public int newField(int pos) {
			if (pos == fldStart) {
				temp[idx++] = EMPTY_BYTE_ARRAY;
			} else {
				temp[idx++] = extract(line, fldStart, pos);
			}
			pos += delimLengthm1;
			inQuote = CommonBits.checkFor(line, pos+quote.length, quote);
			fldStart = pos + 1;
			if (inQuote) {
				pos += quote.length;
			}
			return pos;
		}

		@Override
		public int quote(int pos) {
			if (quotePos < pos - quote.length) {
				quotePos = pos;
			}
			return pos + quote.length - 1;
		}

		@Override
		public void normalChar(int pos) {
		}

		@Override
		public byte[][] fields() {
			if (line.length > fldStart) {
				temp[idx++] = extract(line, fldStart, line.length);
			}   

			return temp;
		}
	}
	
	

	private class StandardLineParserHelper extends BaseLineParserHelper {
		
	    private StandardLineParserHelper(byte[] line, ICsvDefinition csvDef) {
			super(line, csvDef);
	    }
	    

		@Override
		public final int start() {
			return start;
		}


		@Override
		public int newField(int pos) {
			if (buf.length() == 0) {
				temp[idx++] = EMPTY_BYTE_ARRAY;
			} else {
				temp[idx++] = buf.toByteArray();
			}
			pos += delimLengthm1;
			//start = pos + 1;
			buf.clear();
			inQuote = CommonBits.checkFor(line, pos+quote.length, quote);
			if (inQuote) {
				pos += quote.length;
			}
			return pos;
		}

		@Override
		public int quote(int pos) {
			if (quotePos < pos - quote.length) {
				quotePos = pos;
			} else {
				buf.add(quote);
			}
			return pos + quote.length - 1;	
		}

		@Override
		public void normalChar(int pos) {
			if (quotePos == pos - quote.length) {
				buf.add(quote);
			}
				
			buf.add(line[pos]);
		}


		@Override
		public byte[][] fields() {

		    if (buf.length() > 0) {
		    	temp[idx++] = buf.toByteArray();
		    }
		    return temp;
		}
		
		
	}
}
