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

import net.sf.JRecord.Common.ByteArray;
import net.sf.JRecord.Common.Conversion;

/**
 * Class contains common code for use in other CSV parser's
 * A <i>Csv Parser</i> is used to split a Csv line in to seperate fields
 *
 *
 * @author Bruce Martin
 *
 */
public abstract class BaseCsvByteLineParser implements ICsvByteLineParser {

	private static final byte[] EMPTY_BYTE_ARRAY = {};

    private final boolean quoteInColNames;
//	private final boolean allowReturnInFields;

    public BaseCsvByteLineParser(boolean quoteInColumnNames, boolean allowReturnInFields) {
    	quoteInColNames = quoteInColumnNames;
//    	this.allowReturnInFields = allowReturnInFields;
    }

    /**
     * Whether Quote is to be used in column names
     * @return whether there a quote is to be used in column names 
     */
	public boolean isQuoteInColumnNames() {
		return quoteInColNames;
	}

	/**
	 * Convert a line of column names into a list of column names
	 *
	 * @param line line of column names
	 * @param lineDef Csv Definition
	 * @return list of column names
	 */
	public List<String> getColumnNames(byte[] line, ICsvDefinition lineDef) {
		ArrayList<String> ret = new ArrayList<String>();


        byte[] quote = lineDef.getQuoteDefinition().asBytes();
        byte delim = lineDef.getDelimiterDetails().asByte();
        String fontname = lineDef.getFontName();
        int start = 0;

        if (quoteInColNames && quote != null && quote.length > 0) {
        	byte q = quote[0];
        	for (int i = 0; i < line.length; i++) {
	        	if (line[i] == delim) {
		            ret.add(extractColName(line, q, fontname, start, i - 1));
		            start = i + 1;
	        	}
	        }
        	if (start < line.length) {
        		ret.add(extractColName(line, q, fontname, start, line.length));
        	}
        } else {
	        for (int i = 0; i < line.length; i++) {
	            if (line[i] == delim) {
	            	ret.add(Conversion.getString(line, start, i, fontname));
	            	start = i+1;
	            }
	        }
	        if (start < line.length) {
	        	ret.add(Conversion.getString(line, start, line.length, fontname));
	        }
        }

        return ret;
	}
	
	private String extractColName(byte[] line, byte q, String fontname, int start, int end) {

        if (line[start] == q) {
        	start += 1;
        }
        if (line[end] == q) {
        	end -= 1;
        }
    	
    	String s = "";
    	if (end > start) {
    		s = Conversion.getString(line, start, end, fontname);
    	}
    	return s;
		
	}

	/**
	 * @param lineDef
	 * @return
	 */
	protected final byte[] getDelimFromCsvDef(ICsvDefinition lineDef) {
//		String delimiter = lineDef.getDelimiter();
//		if (delimiter == null || delimiter.length() < 5) {
//			
//		} else if (delimiter.startsWith("x'")) {
//			delimiter = Conversion.toString(new byte[] { Conversion.getByteFromHexString(delimiter) }, lineDef.getFontName());
//		}
		return lineDef.getDelimiterDetails().asBytes();
	}

	/**
	 * Convert a list of column names into a line
	 *
	 * @param names list of column names
	 * @param lineDef Csv Definition
	 * @return The formated column name line (1st line in the file)
	 */
	public byte[] getColumnNameByteLine(List<String> names, ICsvDefinition lineDef) {
		ByteArray buf = new ByteArray(names.size() * 8);
		byte[] currDelim = EMPTY_BYTE_ARRAY;
		byte[] quote = lineDef.getQuoteDefinition().asBytes();
        byte[] delim = getDelimFromCsvDef(lineDef);
        String fontname = lineDef.getFontName();

        if (quoteInColNames) {
			for (int i = 0; i < names.size(); i++) {
		        buf.add(currDelim)
	               .add(quote)
	               .add(Conversion.getBytes(names.get(i), fontname))
	               .add(quote);
	 			currDelim = delim;
			}
        } else {
			for (int i = 0; i < names.size(); i++) {
		        buf.add(currDelim)
	               .add(Conversion.getBytes(names.get(i), fontname));
	 			currDelim = delim;
			}
        }

		return buf.toByteArray();
	}


//	public int getFileStructure(ICsvDefinition csvDefinition, boolean namesOnFirstLine, boolean bin) {
//		int ret = Constants.NULL_INTEGER;
//		String quote = csvDefinition.getQuoteDefinition().asString();
//
//		if ((csvDefinition.isEmbeddedNewLine() || allowReturnInFields)
//		&& quote != null && quote.length() > 0) {
//			ret = Constants.IO_CSV;
//			if (bin) {
//				ret = Constants.IO_BIN_CSV;
//			} else if (! csvDefinition.isSingleByteFont()) {
//				ret = Constants.IO_UNICODE_CSV;
//			}
//
//			if (namesOnFirstLine) {
//				ret += 3;
//			}
//		}
//		return ret;
//	}

	/**
	 * Format field list as a Csv Line
	 * @param fields fields to be organised as a line
	 * @param lineDef Csv Line Definition
	 * @param fieldTypes Field types
	 * @return Formatted Csv line
	 */
	public final byte[] formatFieldListByte(List<? extends Object> fields, ICsvDefinition lineDef, int[] fieldTypes) {
		
		if (fields == null || fields.size() == 0) {
			return EMPTY_BYTE_ARRAY;
		}
		
		byte[][] flds = new byte[fields.size()][];
		String fontname = lineDef.getFontName();
		
		int min = 0;
		if (fieldTypes != null) {
	    	min = Math.min(fieldTypes.length, flds.length);
			for (int i = 0; i < min; i++) {
	    		flds[i] = formatField(toBytes(fields.get(i), fontname), fieldTypes[i], lineDef);
	    	}
		}
		for (int i = min; i < flds.length; i++) {
			flds[i] = formatField(toBytes(fields.get(i), fontname), 0, lineDef);
		}
		return formatFieldArray(flds, lineDef);
	}
	
	private byte[] toBytes(Object o, String fontname) {

		if (o == null) { return EMPTY_BYTE_ARRAY; 	}
		return Conversion.getBytes(o.toString(), fontname);
	}

	/**
	 * Format an array of fields as a Line. There is no formatting of fields (it is assumed that this has already 
	 * been done).
	 * 
	 * @param fields fields to format as a Csv Line
	 * @param lineDef Csv Line definition
	 * 
	 * @return Formatted Csv Line
	 */
	protected final byte[] formatFieldArray(byte[][] fields, ICsvDefinition lineDef) {
		if (fields == null || fields.length == 0) {
			return EMPTY_BYTE_ARRAY;
		}
		int size = 0;
		byte[] delimiter = lineDef.getDelimiterDetails().asBytes();

		for (byte[] s : fields) {
			if (s != null) {
				size += s.length + delimiter.length;
			}
		}
		ByteArray buf = new ByteArray(size);
		buf.add(fields[0]);

		for (int i = 1; i < fields.length; i++) {
	        buf.add(delimiter);
	        if (fields[i] != null) {
	            buf.add(fields[i]);
	        }
	    }
	
	    if (lineDef.getDelimiterOrganisation() != ICsvDefinition.NORMAL_SPLIT && lineDef.getFieldCount() > 0) {
	    	int en = lineDef.getFieldCount();
	    	if (lineDef.getDelimiterOrganisation() == ICsvDefinition.SEP_FOR_EVERY_FIELD_PLUS_END) {
	    		en += 1;
	    	}
	
	    	for (int i = fields.length; i < en; i++) {
	            buf.add(delimiter);
	    	}
	    }
	
	    return buf.toByteArray();
	}
	
//	protected abstract byte[] formatField(String s, int fieldType, ICsvDefinition lineDef);
	
	protected abstract byte[] formatField(byte[] s, int fieldType, ICsvDefinition lineDef);

}
