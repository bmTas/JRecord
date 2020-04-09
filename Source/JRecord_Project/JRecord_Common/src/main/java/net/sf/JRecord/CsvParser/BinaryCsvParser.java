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

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFieldDetail;

/**
 * This class is for working with delimited records where the Delimiter is provided as a byte. As such it will  <ul>
 * <li>Extract a field from an array of bytes
 * <li>Set a field in an Array of bytes
 * <li>Get the starting position of a field in an array of Bytes
 * <li>Count the number of fields
 * </ul>
 * 
 * @author Bruce Martin
 * @version 0.68
 * 
 * @deprecated Use <br\>ParserManagerByteLine.getInstance().get(ICsvParserIds.EXTENDED_BASIC_CSV_PARSER)<br\>
 *       instead !!!!
 */

public class BinaryCsvParser {

	private static final byte[] NULL_BYTES = {};
    private int lastPos = 0;
    private int currPos = 0;
    private final byte look4;
    private int foundAt = 0;

    
//    public BinaryCsvParser(String delimiter) {
//    	look4 = Conversion.getByteFromHexString(delimiter); 
//    }
//
    /**
     * 
     * @param delimiter
     * 
     *  @deprecated Use <br\>ParserManagerByteLine.getInstance().get(ICsvParserIds.EXTENDED_BASIC_CSV_PARSER)<br\>
     *       instead !!!!
     */
    public BinaryCsvParser(byte delimiter) {
    	look4 = delimiter; 
    }
		
	private void getPos(byte[] record, int pos) {
     	int i = 0;
     	
     	lastPos = 0;
     	currPos = 0;
     	foundAt = 0;
    	while (foundAt < pos && i < record.length) {
    		if (record[i] == look4) {
    			lastPos = currPos;
    			currPos = i + 1;
    			foundAt += 1;
    		}
    		i += 1;
    	}
    	if (foundAt == pos) {
    		currPos -= 1;
     	} else /* if (currPos < record.length)*/ {
     		foundAt += 1;
     		lastPos = currPos;
     		currPos = record.length;
     	} 
	}

	/**
	 * Count the number of tokens
	 * @param record record to search
	 * @return number of tokens
	 */
	public int countTokens(byte[] record) {
		int count = 1;
		
		if (record != null) {
			for (int i =0; i < record.length; i++) {
				if (record[i] == look4) {
					count += 1;
				}
			}
		}
		return count;
	}
	
	/**
	 * get a fields value
	 * @param record record to retrieve the field from
	 * @param field field details
	 * @return requested fields value
	 */
	public String getValue(byte[] record, IFieldDetail field) {
		return getValue(record,  field.getPos(), field.getFontName());
	}
	
	/**
	 * get a fields value
	 * 
	 * @param record record to search
	 * @param pos position
	 * @param font font name
	 * @return requested field
	 */
	public String getValue(byte[] record, int pos, String font) {
		String ret = null;
		
		try {
			getPos(record, pos);
			
			//System.out.println("--->> get Value " + pos + " > "+ lastPos + " " + currPos + " < " + foundAt + " " + pos);
			if (foundAt == pos) {
				ret = Conversion.getString(record, lastPos, Math.min(record.length, currPos), font);
			}
		} catch (Exception e) {
			// TODO: handle exception
		}
		
		return ret; 
	}
	

    /**
     * Get list of fields
     * @param line line to extract the list from
     * @param font font of the file
     * @return list of fields
     */
    public List<String> getFieldList(byte[] line,  String font) {
    	ArrayList<String> ret = new ArrayList<String>();
    	int start = 0;
     	
    	for (int i = 0; i < line.length; i++) {
    		if (line[i] == look4) {
    			ret.add(Conversion.getString(line, start, i, font));
    			start = i+1;
    		}
    	}
		ret.add(Conversion.getString(line, start, line.length, font));
		
		return ret;
    }
    

	
	public byte[] updateValue(byte[] record, IFieldDetail field, String value) {
		byte[] ret = record;
		byte[] temp;
		int i;
		
		try {
			temp = Conversion.getBytes(value, field.getFontName());
			getPos(record,  field.getPos());
			
			if (foundAt == field.getPos()) {
				int currLen = currPos - lastPos;
				if (currLen == temp.length) {
					System.arraycopy(temp, 0, ret, lastPos, temp.length);
				} else {
					byte[] old = record;
					ret = new byte[record.length + temp.length - currLen];
					//System.out.println("--->> " + currPos);
					if (lastPos > 0) {
						System.arraycopy(old, 0, ret, 0, lastPos);
	//					ret[currPos-1] = look4;
					}
					System.arraycopy(temp, 0, ret, lastPos, temp.length);
					if (currPos < old.length) {
						//System.out.println("--->> found" + (currPos + 1) + " " + (lastPos + temp.length) + "  " + ret.length);
						System.arraycopy(old, currPos, ret,  lastPos + temp.length, old.length - currPos);
					}
				}
			} else {
					byte[] old = record;
					int dif = field.getPos() - foundAt;
					if (field.getPos() == 1) {
						dif -= 1;
					}
											
					ret = new byte[currPos + temp.length + dif];
					
					System.arraycopy(old, 0, ret, 0, currPos);
					for (i =dif - 1; i >= 0 ; i--) {
						ret[old.length + i +0] = look4;
					}
					
					//System.out.println("--->> not found " + new String(temp) + " ~> "+ old.length + " " + (old.length + dif - 1) + "  " + (ret.length)
					//		+ " ~~ " + temp.length + " " + (ret.length - (old.length + dif)) );
					System.arraycopy(temp, 0, ret, old.length  + dif, temp.length);
					//System.out.println("--->> output ~  " + new String(ret));
			}
			
		} catch (Exception e) {
			e.printStackTrace();
			return record;
		}
		return ret;
	}

	

	/**
	 * Format field list as a Csv Line
	 * @param fields fields to be organised as a line
	 * @param font character-set to use in byte conversion
	 * @return Formatted Csv line
	 */
	public byte[] formatFieldList(List<String> fields, String font) {
		if (fields == null || fields.size() == 0) {
			return NULL_BYTES;
		}
		int size = countListSize(0, fields);
		int charSize = 1;
		if (Conversion.isMultiByte(font)) {
			charSize = 2;
		}
		
		byte[] sep = {look4};
		byte[] b = Conversion.getBytes(fields.get(0), font);
		byte[] ret = copyTo(0, new byte[size * charSize + fields.size() - 1], b);
		int pos = b.length;
		
		for (int i = 1; i < fields.size(); i++) {
			b = Conversion.getBytes(fields.get(i), font);
			ret = copyTo(pos++, ret, sep);
			ret = copyTo(pos, ret, b);
			pos += b.length;
		}
		
		if (pos < ret.length) {
			byte[] t = new byte[pos];
			System.arraycopy(ret, 0, t, 0, pos);
			ret = t;
		}
		return ret;
	}

	private byte[] copyTo(int pos, byte[] to, byte[] from) {
		if (pos + from.length > to.length) {
			byte[] t = new byte[(pos + from.length + 6) * 6 / 5];
			//System.out.println("==> " + pos + " " + to.length + " ~ " + from.length + " / " + t.length);
			System.arraycopy(to, 0, t, 0, pos-1);
			to = t;
		}
		System.arraycopy(from, 0, to, pos, from.length);
		return to;
		
	}
	
	private int countListSize(int start, List<String> fields) {
		int size = 0;
		for (int i = fields.size() - 1; i >= start; i--) {
			size += fields.get(i).length();
		}
		
		return size;
	}
	
}
