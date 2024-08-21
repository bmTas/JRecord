/**
 * @Author Bruce Martin
 * Created on 6/09/2005
 *
 * Purpose:
 * Define mainframe Zoned Decimal Type.
 * 
 * In Zoned-Decimal, the sign is held in the high nyble of the last byte.
 * Decimal points are assumed so Decimal points<ul>
 *   <li> must be added back in (when the value is retrieved)
 *   <li> removed before the value is saved
 * </ul>
 * 
 * in EBCIDIC '0'..'9' are x'F0' .. x'F9'
 * so for Ebcdic 	x'D1' = -1 and x'C1' = +1	
 * # Version 0.60 Bruce Martin 2007/02/16
 *   - Starting to seperate the Record package out from the RecordEditor
 *     so that it can be used seperately. So classes have been moved
 *     to the record package (ie RecordException + new Constant interface
 *     
 * Version 0.80.5
 *    Changed so that for EBCDIC charsets, the sign is calculated at byte level, 
 *    other charsets it is done at character level. 
 *    This will overcome a problem with German-Ebcdic where 
 *    +0 = accented-a instead of {
 **/
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
      
package net.sf.JRecord.Types;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;

/**
 * Define mainframe Zoned Decimal Type
 *
 * @author Bruce Martin
 *
 * @version 0.55
 */
public class ZOld_TypeZonedLeading extends TypeNum {

	/**
     * Define mainframe Zoned Decimal Type
     *
     * <p>This class is the interface between the raw data in the file
     * and what is to be displayed on the screen for Mainframe Zoned Decimal
     * fields.
     */
    public ZOld_TypeZonedLeading() {
        super(false, true, true, false, false, false, false);
    }

    public ZOld_TypeZonedLeading(boolean positive) {
        super(false, true, true, positive, false, false, false);
    }

    /**
     * @see net.sf.JRecord.Types.Type#getField(byte[], int, IFieldDetail)
     */
    public Object getField(byte[] record,
            final int position,
			final IFieldDetail field) {
        String val = super.getFieldText(record, position, field);
        String zoned = val; 
        char ch;
        if (val.length() == 0 || ((ch = val.charAt(0)) >= '0' && ch <= '9')) {
        	
        } else {
			String charset = field.getFontName();
			if (Conversion.isSingleByteEbcidic(charset)) {
				String sign = "";
				byte signByte = record[position - 1];
				if (((byte) (signByte & HIGH_NYBLE)) == ZONED_NEGATIVE_NYBLE_VALUE1) {
					sign = "-";
				}
				byte[] lastDigitBytes = {(byte) (signByte | HIGH_NYBLE)};
			
				zoned = sign 
					  + Conversion.getString(lastDigitBytes, 0, 1, charset)
					  + val.substring(1); 
															 // in EBCIDIC '0'..'9' are x'F0' .. x'F9'
				                                             // in zoned decimal the sign is held in the high Nyble
				                                             // last digit
				                                             // so x'D1' = -1 and x'C1' = +1
			} else {
				zoned = fromZoned(val);
			}
		}

		return addDecimalPoint(
                	zoned,
                	field.getDecimal());
    }


    /**
     * @see net.sf.JRecord.Types.Type#setField(byte[], int, IFieldDetail, Object)
     */
    @Override
    public byte[] setField(byte[] record,
            final int position,
			final IFieldDetail field,
			Object value)
    throws RecordException {

    	if (field.getLen() == 0) {
    		return record;
    	}
    	
        String val = checkValue(field, toNumberString(value));
	    String charset = field.getFontName();
	    if (Conversion.isSingleByteEbcidic(charset)) {
	    	byteLevelAssign(record, position, field, val);
	    } else { 
		    String zoned = toZoned(val, field.getLen());
			copyRightJust(record, zoned,
		            position - 1, field.getLen(),
		            "0", charset);
	    }
	    return record;
    }
    
    
    @Override
	public String formatValueForRecord(IFieldDetail field, String value) {
        String val = checkValue(field, toNumberString(value));
	    String charset = field.getFontName();
	    if (Conversion.isSingleByteEbcidic(charset)) {
	    	int len = field.getLen();
	    	if (len <= 0) {
	    		len = value.length();
	    	}

			byte[] record = new byte[len];
	    	byteLevelAssign(record, 1, field, val);
	    	String s = Conversion.toString(record, charset);
	    	if (field.isFixedFormat()) {
	    		return s;
	    	}
	    	return Conversion.numTrim(s);
	    }
	    
	    val = toZoned(val, field.getLen());
	    if (field.isFixedFormat() && val.length() < field.getLen()) {
	    	return Conversion.padFront(val, field.getLen() - val.length(), '0');			
//	    			new StringBuilder()
//	    					.append(getCharArray(field.getLen() - val.length(), '0'))
//	    					.append(val)
//	    				.toString();
	    }
	    return val;
	}

	private void byteLevelAssign(byte[] record,
            final int position,
			final IFieldDetail field,
			String val) {
		byte andByte = ZONED_POSITIVE_NYBLE_AND;
		int len = field.getLen();
		int posM1 = position - 1;
		if (! field.isFixedFormat()) {
			len = record.length - posM1;
		}

		if (val.startsWith("-")) {
			andByte = ZONED_NEGATIVE_NYBLE_AND;
			val = val.substring(1);
		} else {
			if (val.startsWith("+")) {
				val = val.substring(1);
			} 
			
			if (isPositive()) {
				andByte = (byte) 0xF0;
			}
		}
		copyRightJust(record, val,
				posM1, len,
	            "0", field.getFontName());

		if (! super.isPositive()) {
			record[posM1] = (byte) (record[posM1] & andByte);
		}
    }
	
	private String toZoned(String num, int len) {
	    if (num == null) {
	        return "";
	    }
	    String number = num.trim();
	    StringBuilder ret;

		if (num.equals("") || num.equals("-") || num.equals("+")) {
			// throw ...
			return "";
		}
		
		if (len <= 0) {
			len = num.length();
		}

		char firstChar = number.charAt(0);
		//System.out.print(ret + " Char - " + lastChar);
		if (firstChar == '-') {
			ret = toBuilder(number, 1, len);
			firstChar = ret.charAt(0);
			if (firstChar == '0') {
				ret.setCharAt(0, Conversion.getNegative0EbcdicZoned());
			} else {
				ret.setCharAt(0, (char) (firstChar + Conversion.EBCDIC_ZONED_NEGATIVE_DIFF));
			}
		} else  {
			int st = 0;
		    if (firstChar == '+') {
		        st = 1;
		        if (number.length() == 1) { return ""; }
		    }
		    ret = toBuilder(number, st, len);
			firstChar = ret.charAt(0);

		    if (isPositive() || firstChar < '0' || firstChar > '9') {
		    	
		    } else if (firstChar == '0') {
		    	ret.setCharAt(0, Conversion.getPositive0EbcdicZoned());
		    } else if (! isPositive()){
		    	ret.setCharAt(0, (char) (firstChar + Conversion.EBCDIC_ZONED_POSITIVE_DIFF));
		    }
		}

		return ret.toString();
	}

	private StringBuilder toBuilder(CharSequence cs, int start, int len) {
		StringBuilder b = new StringBuilder(len);
	
		int fldStart = len - cs.length() + start;
		for (int i = 0; i < fldStart; i++) {
			b.append('0');
		}
		for (int i = Math.max(start, cs.length() - len); i < cs.length(); i++) {
			b.append(cs.charAt(i));
		}
		
		return b;
	}
	
	/**
	 * Convert a Mainframe Zoned Number String to a number string
	 *
	 * @param numZoned Zoned Numeric string
	 *
	 * @return number-string
	 */
	public static String fromZoned(String numZoned) {
		String ret;
		String sign = "";
		char firstChar, ucFirstChar;

		if (numZoned == null || ((ret = numZoned.trim()).length() == 0) || ret.equals("-")) {
			return "";
		}

		firstChar = ret.charAt(0);
		ucFirstChar = Character.toUpperCase(firstChar);

		
		switch (ucFirstChar) {
		case 'A':
		case 'B':
		case 'C':
		case 'D':
		case 'E':
		case 'F':
		case 'G':
		case 'H':
		case 'I':
			firstChar = (char) (ucFirstChar - Conversion.EBCDIC_ZONED_POSITIVE_DIFF);
			break;
		case 'J':
		case 'K':
		case 'L':
		case 'M':
		case 'N':
		case 'O':
		case 'P':
		case 'Q':
		case 'R':
			sign = "-";
			firstChar = (char) (ucFirstChar - Conversion.EBCDIC_ZONED_NEGATIVE_DIFF);
			break;
		default:
			if (firstChar == Conversion.getPositive0EbcdicZoned()) {
				firstChar = '0';
			} else if (firstChar == Conversion.getNegative0EbcdicZoned()) {
				firstChar = '0';
				sign = "-";
			}			
		}
		ret = sign + firstChar + ret.substring(1);

		return ret;
	}

}
