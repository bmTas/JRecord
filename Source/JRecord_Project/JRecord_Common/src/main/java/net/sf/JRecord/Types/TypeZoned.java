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
public class TypeZoned extends TypeNum {

	/**
     * Define mainframe Zoned Decimal Type
     *
     * <p>This class is the interface between the raw data in the file
     * and what is to be displayed on the screen for Mainframe Zoned Decimal
     * fields.
     */
    public TypeZoned() {
        super(false, true, true, false, false, false, false);
    }

    public TypeZoned(boolean positive) {
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
        if (val.length() == 0 || ((ch = val.charAt(val.length() - 1)) >= '0' && ch <= '9')) {
        	
        } else {
			String charset = field.getFontName();
			if (Conversion.isSingleByteEbcidic(charset)) {
				String sign = "";
				int end = position + field.getLen() - 1;
				byte signByte = record[end - 1];
				if (((byte) (signByte & HIGH_NYBLE)) == ZONED_NEGATIVE_NYBLE_VALUE1) {
					sign = "-";
				}
				byte[] lastDigitBytes = {(byte) (signByte | HIGH_NYBLE)};
			
				zoned = sign 
					  + val.substring(0, val.length() - 1) 
					  + Conversion.getString(lastDigitBytes, 0, 1, charset); 
															 // in EBCIDIC '0'..'9' are x'F0' .. x'F9'
				                                             // in zoned decimal the sign is held in the high Nyble
				                                             // last digit
				                                             // so x'D1' = -1 and x'C1' = +1
			} else {
				zoned = Conversion.fromZoned(val);
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
			return record;
//			byte andByte = ZONED_POSITIVE_NYBLE_OR;
//			int endPos = field.getEnd() - 1;
//
//			if (val.startsWith("+")) {
////				andByte = ZONED_POSITIVE_NYBLE;
//				val = val.substring(1);
//			} else if (val.startsWith("-")) {
//				andByte = ZONED_NEGATIVE_NYBLE_OR;
//				val = val.substring(1);
//			}
//			copyRightJust(record, val,
//		            position - 1, field.getLen(),
//		            "0", charset);
//
//			if (! super.isPositive()) {
//				record[endPos] = (byte) (record[endPos] & andByte);
//			}
//				
//			return record;
	    }
	    
	    if (! super.isPositive()) {
	    	val = Conversion.toZoned(val);
	    }
	    copyRightJust(record, val,
	            position - 1, field.getLen(),
	            "0", charset);
	    return record;
    }
    
    
    @Override
	public String formatValueForRecord(IFieldDetail field, String value) {
        String val = checkValue(field, toNumberString(value));
	    String charset = field.getFontName();
	    if (Conversion.isSingleByteEbcidic(charset)) {
	    	int len = Math.max(field.getLen(), val.length());

			byte[] record = new byte[len];
	    	byteLevelAssign(record, 1, field, val);
	    	String s = Conversion.toString(record, charset);
	    	if (field.isFixedFormat()) {
	    		return s;
	    	}
	    	return Conversion.numTrim(s);
	    }
	    
	    if (! super.isPositive()) {
	    	val = Conversion.toZoned(val);
	    }
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
		int endPos = len + position - 2;
		if (! field.isFixedFormat()) {
			endPos = record.length - 1;
			len = record.length - position + 1;
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
	            position - 1, len,
	            "0", field.getFontName());

		if (! super.isPositive()) {
			record[endPos] = (byte) (record[endPos] & andByte);
		}
    }
}
