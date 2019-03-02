/**
 * @Author Jean-Francois Gagnon
 * Created on 6/26/2006
 *
 * Purpose:
 *    Add support for Sign Separate Display Type numeric
 *
 * # Version 0.60 Bruce Martin 2007/02/16
 *   - Starting to seperate the Record package out from the RecordEditor
 *     so that it can be used seperately. So classes have been moved
 *     to the record package (ie RecordException + new Constant interface

 */
/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
 *    
 *                 Author: Jean-Francois Gagnon
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2006, Jean-Francois Gagnon / Bruce Martin, All Rights Reserved.
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

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;

/**
 * Sign Seperate numeric (ie editted numeric in Cobol)
 *
 * @author Jean-Francois Gagnon
 *
 */
public class TypeSignSeparate extends TypeNum {

    private final boolean isLeadingSign;
    private final boolean isActualDecimal;
    
    /**
     * Define mainframe Zoned Decimal Type
     *
     * <p>This class is the interface between the raw data in the file
     * and what is to be displayed on the screen for Sign Separate Display numeric
     * fields.
     *
     * @param typeId Type Identifier
     */
    public TypeSignSeparate(final int typeId) {
        super(false, true, true, false, false, false, false);

        isLeadingSign = (typeId == Type.ftSignSeparateLead || typeId == Type.ftSignSepLeadActualDecimal);
        isActualDecimal = (typeId == Type.ftSignSepLeadActualDecimal || typeId == Type.ftSignSepTrailActualDecimal);

    }


    /**
     * @see net.sf.JRecord.Types.Type#getField(byte[], int, IFieldDetail)
     */
    public Object getField(byte[] record,
            final int position,
			final IFieldDetail field) {
        return addDecimalPoint(
                	fromSignSeparate(super.getFieldText(record, position, field)),
                	field.getDecimal());
    }


    /**
     * @see net.sf.JRecord.Types.Type#setField(byte[], int, IFieldDetail, Object)
     */
    public byte[] setField(byte[] record,
            final int position,
			final IFieldDetail field,
			Object value) {

        String val = checkValue(field, toNumberString(value));
        copyRightJust(record, toSignSeparate(val, field),
	            position - 1, field.getLen(),
	            "0", field.getFontName());
	    return record;
    }

	@Override
	public String formatValueForRecord(IFieldDetail field, String value) {
		return toSignSeparate(checkValue(field, toNumberString(value)), field);
	}


	/**
	 * Convert a num to a Sign Separate String
	 *
	 * @param num  Numeric string
	 * @param field  Field Detail
	 *
	 * @return number-string
	 * 
	 */
	private String toSignSeparate(String num,
                                  IFieldDetail field) {


		if (num == null || num.length() == 0 || num.equals("-") || num.equals("+")) {
			// throw ...
			return paddingString("+", field.getLen(), '0', !isLeadingSign);
		}
		String ret = num.trim();
        String sign = "";

		if (num.startsWith("-")) {
            sign = "-";
            ret = ret.substring(1);
        } else {
            sign = "+";
            if (num.startsWith("+")) {
                ret = ret.substring(1);
            }
		}

  
        int len = field.getLen() - 1;
        int decimal = field.getDecimal();
		if (decimal > 0 && isActualDecimal && len > 2) {
        	ret = paddingString(ret, len - 1, '0', true);
        	ret = ret.substring(0, ret.length() - decimal) + "." + 
        		  ret.substring(ret.length() - decimal);
        } else {
        	ret = paddingString(ret, len, '0', true);
        }

        if ("+".equals(sign) && ret.length() == field.getLen()) {
        	
        } else if (isLeadingSign) {
            ret = sign + ret;
        } else {
            ret = ret + sign;
        }

        if (ret.length() > field.getLen() && field.isFixedFormat()) {
            throw new RecordException("Value: " + ret + " is too large to fit field: " + field.getLen());
        }

		return ret;
	}

    /**
     * Convert a Sign Separate Number String to a number string
     *
     * @param numSignSeparate Zoned Numeric string
     *
     * @return number-string
     */
    private String fromSignSeparate(String numSignSeparate) {
         if (numSignSeparate == null || numSignSeparate.length() == 0 || numSignSeparate.equals("-")) {
            // throw ...
            return "";
        }

         String ret;
         String sign = "";


        ret = numSignSeparate.trim();
        if (isLeadingSign) {
            if (ret.length() > 0 && ret.charAt(0) == '+') {
            	ret = ret.substring(1);
            }
        } else {
			int lastIdx = ret.length() - 1;
			if (ret.length() > 0 && (ret.charAt(lastIdx) == '+' || ret.charAt(lastIdx) == '-')) {
			    sign = ret.substring(lastIdx);
			    ret = ret.substring(0, lastIdx);
			}
        }

        if ("-".equals(sign)) {
        	ret = sign + ret;
        }
        return ret;

    }

  /**
   * Pad a string S with a size of N with char C
   * on the left (True) or on the right(false)
   *
   * @param s String to be padded
   * @param n Desired Length
   * @param c Padding character
   * @param paddingLeft true for Left, false for Right
   *
   * @return padded String
   **/
  private String paddingString(String s, int n, char c, boolean paddingLeft) {
    StringBuffer str = new StringBuffer(s);
    int strLength  = str.length();
    if (n > 0 && n > strLength) {
      for (int i = 0; i <= n; i++) {
            if (paddingLeft) {
              if (i < n - strLength) {
                  str.insert(0, c);
              }
            } else {
              if (i > strLength) {
                  str.append(c);
              }
            }
      }
    }
    return str.toString();
  }


/**
 * @return the isLeadingSign
 */
public final boolean isLeadingSign() {
	return isLeadingSign;
}

}

