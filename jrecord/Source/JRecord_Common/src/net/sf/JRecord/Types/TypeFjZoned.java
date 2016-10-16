/**
 * @Author Jean-Francois Gagnon
 * Created on 6/26/2006
 *
 * Purpose:
 *    Add support for Fujitsu Type Zoned Decimal
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

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFieldDetail;

/**
 * Fujitsu Type Zoned Decimal type.
 *
 * @author Jean-Francois Gagnon
 *
 */
public class TypeFjZoned extends TypeNum {

	private static int positiveFjDiff = '@' - '0';
	private static int negativeFjDiff = 'P' - '0';

	private final boolean overtypePositive;
    /**
     * Define Fujitsu Zoned Decimal Type
     *
     * <p>This class is the interface between the raw data in the file
     * and what is to be displayed on the screen for Fujitsu Zoned Decimal
     * fields.
     */
    public TypeFjZoned(boolean overtypePositive) {
        super(false, true, true, false, false, false, false);
        this.overtypePositive = overtypePositive;
    }


    /**
     * @see net.sf.JRecord.Types.Type#getField(byte[], int, IFieldDetail)
     */
    public Object getField(byte[] record,
            final int position,
			final IFieldDetail field) {
        return addDecimalPoint(
                	fromFjZoned(super.getFieldText(record, position, field)),
                	field.getDecimal());
    }


    /**
     * @see net.sf.JRecord.Types.Type#setField(byte[], int, IFieldDetail, Object)
     */
    public byte[] setField(byte[] record,
            final int position,
			final IFieldDetail field,
			Object value) {

	    copyRightJust(record, formatValueForRecord(field, toNumberString(value)),
	            position - 1, field.getLen(),
	            "0", field.getFontName());
	    return record;
    }

    
	@Override
	public String formatValueForRecord(IFieldDetail field, String value) {
		String val = toAsciiZoned(checkValue(field, toNumberString(value)));
		if (field.isFixedFormat()) {
			return Conversion.padFront(val, field.getLen() - val.length(), '0');
		}
		return val;
	}


	/**
	 * Convert a num to a Fujitsu/Ascii Zoned Number String
	 *
	 * @param num  Numeric string
	 *
	 * @return number-string
	 */
	private String toAsciiZoned(String num) {

		String ret;
		if (num == null || (ret = num.trim()).length() == 0 || ret.equals("-") || ret.equals("+")) {
			return "";
		}


        char lastChar = ret.substring(ret.length() - 1).charAt(0);

		if (num.startsWith("-")) {
            ret = ret.substring(1);

			if (lastChar < '0' || lastChar > '9') {
				// throw ...
			} else {
				lastChar = (char) (lastChar + negativeFjDiff);
			}

        } else {
            if (num.startsWith("+")) {
                ret = ret.substring(1);
            }

            if (lastChar < '0' || lastChar > '9') {
                // throw ...
            } else if (overtypePositive) {
                lastChar = (char) (lastChar + positiveFjDiff);
            }

		}

        ret = ret.substring(0, ret.length() - 1) + lastChar;

		return ret;
	}


    /**
     * Convert a Fujitsu Zoned Number String to a number string
     *
     * @param numZoned Zoned Numeric string
     *
     * @return number-string
     */
    private String fromFjZoned(String numZoned) {
        String ret;
        String sign = "";
        char lastChar;

        if (numZoned == null || (ret = numZoned.trim()).length() == 0 || ret.equals("-")) {
            return "";
        }

        lastChar = ret.substring(ret.length() - 1).toUpperCase().charAt(0);

        switch (lastChar) {
            case '@':
            case 'A':
            case 'B':
            case 'C':
            case 'D':
            case 'E':
            case 'F':
            case 'G':
            case 'H':
            case 'I':
                lastChar = (char) (lastChar - positiveFjDiff);
            break;
            case 'P':
            case 'Q':
            case 'R':
            case 'S':
            case 'T':
            case 'U':
            case 'V':
            case 'W':
            case 'X':
            case 'Y':
                sign = "-";
                lastChar = (char) (lastChar - negativeFjDiff);
            default:
        }
        ret = sign + ret.substring(0, ret.length() - 1) + lastChar;

        return ret;
    }
}
