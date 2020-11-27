/*
 * @Author Bruce Martin
 * Created on 6/09/2005
 *
 * Purpose:
 *    Mainframe Packed Decimal Type.
 *
  * Changes
 * # Version 0.56 Bruce Martin 2007/01/16
 *   - use BASE_16 constant (instead of hard coded 16)
 *
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

import java.math.BigInteger;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFieldDetail;

/**
 * Mainframe Packed Decimal Type.
 *
 * @author Bruce Martin
 *
 * @version 0.55
 */
public class TypePackedDecimal extends TypeNum {


    /**
     * Define a Mainframe Packed Decimal Type.
     *
     * <p>This class is the interface between the raw data in the file
     * and what is to be displayed on the screen for Mainframe Packed Decimal
     * fields.
     */
    public TypePackedDecimal() {
        super(false, true, true, false, true, false, false);
    }

    public TypePackedDecimal(boolean positive) {
        super(false, true, true, positive, true, false, false);
    }

    /**
     * @see net.sf.JRecord.Types.Type#getField(byte[], int, IFieldDetail)
     */
    public Object getField(byte[] record,
            final int position,
			final IFieldDetail field) {
        int pos = position - 1;
        int end = position + field.getLen() - 1;
	    int min = java.lang.Math.min(end, record.length);
	    int fldLength = min - pos;

        String s = Conversion.getMainframePackedDecimal(record,
                								 pos,
                								 fldLength);

        return addDecimalPoint(s, field.getDecimal());
    }


    /**
     * @see net.sf.JRecord.Types.Type#setField(byte[], int, IFieldDetail, Object)
     */
    @Override
    public byte[] setField(byte[] record,
            final int position,
			final IFieldDetail field,
			Object value) {

		int pos = position - 1;
		int len = field.getLen();

        String val = checkValue(field, toNumberString(value));
	    if (val.startsWith("-")) {
	        val = val.substring(1) + "D";
	    } else if (isPositive()) {
	    	val += "F";
	    } else {
	        val += "C";
	    }

	    //if (val.length() < 17) {
	    Conversion.setBigInt(record, pos, len, new BigInteger(val, BASE_16), true);
//	    } else {
//	    	int split = val.length() - 16;
//
//	    	Conversion.setLong(record, pos, len, Long.parseLong(val.substring(split), BASE_16), true);
//	    	Conversion.setLong(record, pos, len-8, Long.parseLong(val.substring(0, split), BASE_16), true);
////	    	System.out.println("Comp3 Error: " + split + " " + val + "  >" + val.substring(0, split) + " " + val.substring(split)
////	    			+ " ~~>> " +  Conversion.getMainframePackedDecimal(record,
////							 pos,
////							 len));
//	    }
	    return record;
    }
}
