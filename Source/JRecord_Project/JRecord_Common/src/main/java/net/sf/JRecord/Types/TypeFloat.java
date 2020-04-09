/*
 * @Author Bruce Martin
 * Created on 5/09/2005 for RecordEditor Version 0.55
 *
 * Purpose:
 *   Float / Double field Type
 *
 * Changes
 * # Version 0.56 Bruce Martin 2007/01/16
 *   - remove unused field val
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

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFieldDetail;

/**
 * Float / Double field Type
 *
 * <p>This class is the interface between the raw data in the file
 * and what is to be displayed on the screen for Float or Double fields
 *
 * @author Bruce Martin
 *
 * @version 0.55
 */
public class TypeFloat extends TypeNum {

	private static final int  LENGTH_OF_DOUBLE  =  8;
	private static final int  LENGTH_OF_FLOAT   =  4;


    /**
     *  Float / Double field Type
     *
     * <p>This class is the interface between the raw data in the file
     * and what is to be displayed on the screen for Float or Double fields
     */
    public TypeFloat() {
        super(true, false, false, false, true, true, false);
    }

    /**
     * @see net.sf.JRecord.Types.Type#getField(byte[], int, IFieldDetail)
     */
    public Object getField(byte[] record,
            final int position,
			final IFieldDetail field) {
         Object val = "";

         int fldLength = field.getLen();
         int pos = position - 1;

         if (fldLength == LENGTH_OF_FLOAT) {
        	 val = Float.toString(Float.intBitsToFloat(Conversion.getLittleEndianBigInt(record, pos, pos + fldLength).intValue()));
//             val = Float.toString(
//                     Float.intBitsToFloat(
//                        Conversion.getBigInt(record, pos, fldLength).intValue()));
         } else if (fldLength == LENGTH_OF_DOUBLE) {
        	 val = Double.toString(Double.longBitsToDouble(Conversion.getLittleEndianBigInt(record, pos, pos + fldLength).longValue()));
//             val = Double.toString(
//                 Double.longBitsToDouble(
//                     Conversion.getBigInt(record, pos, fldLength).longValue()));
         }

         return "" + val;
    }


    /**
     * @see net.sf.JRecord.Types.Type#setField(byte[], int, IFieldDetail, Object)
     */
    public byte[] setField(byte[] record,
            final int position,
			final IFieldDetail field,
			Object value) {

        int len = field.getLen();
        int pos = position - 1;
        double doubleVal  = getBigDecimal(field, toNumberString(value)).doubleValue();

	    if (len == LENGTH_OF_FLOAT) {
	        long l = Float.floatToRawIntBits((float) doubleVal);
	        Conversion.setLongLow2High(record, pos, len, l, true);
	    } else if (len == LENGTH_OF_DOUBLE) {
	        long l = Double.doubleToRawLongBits(doubleVal);
	        Conversion.setLongLow2High(record, pos, len, l, true);
	    }

	    return record;
    }
}
