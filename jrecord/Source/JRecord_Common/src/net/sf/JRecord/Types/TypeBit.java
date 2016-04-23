/*
 * @Author Bruce Martin
 * Created on 6/09/2005
 *
 * Purpose: Bit Type definition
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

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;

/**
 * Bit Type  - displays the raw bits
 *
 * @author Bruce Martin
 *
 * @version 0.55
 */
public class TypeBit extends TypeChar {

    /**
     * Bit Type definition
     * <p>This class is the interface between the raw data in the file
     * and what is to be displayed on the screen for Bit representation
     *
     */
    public TypeBit() {
        super(true, true, true);
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

	    return Conversion.numTrim(Conversion.getBitField(record, pos, min));
    }


    /**
     * @see net.sf.JRecord.Types.Type#setField(byte[], int, IFieldDetail, Object)
     */
    public byte[] setField(byte[] record,
              final int position,
			  final IFieldDetail field,
			  Object value) {

		int pos = position - 1;
		int len = field.getLen();
		BigInteger v;
		if (value == CommonBits.NULL_VALUE || value == null || "".equals(value) || "0".equals(value))  {
			v = BigInteger.ZERO;
		} else {
			String val = value.toString();
			v = new BigInteger(val, 2);
		}

        Conversion.setBigInt(record, pos, len, v, true);
        return record;
	}


    /**
     * @see net.sf.JRecord.Types.Type#formatValueForRecord(IFieldDetail, String)
     */
    public String formatValueForRecord(IFieldDetail field, String val) {
        try {
        	new BigInteger(val, 2);
        } catch (final Exception ex) {
            throw new RecordException("Invalid Bit String: {0}", ex.getMessage());
        }
        return val;
    }
}
