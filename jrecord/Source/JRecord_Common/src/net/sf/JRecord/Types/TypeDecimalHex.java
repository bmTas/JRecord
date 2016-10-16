/*
 * @Author Bruce Martin
 * Created on 6/09/2005
 *
 * Purpose: Decimal or Hex Type
 *   This class is the interface between the raw data in the file
 * and what is to be displayed on the screen for Decimal or Hex fields
 *
 * Changes
 * # Version 0.56 Bruce Martin 2007/01/16
 *   - use BASE_16 constant (instead of hard coded 16)
 *
 * # Version 0.60 Bruce Martin 2007/02/16
 *   - Starting to seperate the Record package out from the RecordEditor
 *     so that it can be used seperately. So classes have been moved
 *     to the record package (ie RecordException + new Constant interface
 *
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

/**
 * Define a Decimal or Hex Type
 * <p>This class is the interface between the raw data in the file
 * and what is to be displayed on the screen for Decimal or Hex fields
 *
 * @author Bruce Martin
 *
 * @version 0.55
 */
public class TypeDecimalHex extends TypeNum {

    private boolean isNumeric;

    /**
     * Define a Decimal or Hex Type
     * <p>This class is the interface between the raw data in the file
     * and what is to be displayed on the screen for Decimal or Hex fields
     *
     * @param typeId type id, possible values are
     * <ol compact>
     *   <li>Type.ftDecimal
     *   <li>Type.ftHex
     * </ol>
     */
    public TypeDecimalHex(final int typeId) {
        super(false, true, true, true, true, true, false);

        isNumeric = typeId == Type.ftDecimal;
        setNumeric(isNumeric);
    }


    /**
     * @see net.sf.JRecord.Types.Type#getField(byte[], int, IFieldDetail)
     */
    public Object getField(byte[] record,
              final int position,
			  final IFieldDetail field) {
        String s; 
        int endOfField = record.length;
        int end = position + field.getLen() - 1;
		if (field.getType() != Type.ftCharRestOfRecord && end < endOfField) {
        	endOfField = end;
        }
        
        s = Conversion.getDecimal(record, position - 1, endOfField);

        if (isNumeric) {
            s = addDecimalPoint(s, field.getDecimal());
        }
        return s;
    }


    /**
     * @see net.sf.JRecord.Types.Type#setField(byte[], int, IFieldDetail, Object)
     */
    public byte[] setField(byte[] record,
            final int position,
			final IFieldDetail field,
			final Object value) {

		int pos = position - 1;
		int len = field.getLen();
        String val = toNumberString(value);
        
        if (field.getType() == Type.ftCharRestOfRecord) {
        	len = record.length - field.getPos();
        }

        if (isNumeric) { // ie not a hex field
            val = checkValue(field, val);
        } else if (value == CommonBits.NULL_VALUE) {
        	val = "0";
        }
        Conversion.setBigInt(record, pos, len, new BigInteger(val, BASE_16), true);

        return record;
    }
}
