/*
 * @Author Bruce Martin
 * Created on 6/09/2005
 *
 * Purpose: Type for Binary Integer - Big Endian (high to low format)
 *
 * Changes
 * # Version 0.56 Bruce Martin 2007/01/16
 *   - use isPostive method (instead of positive variable)
 *
 * # Version 0.60 Bruce Martin 2007/02/16
 *   - Starting to seperate the Record package out from the RecordEditor
 *     so that it can be used seperately. So classes have been moved
 *     to the record package (ie RecordException + new Constant interface)
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
 * Type for Binary Integers - Big Endian (high to low format)
 *
 * @author Bruce Martin
 *
 * @version 0.55
 */
public class TypeBinBigEndian extends TypeNum {

	private final boolean positiveStorage;

    /**
     * Type for Binary Integers - Big Endian (high to low format)
     * <p>This class is the interface between the raw data in the file
     * and what is to be displayed on the screen for Big-Endian
     * binary integers.
     *
     * @param isPositive wether it is a positive integer
     */
    public TypeBinBigEndian(final boolean isPositive) {
        super(false, true, true, isPositive, true, true, false);
        positiveStorage = isPositive;
    }

    /**
     * Type for Binary Integers - Big Endian (high to low format)
     * <p>This class is the interface between the raw data in the file
     * and what is to be displayed on the screen for Big-Endian
     * binary integers.
     *
     * @param isPositive wether it is a positive integer
     */
    public TypeBinBigEndian(final boolean isPositive, boolean positiveStore) {
        super(false, true, true, isPositive, true, true, false);
        this.positiveStorage = positiveStore;
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

        String s;
        if (pos >= min) {
        	s = "0";
        } else if (positiveStorage) {
        	s = Conversion.getPositiveBigInt(record, pos, min - pos).toString();
        } else {
        	s = Conversion.getBigInt(record, pos, min - pos).toString();
        }

        s = addDecimalPoint(s, field.getDecimal());

        return s;
    }


    /**
     * @see net.sf.JRecord.Types.Type#setField(byte[], int, IFieldDetail, Object)
     */
    public byte[] setField(byte[] record,
            			 final int position,
            			 final IFieldDetail field, Object value) {
		
        Conversion.setBigInt(record, position - 1, field.getLen(), formatAsBigInt(field, value), positiveStorage);
        return record;
    }
}
