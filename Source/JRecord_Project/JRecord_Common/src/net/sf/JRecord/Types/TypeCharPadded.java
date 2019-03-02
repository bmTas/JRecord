/*
 * @Author Bruce Martin
 * Created on 9/01/2007 for RecordEditor Version 0.56
 *
 * Purpose: null padded char type
 *
 * # Version 0.60 Bruce Martin 2007/02/16
 *   - removed procedure getFieldEnd because it is now identical
 *     to the method in the parent class
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

/**
 * create a null padded char type
 *
 * @author Bruce Martin
 *
 * @version 0.56
 */
public class TypeCharPadded extends TypeChar {

    private byte padByte;


    /**
     * create a null padded char type
     */
    public TypeCharPadded() {
        this((byte) 0);
    }

    /**
     * create a  padded char type
     * @param b value to pad the field with
     */
    public TypeCharPadded(final byte b) {
        super(true, true);
        padByte = b;
    }

//    /**
//     * @see net.sf.RecordEditor.record.types.TypeChar#getFieldEnd(net.sf.RecordEditor.record.types.FieldDetail, byte[])
//     */
//    protected int getFieldEnd(FieldDetail currField, byte[] record) {
//        int ret = java.lang.Math.min(currField.getEnd(), record.length);
//
//        while (ret > 0 && (record[ret - 1] == padByte)) {
//            ret -= 1;
//        }
//
//        return ret;
//    }

    /**
     * @see net.sf.JRecord.Types.TypeChar#getPadByte(java.lang.String)
     */
    protected byte getPadByte(String font) {
        return padByte;
    }
}
