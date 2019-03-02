/*
 * @Author Bruce Martin
 * Created on 9/01/2007 for RecordEditor Version 0.56
 *
 * Purpose:
 * 	Type for null terminated char String (ie C style string)
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

import net.sf.JRecord.Common.IFieldDetail;

/**
 * Type for null terminated char String (ie C style string)
 *
 * @author Bruce Martin
 *
 * @version 0.56
 *
 */
public class TypeCharNullTerminated extends TypeChar {

    private byte padByte = 0;


    /**
     * create a null terminated char type (ie C style string)
     */
    public TypeCharNullTerminated() {
        super(true, true);
    }

    /**
     * @see net.sf.JRecord.Types.TypeChar#getFieldEnd(int, IFieldDetail, byte[])
     */
    @Override
    protected int getFieldEnd(int position, IFieldDetail currField, byte[] record) {
        int ret = position - 1;
        int end = java.lang.Math.min(ret + currField.getLen(), record.length);

        while (ret < end && (record[ret] != padByte)) {
            ret += 1;
        }

        return ret;
    }

    /**
     * @see net.sf.JRecord.Types.TypeChar#getPadByte(String)
     * 
     */
    protected byte getPadByte(String font) {
        return padByte;
    }
}
