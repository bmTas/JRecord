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

public class TypeCharRestOfFixedRecord extends TypeChar {

	public TypeCharRestOfFixedRecord() {
		super(true, true, false);
	}

	/**
	 * @see net.sf.JRecord.Types.TypeChar#getFieldEnd(int, IFieldDetail, byte[])
	 */
	@Override
	protected int getFieldEnd(int position, IFieldDetail currField, byte[] record) {
		return record.length;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.TypeChar#setField(byte[], int, net.sf.JRecord.Common.FieldDetail, java.lang.Object)
	 */
	@Override
	public byte[] setField(byte[] record, int position, IFieldDetail field,
			Object value) {
        String val  = value.toString();
		String font = field.getFontName();
		int pos = position - 1;
		int len = record.length - pos;


	    byte[] byteVal = getBytes(val, font);
		if (val.length() >= len) {
			System.arraycopy(byteVal, 0, record, pos, len);
		} else {
			System.arraycopy(byteVal, 0, record, pos, val.length());
			//padWith(record, pos + val.length(), len - val.length(), " ", font);
			padByte(record, pos + val.length(), len - val.length(), getPadByte(font));
		}
		return record;
	}


}
