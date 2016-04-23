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

public class TypeCharRestOfRecord extends TypeChar {

	private static final byte[] EMPTY = {};
	
	public TypeCharRestOfRecord() {
		super(true, true, false);
	}

//	/* (non-Javadoc)
//	 * @see net.sf.JRecord.Types.TypeChar#getField(byte[], int, net.sf.JRecord.Common.FieldDetail)
//	 */
//	@Override
//	public Object getField(byte[] record, int position, FieldDetail currField) {
//		Object o = super.getField(record, position, currField);
//		//System.out.println(" ~~ " + position + " " + currField.getLen() + " - " + o);
//		return o;
//	}

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
		if (val.length() == len) {
			System.arraycopy(byteVal, 0, record, pos, len);
		} else if (pos + val.length() == 0) {
			record = EMPTY;
		} else {
			byte[] temp = new byte[pos + val.length()];
			System.arraycopy(record, 0, temp, 0, position);
			System.arraycopy(byteVal, 0, temp, pos, val.length());
			record = temp;
		}
		return record;
	}


}
