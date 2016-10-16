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

import java.math.BigDecimal;
import java.math.BigInteger;

import net.sf.JRecord.Common.IFieldDetail;

public class TypeRmCompPositive extends TypeNum {

	public TypeRmCompPositive() {
		super(false, true, true, true, true, true, false);
	}



	@Override
	public Object getField(byte[] record, int position, IFieldDetail currField) {
		Object ret;
		long retL = getRmComp(record, position, currField.getLen());
		
		ret = Long.valueOf(retL);
		
		if (currField.getDecimal() > 0) {
			ret = new BigDecimal(BigInteger.valueOf(retL), currField.getDecimal());
		}
		
		return ret;
	}
	
	@Override
	public byte[] setField(byte[] record, int position, IFieldDetail field,
			Object value) {
		
		String val = toNumberString(value);
			
		checkValue(field, val);
		
		return setRmComp(record, position, field.getLen(), Math.abs(getBigDecimal(field, val).longValue()));
	}

}
