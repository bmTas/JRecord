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

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;

public class TypeNumAnyDecimal extends TypeNum {

	public TypeNumAnyDecimal(boolean isPositive, boolean couldBeEmpty) {
		super(false, false, true, isPositive, false, false, couldBeEmpty);
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.TypeNum#addDecimalPoint(java.lang.String, int)
	 */
	@Override
	protected String addDecimalPoint(String s, int decimal) {
		return s == null ? "" : s.trim();
	}



	@Override
	public byte[] setField(byte[] record, int position, IFieldDetail field,
			Object value) {
		return setFieldToVal(record, position, field, formatValueForRecord(field, toNumberString(value)));
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.TypeNum#formatValueForRecord(net.sf.JRecord.Common.FieldDetail, java.lang.String)
	 */
	@Override
	public String formatValueForRecord(IFieldDetail field, String val) {

		if (super.couldBeEmpty && (val == null || val.trim().length() == 0)) {
			return "";
		}

        try {
            new BigDecimal(Conversion.numTrim(val));
        } catch (final Exception ex) {
            throw new RecordException("Invalid Number: " + val + " >" + Conversion.numTrim(val) + "<" , ex.getMessage());
        }

	    if (isPositive() && val.indexOf('-') >= 0) {
	        throw new RecordException("Only positive numbers are allowed: " + val);
	    }

	    return val.trim();
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.TypeNum#hasFloatingDecimal()
	 */
	@Override
	public boolean hasFloatingDecimal() {
		return true;
	}
	
	
}
