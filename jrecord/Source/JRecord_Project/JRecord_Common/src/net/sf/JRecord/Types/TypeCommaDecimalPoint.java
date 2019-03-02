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

import java.text.NumberFormat;
import java.util.Locale;

import net.sf.JRecord.Common.Conversion;

public class TypeCommaDecimalPoint extends TypeNum {



	private static final NumberFormat GERMAN_NUM_FORMAT = NumberFormat.getIntegerInstance(Locale.GERMAN);

	public TypeCommaDecimalPoint(int typeId, boolean isPositive) {
		super(typeId, isPositive, ',');
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.TypeNum#getNumberFormat()
	 */
	@Override
	protected NumberFormat getNumberFormat() {
		return GERMAN_NUM_FORMAT;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.TypeNum#addDecimalPoint(java.lang.String, int)
	 */
	@Override
	protected String addDecimalPoint(String s, int decimal) {
		//if (decimal > 0 && s.length() > decimal)  {
			s = s.trim();
			int pos = s.lastIndexOf(',');

			if (pos >= 0) {
				s =  s.substring(0, pos) + '.' + s.substring(pos + 1);
			}
		//}

		return Conversion.numTrim(s);
	}

//	/**
//	 * for Testing
//	 * @param s String to format
//	 * @param decimal number of decimal places
//	 * @return
//	 */
//	public final String addDP(String s, int decimal) {
//		return addDecimalPoint(s, decimal);
//	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.TypeChar#getDecimalChar()
	 */
	@Override
	public char getDecimalChar() {
		return ',';
	}


}
