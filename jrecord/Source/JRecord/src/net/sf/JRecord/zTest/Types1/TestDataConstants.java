/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
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

package net.sf.JRecord.zTest.Types1;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;

public class TestDataConstants {

	public static String getTestDataFileName(String charset) {
		String s = charset;
		if (s.length() == 0 || Conversion.DEFAULT_ASCII_CHARSET.equals(s)) {
			s = "Std";
		}

		return TestDataConstants.class.getResource( "TestData_" + s +".txt").getFile();
	}
	
	
	/**
	 * Create field
	 * 
	 * @param pos field position
	 * @param len field length
	 * @param decimal number of decimal places
	 * @param type type of the field
	 * @param charset character set.
	 * 
	 * @return the requested field
	 */
	public static FieldDetail getType(int pos, int len, int decimal, int type, String charset) {


		FieldDetail field = new FieldDetail("", "", type, decimal, charset, -1, "");

		if (len > 0) {
			field.setPosLen(pos, len);
		} else {
			field.setPosOnly(pos);
		}

		return field;
	}


}
