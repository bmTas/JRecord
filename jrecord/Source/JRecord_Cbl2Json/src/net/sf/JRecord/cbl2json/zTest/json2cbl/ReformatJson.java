/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Cbl2Xml
 *    
 *    Sub-Project purpose: Convert Cobol Data files to / from Xml
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

package net.sf.JRecord.cbl2json.zTest.json2cbl;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Option.IReformatFieldNames;

public class ReformatJson {
	
    public static final int ASIS = 0;
    public static final int UPPER = 1;
    public static final int LOWER = 2;



	
	public static String reformatJson(int format, String jsonStr) {
		String s = jsonStr;
		switch (format) {
		case IReformatFieldNames.RO_CAMEL_CASE: s = toCamelCase(jsonStr);	break;
		case IReformatFieldNames.RO_LEAVE_ASIS:	s = toMinus(jsonStr);	break;
		
		}
		
		return s;
	}
	
	
	public static String toMinus(String jsonStr) {
		return Conversion.replace(jsonStr, "_", "-").toString();		
	}
	
	
	public static String toCamelCase(String jsonStr) {
		StringBuilder b = new StringBuilder(jsonStr.length());
//		String uc = jsonStr.toUpperCase();
//		String lc = jsonStr.toLowerCase();
		int caseUpd = ASIS;
		char c;
		
		for (int i = 0; i < jsonStr.length(); i++) {
			c = jsonStr.charAt(i);
			switch (c) {
			case '_': 
				
					caseUpd = UPPER;
					break;
			case ' ':
				caseUpd = ASIS; 
			default:
			}
			
		}
		
		return b.toString();
	}

}
