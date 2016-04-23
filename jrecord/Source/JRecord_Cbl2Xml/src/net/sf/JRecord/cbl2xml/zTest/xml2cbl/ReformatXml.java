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

package net.sf.JRecord.cbl2xml.zTest.xml2cbl;

import net.sf.JRecord.Option.IReformatFieldNames;

public class ReformatXml {
	
    public static final int ASIS = 0;
    public static final int UPPER = 1;
    public static final int LOWER = 2;



	
	public static String reformaXml(int format, String xmlStr) {
		String s = xmlStr;
		switch (format) {
		case IReformatFieldNames.RO_CAMEL_CASE: s = toCamelCase(xmlStr);	break;
		case IReformatFieldNames.RO_UNDERSCORE:	s = toUnderscore(xmlStr);	break;
		}
		
		return s;
	}
	
	
	public static String toUnderscore(String xmlStr) {
		StringBuilder b = new StringBuilder(xmlStr);
		boolean inTag = false;
		char c;
		
		for (int i = 0; i < xmlStr.length(); i++) {
			c = b.charAt(i);
			switch (c) {
			case '<':
			case '?':
			case '>': inTag = c == '<';				break;
			case '-': 
				if (inTag) {
					b.setCharAt(i, '_');
				}
				break;
			
			}
		}
		
		return b.toString();
	}
	
	
	public static String toCamelCase(String xmlStr) {
		StringBuilder b = new StringBuilder(xmlStr.length());
		String uc = xmlStr.toUpperCase();
		String lc = xmlStr.toLowerCase();
		boolean inTag = false, tagStart = false;
		int caseUpd = ASIS;
		char c, ch;
		
		for (int i = 0; i < xmlStr.length(); i++) {
			c = xmlStr.charAt(i);
			switch (c) {
			case '<':
				inTag = true;
				caseUpd = LOWER;
				b.append(c);
				break;
			case '?':
			case '>': 
				inTag = false;
				caseUpd = ASIS;
				b.append(c);
				break;
			case '-': 
				if (inTag) {
					caseUpd = UPPER;
					break;
				}
				b.append('-');
				break;
			case ' ':
				caseUpd = ASIS; 
			default:
				ch = c;
				if (tagStart
				&&   (		c == 'C' && xmlStr.substring(i).startsWith("CobolData>")
						||  c == 'L' && xmlStr.substring(i).startsWith("Line>"))
				) {
					caseUpd = ASIS;
				}
				switch (caseUpd) {
				case LOWER: ch = lc.charAt(i);			break;
				case UPPER:
					ch = uc.charAt(i);
					caseUpd = LOWER;
				}
				b.append(ch);
			}
			tagStart = (c =='<') || (c == '/');
		}
		
		return b.toString();
	}

}
