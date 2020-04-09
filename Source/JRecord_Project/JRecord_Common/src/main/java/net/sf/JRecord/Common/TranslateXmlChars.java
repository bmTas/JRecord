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
      
package net.sf.JRecord.Common;

public class TranslateXmlChars {

	public static String replaceXmlCharsStr(String in) {
		return replaceXmlChars(new StringBuilder(in)).toString();
	}
	
	public static StringBuilder replaceXmlChars(StringBuilder in) {
        replace(in, "&", "&amp;");
        replace(in, "<", "&lt;");
        replace(in, ">", "&gt;");
        replace(in, "\"", "&quot;");
 //       replace(in, "\t", "&#009;");
        
        return in;
	}
	
	/**
     * Replaces on string with another in a String bugffer
     *
     * @param in String buffer to be updated
     * @param from search string 
     * @param to replacement string
     */
    public static void replace(StringBuilder in, String from, String to) {
        int start;
        int fromLen = from.length();

        start = in.indexOf(from, 0);
        while (start >= 0) {
            in.replace(start, start + fromLen, to);
            start = in.indexOf(from, start + to.length());
        }
    }
}
