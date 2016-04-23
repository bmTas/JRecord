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

import java.text.MessageFormat;


public class BasicTranslation implements ITranslation {

	private static ITranslation trans = new BasicTranslation();

	@Override
	public final String convert(int type, String s, String param) {
		return MessageFormat.format(convert(type, s), param);
	}

	@Override
	public final String convert(int type, String s, Object[] params) {
		return MessageFormat.format(convert(type, s), params);
	}

	@Override
	public final String convert(int type, String s) {
		if (s == null || "".equals(s)) return s;

		return convert(s);
	}


	/**
	 * @see net.sf.JRecord.Common.ITranslation#convert(java.lang.String)
	 */
	@Override
	public final String convert(String s) {
		return convert(s, s);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Common.ITranslation#convert(java.lang.String, java.lang.String)
	 */
	@Override
	public String convert(String s, String defaultStr) {
		return defaultStr;
	}

	/**
	 * @return the trans
	 */
	public static ITranslation getTrans() {
		return trans;
	}

	/**
	 * @param trans the trans to set
	 */
	public static void setTrans(ITranslation trans) {
		BasicTranslation.trans = trans;
	}



}
