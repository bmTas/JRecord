/**
 *
 */
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


/**
 * A basic Row consisting of an integer key and a String field name.
 * It is used return Key / Value pairs to the RecordEditor from JRecord.
 *
 * @author Bruce Martin
 *
 */
public class BasicKeyedField implements AbsRow {

	public int key;
	public String name;
	public Boolean valid=null;
	/**
	 * @see net.sf.JRecord.Common.AbsRow#getField(int)
	 */
	@Override
	public Object getField(int fldNum) {
		switch (fldNum) {
		case (0): return Integer.valueOf(key);
		case (1): return name;
		case (3): return valid;
		default: return null;
		}
	}

}
