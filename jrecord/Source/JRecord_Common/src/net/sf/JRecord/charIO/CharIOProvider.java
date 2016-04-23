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
      
package net.sf.JRecord.charIO;

import net.sf.JRecord.Common.Constants;

public class CharIOProvider {

	public ICharWriter getWriter(int id, String font, String eol, int length) {
		ICharWriter ret = null;
		switch (id) {
		case Constants.IO_FIXED_LENGTH_CHAR: ret = new FixedLengthCharWriter(length, font);		break;
		case Constants.IO_UNICODE_TEXT:
		case Constants.IO_CSV_NAME_1ST_LINE:
		case Constants.IO_CSV: 				 ret = new StandardCharWriter(eol, font);			break;
		}
		
		return ret;
	}
}
