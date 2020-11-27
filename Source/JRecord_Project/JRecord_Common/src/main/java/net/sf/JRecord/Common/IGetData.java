/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
 *    
 *                 Author: Jean-Francois Gagnon
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2005, Bruce Martin / Jean-Francois Gagnon, All Rights Reserved.
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
 * Any class that has a getData() method
 * 
 * @author Bruce Martin
 *
 */
public interface IGetData {
	/**
	 * @return Returns the record (line) as an array of bytes.
	 */
	public abstract byte[] getData();
}
