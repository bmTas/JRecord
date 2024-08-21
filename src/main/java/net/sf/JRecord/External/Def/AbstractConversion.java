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
      
package net.sf.JRecord.External.Def;


/**
 * This interface describes a class that will convert a Type/Format from the
 * external String representation to the internal integer value
 *
 *  @author Bruce Martin
 *
 */
public interface AbstractConversion {

	public static int USE_DEFAULT_IDX = -121;

	/**
	 * Convert a String to a Type value
	 * @param idx db index
	 * @param type Type (String)
	 *
	 * @return integer type value
	 */
	public abstract int getType(int idx, String type);


	/**
	 * Convert a String to a Formay value
	 * @param idx db index
	 * @param format Format (String)
	 *
	 * @return format
	 */
	public abstract int getFormat(int idx, String format);


	/**
	 * Convert type to a string
	 * @param idx db index
	 * @param type type Id
	 * @return Type as a String
	 */
	public abstract String getTypeAsString(int idx, int type);

	/**
	 *
	 * @param idx db index
	 * @param type type Id
	 * @return wether it is valid
	 */
	public abstract boolean isValid(int idx, int type);

	/**
	 * Convert format to a string
	 * @param idx db index
	 * @return format as a String
	 */
	public abstract String getFormatAsString(int idx, int format);


	/**
	 * Get Dialect Name for a dialect code
	 * @param key dialect-code
	 * @return Dialect name
	 */
	public abstract String getDialectName(int key);

	/**
	 * Get Dialect Code from a dialect name
	 * @param name dialect name
	 * @return Dialect Code
	 */
	public abstract int getDialect(String name);


}
