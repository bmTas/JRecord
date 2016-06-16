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

package net.sf.JRecord.cgen.def;

import net.sf.JRecord.Common.IFieldDetail;

public interface IArrayAnyDimension {

	/**
	 * Get a JRecord-Field definition for a specific array index. 
	 * @param indexs the array index's
	 * @return requested field definition
	 */
	public abstract IFieldDetail getField(int...indexs);
	
	/**
	 * Get the maximum number of items allowed for the specified index.
	 * For occurs depending arrays you should check the occurs depending 
	 * value.
	 *  
	 * @param indexNumber Index number
	 * @return maximum number of items allowed for the specified index
	 */
	public abstract int getArrayLength(int indexNumber);

	/**
	 * 
	 * @param indexNumber Index number
	 * @return size of array element for this index
	 */
	public abstract int getArrayElementSize(int indexNumber);
	
	/**
	 * Get the number of Array index's
	 * @return number of Array index's
	 */
	public abstract int getIndexCount();
}
