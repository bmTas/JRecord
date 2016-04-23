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

package net.sf.JRecord.schema;

import net.sf.JRecord.Common.IFieldDetail;

/**
 * Retrieve field Description using a Record_Name / Field_Name
 * 
 * @author Bruce Martin
 *
 */
public interface IGetRecordFieldByName {
	
	/**
	 * Get a field description
	 * 
	 * @param recordName Record Name the field is in
	 * @param fieldName field Name
	 * @param indexs any array index's
	 * 
	 * @return the field
	 */
	public abstract IFieldDetail getField(String recordName, String fieldName, int[] indexs);
}
