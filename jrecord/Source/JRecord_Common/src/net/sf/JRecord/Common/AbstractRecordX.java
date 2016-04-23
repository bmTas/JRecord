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
 * Extended Abstract Record (Record-Schema)  where you can get the Field Definition and Field Count
 * @author Bruce Martin
 *
 * @param <FieldDefinition>
 */
public interface AbstractRecordX<FieldDefinition extends IFieldDetail> extends AbstractRecord, IGetFieldByName {

	/**
	 * Get a specific field definition
	 * @param idx index of the required field
	 * @return requested field
	 */
	public abstract FieldDefinition getField(int idx);

	/**
	 * get the number of fields in the record
	 *
	 * @return the number of fields in the record
	 */
	public abstract int getFieldCount();


	/**
	 * Get a specific field definition (using the field name)
	 *
	 * @param fieldName record name being searched for
	 *
	 * @return index of the record
	 */
	public abstract FieldDefinition getField(String fieldName);

	/**
	 * Get a field (by group names / field name)
	 * @param fieldNames
	 * @return request field
	 */
	public abstract IFieldDetail getGroupField(String... fieldNames);

}