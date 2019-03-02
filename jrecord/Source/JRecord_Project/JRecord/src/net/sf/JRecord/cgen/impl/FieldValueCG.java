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

package net.sf.JRecord.cgen.impl;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.IGetByteData;
import net.sf.JRecord.Details.fieldValue.BaseFieldValue;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;

/**
 * This class Retrieves a Field Value from an array of bytes
 * Its for use in CodeGen generated java classes
 * 
 * @author Bruce Martin
 *
 */
public class FieldValueCG extends BaseFieldValue {

	private final IGetByteData dataSource;
	private final IFieldDetail fieldDetails;
	private final Type type;
	
	/**
	 * This class Retrieves a Field Value from an array of bytes
     * Its for use in CodeGen generated java classes
     * 
	 * @param dataSource source of dataLine (as an array of bytes)
	 * @param fieldDetails field definition
	 */
	public FieldValueCG(IGetByteData dataSource, IFieldDetail fieldDetails) {
		super(fieldDetails);
		
		this.dataSource = dataSource;
		this.fieldDetails = fieldDetails;
		this.type = TypeManager.getInstance().getType(fieldDetails.getType());
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.FieldValue#getValue()
	 */
	@Override
	protected Object getValue() {
		return type.getField(dataSource.getData(), fieldDetails.getPos(), fieldDetails);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.BaseFieldValue#set(java.lang.Object)
	 */
	@Override
	public void set(Object value) {
		dataSource.setData(type.setField(dataSource.getData(), fieldDetails.getPos(), fieldDetails, value));
	}
}
