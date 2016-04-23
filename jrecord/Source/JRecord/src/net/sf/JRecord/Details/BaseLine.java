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

package net.sf.JRecord.Details;

import net.sf.JRecord.Common.IFieldDetail;

public abstract class BaseLine implements AbstractLine {

	protected LayoutDetail layout;


	/**
     * Get a fields value
     *
     * @param field field to retrieve
     *
     * @return fields Value
     */
	@Override
    public final Object getField(IFieldDetail field) {
        return getField(field.getType(), field);
    }

    /**
     * Get a fields value
     *
     * @param type type to use when getting the field
     * @param field field to retrieve
     *
     * @return fields Value
     * 
     */
    public abstract Object getField(int type, IFieldDetail field);

//	@Override
	public IFieldValue getFieldValue(IFieldDetail field) {
		return new FieldValue(this, field);
	}

	@Override
	public  IFieldValue getFieldValue(int recordIdx, int fieldIdx) {
		return new FieldValue(this, recordIdx, fieldIdx);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.IGetFieldValueByName#getFieldValue(java.lang.String)
	 */
	@Override
	public final IFieldValue getFieldValue(String fieldName) {
		IFieldDetail fieldFromName = layout.getFieldFromName(fieldName);
		
		if (fieldFromName == null) {
			throw new RuntimeException("Field: \"" + fieldName +"\" does not exist !!!");
		}
		
		return  getFieldValue(fieldFromName);
	}



	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.AbstractLine#getFieldValueIfExists(java.lang.String)
	 */
	@Override
	public IFieldValue getFieldValueIfExists(String fieldName) {
		return  getFieldValue(layout.getFieldFromName(fieldName));
	}

	/**
	 * Get Field Iterator for the requested Record-Type
	 * @param recordName Record Name to retrieve the field list for.
	 * @return Field Iterator
	 */
	@Override public final FieldIterator getFieldIterator(String recordName) {
		int recordNumber = layout.getRecordIndex(recordName);
		if (recordNumber < 0) {
			throw new RuntimeException("Record: " + recordName + " does not exist in layout");
		}
		return new FieldIterator(this, recordNumber);
	}


	/**
	 * Get Field Iterator for the requested Record-Type
	 * @param recordNumber record number
	 * @return Field Iterator
	 */
	@Override public final FieldIterator getFieldIterator(int recordNumber) {
		return new FieldIterator(this, recordNumber);
	}

	/**
	 * Get the Layout
	 * @return Returns the layouts.
	 */
	public LayoutDetail getLayout() {
	    return layout;
	}
}
