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

import java.util.Iterator;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.fieldValue.FieldValue;
import net.sf.JRecord.External.Def.DependingOnDtls;

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
	public net.sf.JRecord.Details.fieldValue.IFieldValue  getFieldValue(IFieldDetail field) {
		return new FieldValue(this, field);
	}

	@Override
	public  net.sf.JRecord.Details.fieldValue.IFieldValue  getFieldValue(int recordIdx, int fieldIdx) {
		return new FieldValue(this, recordIdx, fieldIdx);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.IGetFieldValueByName#getFieldValue(java.lang.String)
	 */
	@Override
	public final net.sf.JRecord.Details.fieldValue.IFieldValue  getFieldValue(String fieldName) {
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
	public net.sf.JRecord.Details.fieldValue.IFieldValue  getFieldValueIfExists(String fieldName) {
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
	
	
	@Override
	public Iterator<AbstractFieldValue> getFieldIterator() {
		return new FieldIterator(this, getPreferredLayoutIdx());
	}

	/**
	 * 
	 * This basically checks to see if a field is occurs depending field
	 * and the index < maximum index (as defined by the depending field)
	 * 
	 * 	 In Cobol <i>Occurs depending arrays</i> are variable size arrays with the array size determined by a field.
	 * These arrays can vary in size from one line to the next. This means an an index may be valid in one line but 
	 * not the next
	 * 
	 * <pre>
	 *  Occurs Depending in Cobol:
	 *  
	 *       03  count           pic s99 comp.
	 *       03  field-1 occurs 1 to 5 depending on count
	 *                           pic xx.
	 * <pre>
	 * If <b>Count=2</b> the array will only have 2 elements and fields
	 * with index's >= 2 will not exist in the array. This method checks for
	 * index's >= count variable (for occurs depending arrays.                           
	 * 
	 * @param field Field to check
	 * @return For occurs depending arrays the index must be valid, true for all other fields
	 */
	@Override public final boolean isFieldInLine(IFieldDetail field) {
		if (field == null) { return false; }
		if ( field instanceof FieldDetail) { 
			DependingOnDtls dependingOnDtls = ((FieldDetail) field).getDependingOnDtls();
			if (dependingOnDtls != null) {
				DependingOnDtls[] tree = dependingOnDtls.getTree();
				for (int i = 0; i < tree.length; i++){
					dependingOnDtls = tree[i];
		            Object v = this.getField(dependingOnDtls.dependingOn.getField());
		            
		            String s;
		            if (v == null || ((s= v.toString()).length() == 0))  {
		            	return false;
		            }
		            
					int count = Integer.parseInt(s);
		
		            if (dependingOnDtls.index >= count) {
		                return false;
		            }
		        }
			}
		}
        return true;
	}

	/**
	 * Get the Layout
	 * @return Returns the layouts.
	 */
	public LayoutDetail getLayout() {
	    return layout;
	}
}
