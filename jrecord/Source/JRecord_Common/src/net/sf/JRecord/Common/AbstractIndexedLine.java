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


public interface AbstractIndexedLine  {

	/**
	 * Gets a fields value
	 *
	 * @param recordIdx Index of the RecordDescription to be used.
	 * @param fieldIdx Index of the required field
	 *
	 * @return the request field (formated)
	 */
	public abstract Object getField(final int recordIdx, final int fieldIdx);

	/**
	 * Get a fields value
	 *
	 * @param field field to retrieve
	 *
	 * @return fields Value
	 *
	 */
	public abstract Object getField(IFieldDetail field);

	   /**
     * Get the Preferred Record Layout Index for this record
     *
     * @return Index of the Record Layout based on the Values
     */
    public abstract int getPreferredLayoutIdx();

	//
	//    /**
	//     * Get a fields value
	//     *
	//     * @param fieldName field to retrieve
	//     *
	//     * @return fields Value
	//     *
	//     * @deprecated use getFieldValue
	//     */
	//    public abstract Object getField(String fieldName);

	/**
	 * Sets a field to a new value
	 *
	 * @param recordIdx record layout
	 * @param fieldIdx field number in the record
	 * @param val new value
	 *
	 */
	public abstract void setField(final int recordIdx, final int fieldIdx,
			Object val);

	/**
	 * Set a fields value
	 *
	 * @param field field to retrieve
	 * @param value value to set the field to
	 *
	 */
	public abstract void setField(IFieldDetail field, Object value);

}