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

import net.sf.JRecord.Common.AbstractFieldValue;


/**
 * Extended FieldValue interface for us in JRecord.
 * It mainly contains high/low value methods 
 * 
 * @author Bruce Martin
 * 
 * @deprecated use either AbstractFieldValue or {@link net.sf.JRecord.Details.fieldValue.IFieldValue }
 *
 */
public interface IFieldValue extends AbstractFieldValue {

	public abstract void setToHighValues();

	public abstract void setToLowValues();

	public abstract void setHex(String s);

	public abstract boolean isFieldPresent();

	/**
	 * wether the value is "High-Values"
	 * 
	 * @return  is "High-Values"
	 */
	public abstract boolean isHighValues();

	/**
	 * wether the field is "Low-Values"
	 * @return if low values
	 */
	public abstract boolean isLowValues();

	
	/**
	 * Test the field value if it is spaces
	 * @return wether the field is spaces
	 */
	public abstract boolean isSpaces();
	
	
	/**
	 * wether hex update operations (setHex setToHigh/Low Values)
	 * are supported 
	 * 
	 * @return wether it is a Byte based record.
	 */
	public abstract boolean isByteRecord();
	
	/**
	 * For Cobol Records it checks wether a field in a Occurs-Depending
	 * array is actually there (i.e. wether the index < current-Max-Index).
	 * 
	 * @return wether the line is actually valid for this Record.
	 * For Cobol Lines it checks the status of Fields
	 * in an Occurs-Depending
	 */
	public abstract boolean isFieldInRecord();

	/**
	 * Set the fieldValue to spaces
	 */
	public void setToSpaces();
}
