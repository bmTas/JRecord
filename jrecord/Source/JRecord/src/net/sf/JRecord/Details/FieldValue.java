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
import net.sf.JRecord.Types.Type;

/**
 * Reference to one field in a line (or Record).
 * It allows the user to get / set the field value using either simple types (int, double etc) or
 * as a Object.
 *
 * <p>Getting a field value:
 * <pre>
 * 	            long sku = saleRecord.getFieldValue("<font color="blue"><b>KEYCODE-NO</b></font>").asLong();
 * </pre>
 *
 * <p>Updating a field:
 * <pre>
 * 	            saleRecord.getFieldValue("<font color="blue"><b>KEYCODE-NO</b></font>").set(1331);
 * </pre>

 * @author Bruce Martin
 *
 */
public class FieldValue extends BaseFieldValue implements IFieldValue {

	private final AbstractLine theLine;
	//final IFieldDetail field;
	final int recordNum;
	final int fieldNum;;

	/**
	 * Create a field value
	 *
	 * @param line line the field value belongs to
	 * @param fieldDetails Field Description
	 */
	public FieldValue(AbstractLine line, IFieldDetail fieldDetails) {
		super(fieldDetails);
		theLine = line;
		recordNum = -1;
		fieldNum = -1;
	}

	/**
	 * Create a field value (using Record / Field Index's)
	 *
	 * @param line line the field value belongs to
	 * @param recordIndex record index of the field
	 * @param fieldIndex field index of the field
	 */
	public FieldValue(AbstractLine line, int recordIndex, int fieldIndex) {
		super(null);
		theLine = line;
		recordNum = recordIndex;
		fieldNum = fieldIndex;
	}

	/**
	 * Get The fields value
	 * @return fields value
	 */
	@SuppressWarnings("deprecation")
	protected Object getValue() {
		if (recordNum >= 0) {
			return theLine.getField(recordNum, fieldNum);
		}
		if (field == null) {
			return null;
		}
		return theLine.getField(field);
	}
	
	public boolean isFieldInRecord() {
		
		IFieldDetail fld = field;
		
		if (recordNum >= 0) {
			fld = theLine.getLayout().getField(recordNum, fieldNum);
		}
		if (fld == null) { return false; }
		return theLine.isFieldInLine(fld);
//		IFieldDetail fld = field;
//		
//		if (recordNum >= 0) {
//			fld = theLine.getLayout().getField(recordNum, fieldNum);
//		}
//		
//		boolean ret = fld != null;
//		DependingOnDtls depOn;
//		
//		if (fld != null && fld instanceof FieldDetail) { 
//			depOn = ((FieldDetail) fld).getDependingOnDtls();
//			
//			try {
//				while (depOn != null) {
//					if (depOn.index >= theLine.getFieldValue(depOn.dependingOn.getVariableName()).asInt()) {
//						return false;
//					}
//					depOn = depOn.parent;
//				}
//			} catch (Exception e) {
//				ret = false;
//			}
//		}
//		
//		return ret;
	}

	/**
	 * @see IFieldValue#asHex()
	 */
	@SuppressWarnings("deprecation")
	@Override
	public String asHex() {
		IFieldDetail fld = field;
		if (recordNum >= 0) {
			fld = theLine.getLayout().getField(recordNum, fieldNum);
		}
		if (theLine instanceof BaseLine) {
			return ((BaseLine) theLine).getField(Type.ftHex, fld).toString();
		}
		return theLine.getLayout().getField(theLine.getData(),
				Type.ftHex,
				fld).toString();
	}

	

	/**
	 * @see IFieldValue#set(java.lang.Object)
	 */
	@SuppressWarnings("deprecation")
	@Override
	public void set(Object value) {
		if (recordNum >= 0) {
			theLine.setField(recordNum, fieldNum, value);
		} else if (field == null) {
			throw new RuntimeException("Unknown Field !!!");
		} else {
			theLine.setField(field, value);
		}
	}

	
	@Override
	public boolean isFieldPresent() {
		
		if (recordNum >= 0) {
			return this.theLine.isDefined(recordNum, fieldNum);
		}
		if (field == null) {
			return false;
		} 
		
		return this.theLine.isDefined(field);
	}


	/**
	 * Get The field Definition
	 * @return Field Definition
	 */
	@Override
	public IFieldDetail getFieldDetail() {
		if (field != null) {
			return field;
		}
		return theLine.getLayout().getRecord(recordNum).getField(fieldNum);
	}
	
	@Override
	public boolean isByteRecord() {
		return theLine instanceof Line;
	}
	
	
	@Override
	public boolean isLowValues() {
		return false;
	}
	
	@Override
	public boolean isHighValues() {
		return false;
	}
	
	@Override
	public void setHex(String s) {
		throwError();
	}
	
	@Override
	public void setToLowValues() {
		throwError();
	}
	
	
	@Override
	public void setToHighValues() {
		throwError();
	}
	
	private void throwError() {
		String s = "";
		if (theLine != null) {
			s = theLine.getClass().getName();
		}
		
		throw new RuntimeException("Operation is not supported for a " + s);
	}
}
