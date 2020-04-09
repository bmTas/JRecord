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

package net.sf.JRecord.Details.fieldValue;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.base.ExternalConversion;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;

public abstract class BaseFieldValueLine implements IFieldValueUpdLine {

	protected Line theLine; 
	protected IFieldDetail field;

	protected BaseFieldValueLine(Line line, IFieldDetail fieldDetails) {
		this.theLine = line;
		this.field = fieldDetails;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.FieldValue#isLowValues()
	 */
	@Override
	public final boolean isLowValues() {
		return checkFor((byte) 0);
	}



	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.FieldValue#isHighValues()
	 */
	@Override
	public final boolean isHighValues() {
		return checkFor((byte) -1);
	}
	
	
	
	@Override
	public final boolean isSpaces() {
		return checkFor(theLine.getLayout().getSpaceByte());
	}

	private boolean checkFor(byte b) {
		byte[] bytes = theLine.getFieldBytes(field);
		
		if (bytes == null) {
			return b == 0;
		} 
		for (int i = 0; i < bytes.length; i++) {
			if (bytes[i] != b) {
				return false;
			}
		}
		
		return true;
	}

	/**
	 * @see net.sf.JRecord.Details.fieldValue.FieldValue#setHex(java.lang.String)
	 */
	@Override
	public final void setHex(String val) {
		theLine.setFieldHex(field, val); 
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.FieldValue#setToLowValues()
	 */
	@Override
	public final void setToLowValues() {
		theLine.setFieldToByte(field, (byte) 0);
	}
	
	@Override
	public void setToSpaces() {
		setFieldToByte(theLine.getLayout().getSpaceByte());
	}


	protected void setFieldToByte(byte val) {
		theLine.setFieldToByte(field, val);
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.FieldValue#setToHighValues()
	 */
	@Override
	public final void setToHighValues() {
		theLine.setFieldToByte(field, (byte) 0xFF);
	}



	@Override
	public final int asInt() {
		return (int) asLong();
	}
	

	@Override
	public String asHex() {
		return theLine.getField(Type.ftHex, field).toString();
	}


	@Override
	public boolean asBoolean() {
		return false;
	}


	@Override
	public final boolean isFieldInRecord() {
		return theLine.isFieldInLine(field);
	}
	
	@Override
	public final boolean isFieldPresent() {	
		return this.theLine.isDefined(field);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.IFieldValue#isByteRecord()
	 */
	@Override
	public final boolean isByteRecord() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Common.AbstractFieldValue#asString()
	 */
	@Override
	public final String asString() {
		return asBigDecimal().toString();
//		Object fieldVal = theLine.getField(field);
//		return fieldVal == null ? "" : fieldVal.toString();
	}

	/**
	 * Wether it is a Numeric field
	 * @return is a Numeric field
	 */
	public final boolean isNumeric() {
		return getType().isNumeric();
	}

	/**
	 * Wether it is a binary Field
	 * @return is a binary field
	 */
	public final boolean isBinary() {
		return getType().isBinary();
	}
	

	private Type getType() {
		return TypeManager.getInstance().getType(getFieldDetail().getType());
	}

	/**
	 * Get The field Definition
	 * @return Field Definition
	 */
	public final IFieldDetail getFieldDetail() {
		return field;
	}

	/**
	 * Get the Type name
	 * @return Type name
	 */
	public final String getTypeName() {
		return ExternalConversion.getTypeAsString(0, getFieldDetail().getType());
	}
	
	@Override
	public void set(boolean value) {
		throw new RecordException("Can not assign boolean to numeric value");
	}

	/**
	 * @param theLine the theLine to set
	 */
	public BaseFieldValueLine setLine(Line theLine) {
		this.theLine = theLine;
		return this;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.fieldValue.IFieldValueUpdLine#setLine(net.sf.JRecord.Details.AbstractLine)
	 */
	@Override
	public IFieldValueUpdLine setLine(AbstractLine line) {
		this.theLine = (Line) line;
		return this;
	}

}
