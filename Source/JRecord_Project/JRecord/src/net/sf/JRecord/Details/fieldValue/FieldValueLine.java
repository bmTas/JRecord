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
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.Line;

public class FieldValueLine extends FieldValue {

	private Line theLine; 

	protected FieldValueLine(Line line, IFieldDetail fieldDetails) {
		super(line, fieldDetails);
		theLine = line;
	}

	protected FieldValueLine(Line line, int recordIndex, int fieldIndex) {
		super(line, line.getLayout().getField(recordIndex, fieldIndex));
		theLine = line;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.FieldValue#isLowValues()
	 */
	@Override
	public boolean isLowValues() {
		return checkFor((byte) 0);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.FieldValue#isHighValues()
	 */
	@Override
	public boolean isHighValues() {
		return checkFor((byte) -1);
	}
	
	
	
	@Override
	public boolean isSpaces() {
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
	public void setHex(String val) {
		theLine.setFieldHex(field, val); 
	}


	
	@Override
	public void setToSpaces() {
		setFieldToByte(theLine.getLayout().getSpaceByte());
	}


	protected void setFieldToByte(byte val) {
		theLine.setFieldToByte(field, val);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.fieldValue.IFieldValueUpdLine#setLine(net.sf.JRecord.Details.Line)
	 */
	@Override
	public IFieldValueUpdLine setLine(Line line) {
		theLine = line;
		return super.setLine(line);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.fieldValue.IFieldValueUpdLine#setLine(net.sf.JRecord.Details.AbstractLine)
	 */
	@Override
	public IFieldValueUpdLine setLine(AbstractLine line) {
		theLine = (Line) line;
		return super.setLine(line);
	}

}
