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
import net.sf.JRecord.Details.fieldValue.FieldValue;


/**
 * Class to iterate over all fields for a Record
 *
 * @author Bruce Martin
 *
 */
public class FieldIterator implements Iterable<AbstractFieldValue>, Iterator<AbstractFieldValue> {

	private final AbstractLine line;
	private int fieldNo = 0;
	private final int recordNo;
	private final RecordDetail recordDef;


	public FieldIterator(AbstractLine line, int recordNum) {
		super();
		this.line = line;
		this.recordNo = recordNum;

		this.recordDef = line.getLayout().getRecord(recordNum);
		
		toNextField();
	}

	/**
	 * Return the field values
	 * @param fieldNumber field number
	 * @return field values
	 */
	public AbstractFieldValue getFieldValue(int fieldNumber) {
		return new FieldValue(line, recordNo, fieldNumber);
	}

	/**
	 * Get the number of fields
	 * @return number of fields
	 */
	public int getFieldCount() {
		return recordDef.getFieldCount();
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#hasNext()
	 */
	@Override
	public boolean hasNext() {
		return fieldNo < recordDef.getFieldCount();
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#next()
	 */
	@Override
	public AbstractFieldValue next() {
		AbstractFieldValue ret =  new FieldValue(line, recordNo, fieldNo++);
		toNextField();
		return ret;
	}
	
	private void toNextField() {
		while (fieldNo < recordDef.getFieldCount()
		   && ! line.isFieldInLine(recordDef.getField(fieldNo))) {
			fieldNo += 1;
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#remove()
	 */
	@Override
	public void remove() {
		throw new RuntimeException("Remove is not supported");
	}

	/* (non-Javadoc)
	 * @see java.lang.Iterable#iterator()
	 */
	@Override
	public Iterator<AbstractFieldValue> iterator() {
		return new FieldIterator(line, recordNo);
	}



}
