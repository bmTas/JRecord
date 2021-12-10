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
      
package net.sf.JRecord.cgen.def;

import java.util.List;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Option.IRecordPositionOption;
import net.sf.JRecord.detailsBasic.CsvCharDetails;
import net.sf.JRecord.detailsBasic.IItemDetails;
import net.sf.JRecord.detailsSelection.RecordSelection;

/**
 * This is `Record` class used by CodeGen to help generate code.
 * It is implemented by RecordDetail in both JRecord and JRecord-for-Recordeditor.
 * This interface allows CodeGen to work with both JRecord and the RecordEditor.
 * 
 * @author Bruce Martin
 *
 */
public interface IRecordDetail4gen {

	/**
	 * 
	 * @return number of fields in a Record
	 */
	public abstract int getFieldCount();
	
	/**
	 * Get a specific field
	 * @param index index of the field being requested
	 * @return requested field
	 */
	public abstract FieldDetail getField(int index);
	
	/**
	 * 
	 * @return Record name
	 */
	public abstract String getRecordName();
	
	/**
	 * For the RecordEditor to know which Record to use for a line in a file,
	 * it uses Record-Activation-criteria crtiteria of each record.
	 * @return Record-Activation-criteria
	 */
	public abstract RecordSelection getRecordSelection();
	
	public abstract IRecordPositionOption getRecordPositionOption();
	
	/**
	 * 
	 * @return Record-Type code
	 */
	public abstract int getRecordType();

	/**
	 * 
	 * @return Record-Style code
	 */
	public abstract int getRecordStyle();
	
	/**
	 * 
	 * @return Record-Length
	 */
	public abstract int getLength();

	/**
	 * 
	 * @return Csv-Details (if applicable)
	 */
	public abstract CsvCharDetails getQuoteDefinition();
	
	/**
	 * 
	 * @return Cobol Definition
	 */
	public abstract List<? extends IItemDetails> getCobolItems();
}
