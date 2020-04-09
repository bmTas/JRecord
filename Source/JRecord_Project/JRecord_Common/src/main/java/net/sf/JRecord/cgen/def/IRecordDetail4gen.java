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

	public abstract int getFieldCount();
	
	public abstract FieldDetail getField(int idx);
	
	public abstract String getRecordName();
	
	public abstract RecordSelection getRecordSelection();
	
	public abstract IRecordPositionOption getRecordPositionOption();
	
	public abstract int getRecordType();

	public abstract int getRecordStyle();

	public abstract CsvCharDetails getQuoteDefinition();
	
	public abstract List<? extends IItemDetails> getCobolItems();
}
