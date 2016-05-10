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
      
package net.sf.JRecord.detailsSelection;

import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.Common.AbstractIndexedLine;


/**
 * Used to check if the line is a specific Record.
 *
 * @author Bruce Martin
 *
 */
public class RecordSelection {
	
	private RecordSel recSel;

	//private final RecordDetail parent;
	private boolean defaultRecord = false;
	
	
	public RecordSelection() {
		super();
		//this.parent = parent;
	}
	
	
//	/**
//	 * @param index
//	 * @return
//	 * @see java.util.ArrayList#get(int)
//	 */
//	public SelectionField get(int index) {
//		return flds.get(index);
//	}

	/**
	 * @return size of the selection
	 */
	public int size() {
		if (recSel == null) {
			return 0;
		}
		return recSel.getSize();
	}


	/**
	 * @return the number of child elements
	 * @see net.sf.JRecord.ExternalRecordSelection.ExternalSelection#getElementCount()
	 */
	public int getElementCount() {
		if (recSel == null) {
			return 0;
		}
		return recSel.getElementCount();
	}


	public FieldSelect getFirstField() {
		return recSel.getFirstField();
	}
	
	
//	/**
//	 * Add a Field/Value that should be tested to determine if this is the valid
//	 * Sub-Record for the current line.
//	 * 
//	 * @param tstField the tstField to set
//	 * @param value Value to compare field to
//	 */ 
//	public void addTstField(String tstField, String value) {
//		if (tstField == null || "".equals(tstField)) {
//			if ("*".equals(value)) {
//				defaultRecord = true;
//			}
//		} else {
//			FieldDetail fld = parent.getField(tstField);
//			
//			getFields().add( new SelectionField(tstField, fld, value));
//		}
//	}
	
//	/**
//	 * Add a list of TstFields
//	 * @param flds fields to add
//	 */
//	public void add(List<TstField> flds) {
//		for (TstField fld : flds) {
//    		addTstField(fld.fieldName, fld.value);
//    	}
//	}
//	
//	public void setTstField(int idx, String tstField, FieldDetail fld, String value) {
//		if (getFields().size() == 0) {
//			flds.add(null);
//		}
//		flds.set(idx, new SelectionField(tstField, fld, value));
//	}
	
	public RecordSelectionResult isSelected(AbstractIndexedLine line) {
		RecordSelectionResult ret = RecordSelectionResult.NO;
		
		if (recSel == null) {
			if (defaultRecord) {
				ret = RecordSelectionResult.DEFAULT;
			} 
		} else {
			if (recSel.isSelected(line)) {
				if (defaultRecord) {
					ret = RecordSelectionResult.DEFAULT;
				} else {
					ret = RecordSelectionResult.YES;
				}
			} 
		}
		
		return ret;
	}

//	private ArrayList<SelectionField> getFields() {
//		
//		if (flds == null) {
//			flds = new ArrayList<SelectionField>(5);
//		}
//		
//		return flds;
//	}
	
	/**
	 * @return the defaultRecord
	 */
	public boolean isDefaultRecord() {
		return defaultRecord;
	}


	/**
	 * @param defaultRecord the defaultRecord to set
	 */
	public void setDefaultRecord(boolean defaultRecord) {
		this.defaultRecord = defaultRecord;
	}
	
	/**
	 * @param recSel the recSel to set
	 */
	public void setRecSel(RecordSel recSel) {
		this.recSel = recSel;
	}
	
	public List<FieldSelect> getAllFields() {
		List<FieldSelect> fields = new ArrayList<FieldSelect>();
		
		if (recSel != null) {
			recSel.getAllFields(fields);
		}
		
		return fields;
	}

	public static enum RecordSelectionResult {
		NO,
		DEFAULT,
		YES
	}

	/**
	 * @return the recSel
	 */
	public final RecordSel getRecSel() {
		return recSel;
	}



}
