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

import java.util.List;

import net.sf.JRecord.ExternalRecordSelection.ExternalGroupSelection;

public abstract class AbsGroup extends ExternalGroupSelection<RecordSel> implements RecordSel  {

	
	public AbsGroup(int size) {
		super(size);
	}



	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.Selection.RecordSel#getFirstField()
	 */
	@Override
	public FieldSelect getFirstField() {
		FieldSelect s = null;
		
		for (int i = 0; i < getSize() && s == null; i++) {
			s = get(i).getFirstField();
		}
		return s;
	}



	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.Selection.RecordSel#getAllFields(java.util.List)
	 */
	@Override
	public void getAllFields(List<FieldSelect> fields) {
		for (int i = 0; i < getSize(); i++) {
			get(i).getAllFields(fields);
		}
	}
	
	
	
	
}
