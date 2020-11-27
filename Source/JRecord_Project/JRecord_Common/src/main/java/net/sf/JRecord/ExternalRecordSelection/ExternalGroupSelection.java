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
      
package net.sf.JRecord.ExternalRecordSelection;

import java.util.ArrayList;

public class ExternalGroupSelection<fs extends ExternalSelection> implements ExternalSelection {

	private ArrayList<fs> items ;
	private int type = ExternalSelection.TYPE_AND;


	public static ExternalGroupSelection<ExternalSelection> newAnd(ExternalSelection... selections) {
		ExternalGroupSelection<ExternalSelection> ret = new ExternalGroupSelection<ExternalSelection>(selections.length);
		
		ret.addAll(selections);
		
		return ret;
	}
	
	public static ExternalGroupSelection<ExternalSelection> newOr(ExternalSelection... selections) {
		ExternalGroupSelection<ExternalSelection> ret = new ExternalGroupSelection<ExternalSelection>(selections.length);
		ret.type = ExternalSelection.TYPE_OR;
		
		ret.addAll(selections);
		
		return ret;
	}


	public ExternalGroupSelection() {
		super();
		items = new ArrayList<fs>();
	}


	public ExternalGroupSelection(int size) {
		super();
		items = new ArrayList<fs>(size);
	}

	/**
	 * @param e
	 * @return
	 * @see java.util.ArrayList#add(java.lang.Object)
	 */
	public boolean add(fs e) {
		return items.add(e);
	}

	private void addAll(@SuppressWarnings("unchecked") fs...selections ) {
		for (fs sel: selections) {
			items.add(sel);
		}
	}
	/**
	 * @param index
	 * @return
	 * @see java.util.ArrayList#get(int)
	 */
	public fs get(int index) {
		return items.get(index);
	}

	/**
	 * @return
	 * @see java.util.ArrayList#size()
	 */
	public int size() {
		return items.size();
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.RecordSelection.ExternalSelection#getType()
	 */
	@Override
	public int getType() {
		return type;
	}


	/**
	 * @param type the type to set
	 */
	public void setType(int type) {
		this.type = type;
	}
	
	@Override
	public int getSize() {
		return items.size();
	}
	
	
	@Override
	public int getElementCount() {
		int count = 0;
		
		for (ExternalSelection s : items) {
			count += s.getElementCount();
		}
		return count;
	}


	/**
	 * @param idx index
	 * @param selectionj
	 * @return
	 * @see java.util.ArrayList#set(int, java.lang.Object)
	 */
	public fs set(int idx, fs selection) {
		return items.set(idx, selection);
	}

}
