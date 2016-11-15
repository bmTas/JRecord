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
      
package net.sf.JRecord.External.Def;

import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.Common.AbstractRecordX;
import net.sf.JRecord.Common.IFieldDetail;

/**
 * This class holds a Node for each Array-Index. It is like java's TreeNode where
 * parent refers to the parent array index.
 * 
 * i.e. it starts at Field and goes up the Tree. This structure holds
 * <b>all arrays<b> (<i>not just occurs-depending-on-arrays</i>)
 * 
 * So for a Cobol Field <b>field-1 (3, 4, 5)</b> you would have
 * (if there are occurs-depending-on arrays in the mix):
 * 
 * <pre>
 * 
 *  Field-1 (2, 4, 6)
 *    | 
 *    *  DependingOnDtls
 *         * index = 6
 *         * parent = DependingOnDtls
 *                      * index = 4
 *                      * parent = DependingOnDtls
 *                                   * index = 2
 *                                   * parent = null
 *         
 * </pre>        
 * 
 * @author Bruce Martin
 *
 */
public final class DependingOnDtls implements IDependingOnIndexDtls {
	public final DependingOn dependingOn;
	public final int index;
	public final DependingOnDtls parent;
	public final boolean firstIdx;
	private List<DependingOn> children = null;
	private int reliableCalculationsTo;
	
	public DependingOnDtls(DependingOn dependingOn, int index,
			DependingOnDtls parent) {
		this(dependingOn, index, parent, true);
	}
	public DependingOnDtls(DependingOn dependingOn, int index,
			DependingOnDtls parent, boolean permanent) {
		super();
		this.dependingOn = dependingOn;
		this.index = index;
		this.parent = parent;
		this.firstIdx = index == 0 
				     && (parent == null || parent.firstIdx);
		
		if (permanent) {
			if (parent != null && index == 0) {
				if (parent.children == null) {
					parent.children = new ArrayList<DependingOn>(3);
				}
				parent.children.add(dependingOn);
			}
			
			dependingOn.indexDtls.add(this);
		}
		
		reliableCalculationsTo = dependingOn.getPosition() + dependingOn.getOccursLength() * (index + 1);
	}
	
	
	public DependingOnDtls[] getTree() {
		return getTree(1);
	}
	
	
	private DependingOnDtls[] getTree(int lvl) {
		DependingOnDtls[] ret;
		if (parent == null) {
			ret = new DependingOnDtls[lvl];
		} else {
			ret = parent.getTree(lvl + 1);
		}
		ret[ret.length - lvl] = this;
		return ret;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.Def.DependingOnIndexDtls#getDependingOn()
	 */
	@Override
	public DependingOn getDependingOn() {
		return dependingOn;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.Def.DependingOnIndexDtls#getIndex()
	 */
	@Override
	public int getIndex() {
		return index;
	}
	
	public int getReliableCalculationsTo() {
		return reliableCalculationsTo;
	}


	@Override
	public List<DependingOn> getChildren() {
		return children; 
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.Def.DependingOnIndexDtls#updateFieldInChildren(net.sf.JRecord.Common.AbstractRecordX)
	 */
	@Override
	public void updateFieldInChildren(AbstractRecordX<? extends IFieldDetail> rec) {
		if (children != null) {
			for (DependingOn c : children) {
				c.updateField(rec);
			}
			
			DependingOn c = children.get(0);
			reliableCalculationsTo = c.getPosition() + c.getOccursLength();
		}
	}
}
