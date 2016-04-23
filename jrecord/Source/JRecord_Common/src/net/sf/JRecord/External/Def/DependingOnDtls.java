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

public final class DependingOnDtls {
	public final DependingOn dependingOn;
	public final int index;
	public final DependingOnDtls parent;
	public final boolean firstIdx;
	
	
	public DependingOnDtls(DependingOn dependingOn, int index,
			DependingOnDtls parent) {
		super();
		this.dependingOn = dependingOn;
		this.index = index;
		this.parent = parent;
		this.firstIdx = index == 0 
				     && (parent == null || parent.firstIdx);
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

}
