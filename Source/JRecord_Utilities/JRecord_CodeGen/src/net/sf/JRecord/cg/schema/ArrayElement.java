/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord CodeGen
 *    
 *    Sub-Project purpose: Generate Java - JRecord source code 
 *                        to read/write cobol data files.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: GPL 3 or later
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU General Public License
 *    as published by the Free Software Foundation; either
 *    version 3.0 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.cg.schema;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

public final class ArrayElement extends JavaDetails {
	final String arrayName;
	final int[] indexs;
	final boolean special, first, firstIndex;
	/**
	 * @return the special
	 */
	final int specialLevel;
	
	public static ArrayElement newArrayItem(String arrayElement, String schemaName) {
		arrayElement = arrayElement.trim();
		int idx = arrayElement.indexOf('(');
		
		return new ArrayElement(arrayElement.substring(0, idx - 1), idx, arrayElement, schemaName);
	}
	private ArrayElement(String arrayName, int idx, String arrayElement, String schemaName) {
		super(arrayName, schemaName, null);
		this.arrayName = arrayName;
		
		int idx2 = arrayElement.indexOf(')');
		if (idx2 >= 0 && idx2 < arrayElement.length() - 1 ) {
			throw new RuntimeException("Duplicate Array Field: " + arrayElement);
		}
		StringTokenizer t = new StringTokenizer(arrayElement.substring(idx+1, /*idx2>0 ? idx2:*/ arrayElement.length() - 1), ",");
		List<Integer> li = new ArrayList<Integer>(7);
		while (t.hasMoreTokens()) {
			li.add(Integer.parseInt(t.nextToken().trim()));
		}
		indexs = new int[li.size()];
		for (int i = 0; i < indexs.length; i++) {
			indexs[i] = li.get(i);
		}
		int oneCount = 0;
		int sLevel = indexs.length;
		boolean firstIdx = true;
		
		for (int i = 0; sLevel >= 0 && i < indexs.length; i++) {
			switch (indexs[i]) {
			case 0: break;
			case 1: 
				if (oneCount > 0) {
					sLevel = -1;
					break;
				}
				sLevel = i;
				oneCount += 1;
				firstIdx = false;
				break;
			default:
				firstIdx = false;
				sLevel = -1;
			}
		}
		this.special = sLevel >= 0;
		this.specialLevel = sLevel;
		this.first = special && oneCount == 0;
		this.firstIndex = firstIdx;
	}
	
	public final boolean isSpecial() {
		return special;
	}

}
