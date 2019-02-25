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
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;

public class ArrayDetails {

	public final String arrayName;
	public final int[] sizes;
	private final ArrayList<FieldDef> fieldDefs = new ArrayList<FieldDef>();
	private boolean toSort = true;
	private final ArrayElement firstElement;
	private final String zeroArrayIndex, dimensionString;
	
	public ArrayDetails(ArrayElement ai, FieldDef fieldDef) {
		this.arrayName = ai.arrayName;
		this.firstElement = ai;
		sizes = new int[ai.indexs.length];
		Arrays.fill(sizes, 1);
		fieldDefs.add(fieldDef);
		
		StringBuilder b = new StringBuilder("");
		String sep = "";
		for (int i = 0; i < sizes.length; i++) {
			b.append(sep).append("0");
			sep = ", ";
		}
		
		zeroArrayIndex = b.toString();
		
		switch (sizes.length) {
		case 1:
			dimensionString = "1";
			break;
		case 2:
			dimensionString = "2";
			break;
		case 3:
			dimensionString = "3";
			break;
		default:
			dimensionString = "Any";
		}
	} 
	
	/**
	 * @return the arrayName
	 */
	public final String getArrayName() {
		return arrayName;
	}

	public void addDetails(ArrayElement ai, FieldDef fieldDef) {
		for (int i = 0; i < sizes.length; i++) {
			sizes[i] = Math.max(sizes[i], ai.indexs[i]);
		}
		
		if (fieldDef.getArrayDetails().special) {
			fieldDefs.add(fieldDef);
		}
	}
	
	public boolean sizesEqual(ArrayDetails ai) {
		if (sizes.length != ai.sizes.length) {
			return false;
		}
		for (int i = 0; i < sizes.length; i++) {
			if (sizes[i] != ai.sizes[i]) {
				return false;
			}
		}
		return true;
	}

	/**
	 * @return the fieldDefs
	 */
	public final ArrayList<FieldDef> getFieldDefs() {
		if (toSort) {
			Collections.sort(fieldDefs, new Comparator<FieldDef>() {
				@Override public int compare(FieldDef o1, FieldDef o2) {
					if (o1.getArrayDetails().specialLevel > o2.getArrayDetails().specialLevel) {
						return 1;
					} else if (o1.getArrayDetails().specialLevel < o2.getArrayDetails().specialLevel) {
						return -1;
					}
					return 0;
				}
			});
			
			toSort = false;
		}
		return fieldDefs;
	}
	
	public FieldDef getFirstFieldDef() {
		 ArrayList<FieldDef> fd = getFieldDefs();
		 return fd.get(fd.size() - 1);
	}

	/**
	 * @return the firstElement
	 */
	public final ArrayElement getFirstElement() {
		return firstElement;
	}

	/**
	 * @return the zeroArrayIndex
	 */
	public final String getZeroArrayIndex() {
		return zeroArrayIndex;
	}

	/**
	 * @return the dimensionString
	 */
	public final String getDimensionString() {
		return dimensionString;
	}
	
	public String getIndexParameters() {
		StringBuilder b = new StringBuilder();
		
		String sep = "";
		for (int i = 1; i <= sizes.length; i++) {
			b.append(sep).append("int idx").append(i);
			sep = ", ";
		}

		
		return b.toString();
	}
	
	
	public String getZeroIndexParameters() {
		StringBuilder b = new StringBuilder();
		
		String sep = "";
		for (int i = 1; i <= sizes.length; i++) {
			b.append(sep).append("0");
			sep = ", ";
		}

		return b.toString();
	}
	
	public String getCallIndexParameters() {
		StringBuilder b = new StringBuilder();
		
		String sep = "";
		for (int i = 1; i <= sizes.length; i++) {
			b.append(sep).append("idx").append(i);
			sep = ", ";
		}

		
		return b.toString();
	}

	public String getArrayAccessParameters() {
		StringBuilder b = new StringBuilder();
		
		for (int i = 1; i <= sizes.length; i++) {
			b.append("[idx").append(i).append(']');
		}

		
		return b.toString();
	}

	
	public String getForLoops(String destName) {
		StringBuilder b = new StringBuilder();
		
		StringBuilder indent = new StringBuilder("        ");
		if (destName == null || destName.length() == 0) {
			for (int i = 1; i <= sizes.length; i++) {
				b.append(indent).append("for (int idx" + i + " = 0; idx" + i + " < " + sizes[i - 1] + "; idx" + i + "++) {\n");
				indent.append("    ");
			}
		} else {
			for (int i = 1; i <= sizes.length; i++) {
				b.append(indent).append("for (int idx" + i + " = 0; idx" + i + " < " + destName + "(" + (i-1) + "); idx" + i + "++) {\n");
				indent.append("    ");
			}
		}

		if (b.length() > 0) {
			b.setLength(b.length() - 1);
		}
		return b.toString();
	}
	
	
	public String getEndForLoops() {
		StringBuilder b = new StringBuilder();
		
		String spaces = "                                                                                                ";
		StringBuilder indent = new StringBuilder(spaces).append(spaces).append(spaces);
		indent.setLength(sizes.length * 4 + 4);

		for (int i = 1; i <= sizes.length; i++) {
			b.append(indent).append("}\n");
			indent.setLength(indent.length() - 4);
		}

		return b.toString();
	}

	/**
	 * @return the sizes
	 */
	public final int[] getSizes() {
		return sizes;
	}

	public final String getSizesAsString() {
		StringBuilder b = new StringBuilder();
		String sep = "";
		
		for (int i = 0; i < sizes.length; i++) {
			b.append(sep).append((sizes[i]+1));
			sep = ", ";
		}
		
		return b.toString();
	}
}
