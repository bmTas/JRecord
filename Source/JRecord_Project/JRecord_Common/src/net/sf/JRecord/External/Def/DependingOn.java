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
import java.util.regex.Pattern;

import net.sf.JRecord.Common.AbstractRecordX;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;


/**
 * This class represents one <b>Cobol Occurs depending on</b> clause.
 * Any <i>child</i> Occurs depending clauses are stored in children.
 * 
 * <pre>
 *         05 A1    occurs depending on ..
 *            ...
 *            07 A11    occurs depending on ..
 *            ...
 *            07 A12    occurs depending on ..
 * </pre>
 * 
 * is represented as the following class structure
 * 
 * <pre>
 * 
 *    DependingOn
 *      * variableName = A1
 *        
 *                   +-----  DependingOn
 *                   |            variableName = A11
 *      * children=  |
 *                   |
 *                   +-----  DependingOn
 *                                variableName = A12 
 * 
 * <pre>
 * 
 * @author Bruce Martin
 *
 */
public class DependingOn {
	private static final Pattern OF_SPLIT = Pattern.compile("\\sOF\\s", Pattern.CASE_INSENSITIVE);
	
	private final String variableName, variableNameNoIndex;
	private final int position, occursLength, occursMax, occursMaxLength;
	        final List<IDependingOnIndexDtls> indexDtls;
	//private List<DependingOn> children = null;
	
	private IFieldDetail field;
	int fieldNumber;
	
	
	private boolean complicatedDependingOn = false;

	//private int reliableCalculationsTo;
//	private DependingOn parent = null;
	
	/**
	 * This class represents one <b>Cobol Occurs depending on</b> clause.
     * Any <i>child</i> Occurs depending clauses are stored in children.
     *  
	 * @param variableName Cobol field name
	 * @param position     position in the record (assuming all occurs depending at there max)
	 * @param occursLength Length of occurs (assuming no child occurs at max)
	 * @param occursMax    Maximum number of occurs
	 */
	public DependingOn(String variableName, String variableNameNoIndex, int position, int occursLength, int occursMax) {
		super();
		this.variableName = variableName;
		this.variableNameNoIndex = variableNameNoIndex;
		this.position = position;
		this.occursLength = occursLength;
		this.occursMax = occursMax;
		this.occursMaxLength = occursLength * occursMax;
		this.indexDtls = new ArrayList<IDependingOnIndexDtls>(occursMax);
		//this.complicatedOdDef = ! variableName
	}

	/**
	 * @return the children
	 */
	public final List<DependingOn> getChildren() {
		if (indexDtls == null || indexDtls.size() == 0) { return null; }
		return indexDtls.get(0).getChildren();
		//return children;
	}

	public List<IDependingOnIndexDtls> getIndexDtls() {
		return indexDtls;
	}

	/**
	 * @return the variableName
	 */
	public final String getVariableName() {
		return variableName;
	}

	/**
	 * @return the variableNameNoIndex
	 */
	public final String getVariableNameNoIndex() {
		return variableNameNoIndex;
	}

	/**
	 * @return the position
	 */
	public final int getPosition() {
		return position;
	}
	/**
	 * @return the position
	 */
	public final int getEnd() {
		return position + occursMaxLength - 1;
	}

	/**
	 * @return the occursLength
	 */
	public final int getOccursLength() {
		return occursLength;
	}

	/**
	 * @return the occursMax
	 */
	public final int getOccursMax() {
		return occursMax;
	}


	/**
	 * @return the field
	 */
	public final IFieldDetail getField() {
		return field;
	}

	public int getFieldNumber() {
		return fieldNumber;
	}

	public boolean isComplicatedDependingOn() {
		return complicatedDependingOn;
	}

	/**
	 * @return the occursMaxLength
	 */
	public final int getOccursMaxLength() {
		return occursMaxLength;
	}
	
//	public final void addChild(DependingOn child) {
//		children = addChild(children, child);
//	}
	
	
//	public final boolean addNextIndex(DependingOn child) {
//		if (children != null && children.size() > 0) {
//			for (DependingOn c : children) {
//				if (c.variableNameNoIndex.equalsIgnoreCase(child.variableNameNoIndex)) {
//					DependingOn next  = c;
//					while (next.nextIndex != null) {
////						if (next.position <= child.position
////						&& next.position + next.occursMaxLength > child.position) {
////							next.addNextIndex(child);
////							return;
////						}
//						next = next.nextIndex;
//					}
//					next.nextIndex = child;
//					return true;
//				} else 	if (c.position <= child.position
//						&& c.position + c.occursMaxLength > child.position) {
//					if (c.addNextIndex(child)) {
//						return true;
//					}
//				}
//			}
//		}
//		return false;
//	}

//	/**
//	 * add a 'child' to a list of children
//	 * 
//	 * @param childList current child list
//	 * @param child to add to the list
//	 * 
//	 * @return updated list
//	 */
//	public static List<DependingOn> addChild(List<DependingOn> childList, DependingOn child) {
//		if (childList == null) {
//			childList = new ArrayList<DependingOn>(3);
//		} else { 
//			for (DependingOn c : childList) {
//				if (c.position <= child.position
//				&& c.position + c.occursMaxLength > child.position) {
////					child.parent = c;
//					c.addChild(child);
//					return childList;
//				}
//			}
//		}
//		
//		childList.add(child);
//		return childList;
//	}

//	public static void addNextIndex(List<DependingOn> childList, DependingOn child) {
//		if (childList != null) {
//			for (DependingOn c : childList) {
//				if (c.position <= child.position
//				&& c.position + c.occursMaxLength > child.position) {
////					child.parent = c;
//					if (c.addNextIndex(child)) {
//						return;
//					}
//				}
//			}
//		}
//
//	}

	/**
	 * update occurs depending field details
	 * 
	 * @param rec record to search.
	 */
	public void updateField(AbstractRecordX<? extends IFieldDetail> rec) {
		if (field == null) {
			try {
				String[] groups = OF_SPLIT.split(variableName);
				if (groups == null || groups.length < 2) {
					field = rec.getField(variableName);
				} else {
					String[] groupsNS = new String[groups.length];
					for (int i = 0; i < groups.length; i++) {
						groupsNS[groups.length - i] = groups[i];
					}
					field = rec.getGroupField(groupsNS);
				}
			} catch (Exception e) {
				throw new RuntimeException("Error With Occurs Depending On Field: " + variableName, e);
			}
			
			if (field == null) {
				throw new RuntimeException("Error With Occurs Depending On Field: " + variableName);
			}
			
			if (field instanceof FieldDetail) {
				((FieldDetail) field).setOccursDependingOnValue(true);
			}
			
			
			for (IDependingOnIndexDtls idxDtls : indexDtls) {
				idxDtls.updateFieldInChildren(rec);
			}

			List<DependingOn> children2 = indexDtls.get(0).getChildren();
			if (children2 != null && children2.size() > 0) {
				for (DependingOn c : children2) {
					//c.updateField(rec);
					complicatedDependingOn 	 =   complicatedDependingOn
											||   c.complicatedDependingOn
											|| ! c.variableName.equalsIgnoreCase(c.variableNameNoIndex);
				}
				
			}
		}
	}
		
//		DependingOn next  = nextIndex;
//		while (next != null) {
//			if (variableName.equalsIgnoreCase(next.variableName)) {
//				next.field = field;
//			} else {
//				next.updateField(rec);
//			}
//			next = next.nextIndex;
//		}
		//System.out.print(true);
//	}
}
