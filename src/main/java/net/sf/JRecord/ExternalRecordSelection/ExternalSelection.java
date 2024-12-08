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

/**
 * An ExternalSelection is boolean expression stored as a tree i.e.
 * 
 *                      +--- Field Test 1
 *                      |     
 *        +------and----+--- Field Test 2
 *        |             |     
 * ---or--+             +--- Field Test 3
 *        |                 
 *        +-------------+--- Field Test 3
 *        
 * ExternalSelection has 2 extended interfaces:<ul>
 * <li><b>IExternalSelectionGroup</b> implements boolean and/or operators.
 * There can be multiple <i>child</i> ExternalSelection's.
 * <li><B>IExternalSelectionField</b> which tests one field. These have no 
 * child ExternalSelection's.
 * </ul>
 *     
 * @author Bruce Martin
 *
 */
public interface ExternalSelection {

	public static final int TYPE_ATOM = 1;
	public static final int TYPE_AND = 2;
	public static final int TYPE_OR = 3;
//	public static final int TYPE_AND_OR = 4;
//	public static final int TYPE_OR_AND = 5;
	
	/**
	 * Get the <i>Type of Selections</i><ul>
	 * <li><b>TYPE_ATOM = 1</b> Normal Field selection (Leaf in the Tree).
	 * <li><b>TYPE_AND = 2</b> Boolean and operator.
	 * <li><b>TYPE_OR = 2</b> Boolean or operator.
	 * </ul>
	 * @return Type of Selection class.
	 */
	public int getType();
	
	/**
	 * 
	 * @return The number of child Selection elements (will be 0 if a field selection
	 */
	public int getSize();

	/**
	 * 
	 * @return The number of leaf elements (Field Selections) under this selection elements
	 */
	int getElementCount();
}
