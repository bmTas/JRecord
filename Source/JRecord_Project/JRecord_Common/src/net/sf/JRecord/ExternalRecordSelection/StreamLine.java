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
 * Purpose Streamline (remove unnecessary groups) from a Selection
 * equation. Basically it performs very basic optimization on the equation
 * 
 * Each <i>ExternalSelection</i> represents an boolean-equation and it can
 * be one of
 * <ul>
 *   <li><b>Or-group</b> which has one or more child <i>ExternalSelection</i> 
 *   <li><b>And-group</b> which has one or more child <i>ExternalSelection</i> 
 *   <li><b>Field-Test</b>Test a field against a value
 * </ul
 *    
 * @author Bruce Martin
 *
 * @param <Selection> Equation type
 */
public class StreamLine<Selection extends ExternalSelection> {
	
	private static final StreamLine<ExternalSelection> externalStreamLine
				= new StreamLine<ExternalSelection>();
	
	/**
	 * Streamline (optimize - remove unnessary groups) the equation
	 * @param sel boolean equation 
	 * @return optimized equation 
	 */
	public final Selection streamLine(Selection sel) {
		if (sel instanceof ExternalGroupSelection) {
			@SuppressWarnings("unchecked")
			ExternalGroupSelection<Selection> grp = (ExternalGroupSelection<Selection>) sel;
			if (grp.size() == 1) {
				return streamLine(grp.get(0));
			} else {
				for (int i = 0; i < grp.size(); i++) {
					
					grp.set(i, streamLine(grp.get(i)));
				}
			}
		}
		return sel;
	}

	/**
	 * @return the externalStreamLine
	 */
	public static StreamLine<ExternalSelection> getExternalStreamLine() {
		return externalStreamLine;
	}
}
