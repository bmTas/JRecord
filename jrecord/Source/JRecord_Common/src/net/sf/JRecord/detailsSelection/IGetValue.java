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

import net.sf.JRecord.Common.AbstractIndexedLine;

public interface IGetValue {

	/**
	 * is the value going to be numeric ???
	 * @return is the value going to be numeric ???
	 */
	public boolean isNumeric();

	/**
	 * get the value from a line
	 * @param line to extract the value from
	 * @return requested value
	 */
	public Object getValue(AbstractIndexedLine line);

	/**
	 * Wether this record should be Tested ???
	 *
	 * @param line line to be checked
	 * @return wether to test it or not
	 */
	public boolean isIncluded(AbstractIndexedLine line);

	/**
	 * get the value from a list of lines
	 * @param line to extract the value from
	 * @return requested value
	 */
	public Object getValue(List<? extends AbstractIndexedLine> lines);

}
