/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
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

package net.sf.JRecord.cgen.impl;

import net.sf.JRecord.cgen.def.IFieldName1Dimension;
import net.sf.JRecord.cgen.def.IFieldName2Dimension;
import net.sf.JRecord.cgen.def.IFieldName3Dimension;
import net.sf.JRecord.cgen.def.IFieldName4Dimension;

public class ArrayFieldName implements IFieldName1Dimension, IFieldName2Dimension, IFieldName3Dimension , IFieldName4Dimension {

	private final String name;
	
	
	public ArrayFieldName(String name) {
		this.name = name;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IFieldName1Dimension#get(int)
	 */
	@Override
	public String get(int index1) {
		return genName(index1);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IFieldName2Dimension#get(int, int)
	 */
	@Override
	public String get(int index1, int index2) {
		return genName(index1, index2);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IFieldName3Dimension#get(int, int, int)
	 */
	@Override
	public String get(int index1, int index2, int index3) {
		return genName(index1, index2, index3);
	}

	@Override
	public String get(int index1, int index2, int index3, int index4) {
		return genName(index1, index2, index3, index4);
	}

	private String genName(int... indexs) {
		StringBuilder b = new StringBuilder(name);
		String sep = " (";
		
		for (int idx : indexs) {
			b.append(sep).append(idx);
			sep = ", ";
		}
		return b.append(')').toString();
	}
}
