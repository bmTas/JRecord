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

package net.sf.JRecord.schema;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.schema.jaxb.IItem;

public interface IArrayItemCheck {
	public static final int R_PROCESS = 0;
	public static final int R_SKIP    = 1;
	public static final int R_STOP    = 2;
	
	public int checkItem(AbstractLine line, IItem item, int[] indexs, int index);
	
	public int getCount(AbstractLine line, IItem item, int[] indexs, int defaultCount);
	
	public void updateForCount(AbstractLine line, IItem item, int[] indexs, int count);
}
