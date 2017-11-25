/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord "Classic" Interface examples
 *    
 *    Sub-Project purpose: Examples of using JRecord Classic (or old interface)
 *                        to perform IO on Cobol Data files
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
      
package net.sf.JRecord.zExamples.cobol.ebcdicUnix;

import java.util.ArrayList;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.Details.RecordDetail;

public class TstGetField {

	public static void main(String[] args) {
		LayoutDetail schema = null; 
		Line line = null;
		ArrayList<IFieldDetail> myArrayFlds = new ArrayList<IFieldDetail>();
		IFieldDetail fldDef;
		int i = 0;
		
		while ((fldDef = schema.getFieldFromName("myArray (" + (i++) + ")")) != null ) {
			myArrayFlds.add(fldDef);
		}
		
		System.out.print(line.getFieldValue(myArrayFlds.get(3)).asString());
		
		RecordDetail rec = schema.getRecord(0);
		
		int j = rec.getFieldCount() -1;
		String fn = "myarray (";
		int nameLength = fn.length();
		while (j >= 0
		   &&  rec.getField(j).getName().length() > nameLength + 2
		   &&  ! fn.equalsIgnoreCase(rec.getField(j).getName().substring(0, nameLength))) {
			j -= 1;
		}
	}

}
