/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Cbl2Xml
 *    
 *    Sub-Project purpose: Convert Cobol Data files to / from Xml
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

package net.sf.JRecord.cbl2xml.impl;

public class RecordParent {

	public final String recordName, parentName;
	//private final String inStr; //used in testing
	
	public RecordParent(String arg) {
		//inStr = arg; //used in testing
		arg = arg.trim();
		
		String rn = arg, pn = null;
		int pos = arg.indexOf(' ');
		if (pos >= 0) {
			rn = arg.substring(0, pos);
			pn = arg.substring(pos).trim();
		}
		recordName = rn;
		parentName = pn;
	}
	
	public final boolean ok() {
		return parentName != null && parentName.length() > 0 && parentName.indexOf(' ') < 0; 
	}
	
	
//  Test Code:
	
//	private void print() {
//		System.out.println("==>" + inStr + "<- ->" + recordName + "<- ->" + parentName + "<");
//	}
//	
//	public static void main(String[] args) {
//		(new RecordParent(" aa ")).print();
//		(new RecordParent("aa bb")).print();
//		(new RecordParent(" aa bb ")).print();
//		(new RecordParent("aa  bb")).print();
//		(new RecordParent(" aa    bb ")).print();
//	}
}
