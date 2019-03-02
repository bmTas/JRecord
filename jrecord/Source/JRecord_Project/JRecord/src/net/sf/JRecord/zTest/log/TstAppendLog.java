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

package net.sf.JRecord.zTest.log;

import net.sf.JRecord.Log.AppendableLog;
import junit.framework.TestCase;

public class TstAppendLog extends TestCase {
	String[] lines = {
		"a line",
		"Line  2",
		"Line  3",
		"Line  4",
		"Yet another line",
	};
	public void test01() {
		StringBuilder b = new StringBuilder();
		StringBuilder b1 = new StringBuilder();
		AppendableLog l = new AppendableLog(b);
		
		for (String s : lines) {
			b1.append('\n').append(s).append('\n');
			l.logMsg(0, s);
			assertEquals(b1.toString(), b.toString());
		}
	}
}
