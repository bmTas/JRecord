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

package net.sf.JRecord.zTest.Types;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeSignSeparate;
import junit.framework.TestCase;

public class TstSignSeperate extends TestCase {

	String[][] VALUES = {
			{"-1", "01-", "-01" },
			{"-12", "12-", "-12" },
			{"1", "01+", "+01" },
			{"12", "12+", "+12" },
	};
	public void testTrailing() throws RecordException {
		int signSepTypeId = Type.ftSignSeparateTrail;
		FieldDetail f = getField(signSepTypeId);
		TypeSignSeparate sep = new TypeSignSeparate(signSepTypeId);
		
		for (String[] vals : VALUES) {
			assertEquals(vals[1], sep.formatValueForRecord(f, vals[0]));
		}
	}
	
	public void testLeading() throws RecordException {
		int signSepTypeId = Type.ftSignSeparateLead;
		FieldDetail f = getField(signSepTypeId);
		TypeSignSeparate sep = new TypeSignSeparate(signSepTypeId);
		
		for (String[] vals : VALUES) {
			assertEquals(vals[2], sep.formatValueForRecord(f, vals[0]));
		}
	}
	

	
	private FieldDetail getField(int type) {
		FieldDetail f = new FieldDetail("", "", type, 0, "", 0, "");
		f.setPosLen(1, 3);
		return f;
	}
}
