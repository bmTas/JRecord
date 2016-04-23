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

package net.sf.JRecord.cbl2xml.zTest.xml2cbl;

import static org.junit.Assert.*;
import net.sf.JRecord.cbl2xml.impl.RecordParent;

import org.junit.Test;

/**
 * Test the Record Parent (parameter Class
 * 
 * @author Bruce Martin
 *
 */
public class TstRecordParent {

	@Test
	public void testValid() {
		check(new RecordParent("aa bb"));
		check(new RecordParent(" aa bb "));
		check(new RecordParent("aa  bb"));
		check(new RecordParent(" aa    bb "));
	}
	@Test
	public void testInValid() {
		assertFalse(new RecordParent(" ").ok());
		assertFalse(new RecordParent("aa b c").ok());
		assertFalse(new RecordParent("aa").ok());
		assertFalse(new RecordParent(" aa ").ok());
		assertFalse(new RecordParent(" aa").ok());
		assertFalse(new RecordParent("aa ").ok());
		
	}

	private void check(RecordParent rp) {
		assertTrue(rp.ok());
		assertEquals(rp.recordName, "aa");
		assertEquals(rp.parentName, "bb");
	}
}
