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

package net.sf.JRecord.zTest.detailsSelection;

import java.util.List;

import junit.framework.TestCase;
import net.sf.JRecord.Common.AbstractIndexedLine;
import net.sf.JRecord.detailsSelection.AndSelection;
import net.sf.JRecord.detailsSelection.FieldSelect;
import net.sf.JRecord.detailsSelection.FieldSelectX;
import net.sf.JRecord.detailsSelection.OrSelection;

public class TstAndOr extends TestCase {
	private static final FieldSelect yes = FieldSelectX.getTrueSelection();
	private static final FieldSelect no = new FieldSelect("", "", "", null) {

		@Override
		public boolean isSelected(Object line) {
			return false;
		}
	};


	private static final FieldSelect[][]  items = {
		{no,  no},
		{no,  yes},
		{yes, no},
		{yes, yes},

		{no,  no , no},
		{no,  yes, no},
		{yes, no , no},
		{yes, yes, no},
		{no,  no,  yes},
		{no,  yes, yes},
		{yes, no,  yes},
		{yes, yes, yes},

		{no,  no,  no, no},
		{no,  yes, no, no},
		{yes, no,  no, no},
		{yes, yes, no, no},
		{no,  no,  yes, no},
		{no,  yes, yes, no},
		{yes, no,  yes, no},
		{yes, yes, yes, no},
		{no,  no,  no, yes},
		{no,  yes, no, yes},
		{yes, no,  no, yes},
		{yes, yes, no, yes},
		{no,  no,  yes, yes},
		{no,  yes, yes, yes},
		{yes, no,  yes, yes},
		{yes, yes, yes, yes},

	};

	private static final boolean[] andResult = {
			false, false, false, true,
			false, false, false, false, false, false, false, true,

			false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true,
	};
	private static final boolean[] orResult = {
		false, true, true, true,
		false, true, true, true, true, true, true, true,

		false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true,
	};


	public void testAnd() {
		AndSelection and;

		for (int i = 0; i < items.length; i++) {
			and = new AndSelection();
			for (FieldSelect f : items[i]) {
				and.add(f);
			}

			assertEquals(andResult[i], and.isSelected((AbstractIndexedLine) null));
			assertEquals(andResult[i], and.isSelected((List<AbstractIndexedLine>) null));
		}
	}


	public void testOr() {
		OrSelection and;

		for (int i = 0; i < items.length; i++) {
			and = new OrSelection();
			for (FieldSelect f : items[i]) {
				and.add(f);
			}

			assertEquals(orResult[i], and.isSelected((AbstractIndexedLine) null));
			assertEquals(orResult[i], and.isSelected((List<AbstractIndexedLine>) null));
		}
	}
}
