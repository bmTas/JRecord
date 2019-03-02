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

package net.sf.JRecord.cbl2xml.zTest.xml2cbl.cobol;

import java.io.FileNotFoundException;
import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cbl2xml.zTest.xml2cbl.Cb2XmlCode;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class GenMRHF {

	final HierarchyRecordDef file;
	
	final ICobolIOBuilder iob;
	final LayoutDetail schema;
	int lineNum = 0;
	
	private GenMRHF() throws IOException {
		iob = JRecordInterface1.COBOL
				.newIOBuilder(Cb2XmlCode.getFullName("cobol/MRHF.cbl"))
					.setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
					.setFileOrganization(Constants.IO_UNICODE_TEXT)
					.setDialect(ICopybookDialects.FMT_MAINFRAME)
				;
		schema = iob.getLayout();
		file = HierarchyRecordDef.getMrhfRecordDef(schema);
	}
	
	private void run() throws FileNotFoundException, IOException {

		AbstractLineWriter w = iob.newWriter("G:\\Temp\\MRHF.txt");
		
		do {
			doWrite(w, file);
		} while (lineNum < 500);
		w.close();
	}
	
	void doWrite(AbstractLineWriter w, HierarchyRecordDef rec) throws IOException {
		for (HierarchyRecordDef r : rec.children) {
			int num = r.nextCount();
			for (int i = 0; i < num; i++) {
				AbstractLine l = iob.newLine();
				int recIdx = r.getRecIdx();
				System.out.println(" " + r.name + " " + recIdx );
				
				lineNum += 1;
				
				l.getFieldValue(0, 0).set(r.recId);
				l.getFieldValue(0, 1).set(r.name);
				
				for (int j = 2; j < schema.getRecord(recIdx).getFieldCount(); j++) {
					l.getFieldValue(recIdx, j).set(lineNum * 10 + j);
				}
				l.setWriteLayout(recIdx);
				w.write(l);
				if (r.children != null) {
					doWrite(w, r);
				}
			}
		}
	}
	
	public static void main(String[] args) throws FileNotFoundException, IOException {
		new GenMRHF().run();
	}
}
