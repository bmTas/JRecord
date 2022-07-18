/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord IOBuilder examples
 *    
 *    Sub-Project purpose: Examples of using JRecord IOBuilders
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
      
package net.sf.JRecord.zExamples.iob.cobol.readWrite;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Arrays;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

/**
 * Copy a File (Continuous (no Line Marker)) to a Mainframe-VB file
 * 
 * @author Bruce Martin
 *
 */
public class CopyFile150Lines {

	private static final int LINE_COUNT = 150;
	private static final  String TEMP_DIR = "G:\\Temp\\CobolData\\";
	
    String copybookName = TEMP_DIR + "FCUSDAT.cbl";
    String dataIn       = TEMP_DIR + "ZOS.FCUSTDAT.bin";
    String dataOutVb    = TEMP_DIR + "ZOS.FCUSTDAT_150.vb.bin";
    String dataOutCont  = TEMP_DIR + "ZOS.FCUSTDAT_150.bin";


    ICobolIOBuilder iob, iobVb;
    
    public CopyFile150Lines() {
		iob = JRecordInterface1.COBOL
				.newIOBuilder(copybookName)
					.setFileOrganization(IFileStructureConstants.IO_CONTINOUS_NO_LINE_MARKER)
					.setFont("cp037");
		iobVb = JRecordInterface1.COBOL
				.newIOBuilder(copybookName)
				.setFileOrganization(IFileStructureConstants.IO_VB)
				.setFont("cp037");
    }

	public void readWrite() throws FileNotFoundException, IOException {
		AbstractLineReader r = iob .newReader(dataIn);
		AbstractLineWriter wC  = iob .newWriter(dataOutCont);
		
		AbstractLineWriter wVB = iobVb .newWriter(dataOutVb);
		System.out.println(" --> " + dataOutCont + " " + dataOutVb);
		AbstractLine l;
		int lineNum = 0;
		
		while( lineNum++ < LINE_COUNT && (l = r.read()) != null ) {
			wVB.write(l);
			wC.write(l);
		}
		
		wVB.close();
		wC.close();
		r.close();
	}
	
	void chk() throws FileNotFoundException, IOException {
		System.out.println("Check Continuous");
		chk(iob.newReader(dataIn), iob.newReader(dataOutCont));
		System.out.println();
		System.out.println("Check VB");
		chk(iob.newReader(dataIn), iobVb.newReader(dataOutVb));
	}
	
	void chk(AbstractLineReader r1, AbstractLineReader r2) throws IOException {
		AbstractLine l1, l2;
		int lineNum = 0;
		while (lineNum++ < LINE_COUNT && (l1 = r1.read()) != null & (l2 = r2.read()) != null) {
			if (! Arrays.equals(l1.getData(), l2.getData())) {
				System.out.print("\t" + lineNum);
			}
		}
	}
	
	public static void main(String[] args) throws FileNotFoundException, IOException {
		CopyFile150Lines cpyFile =(new CopyFile150Lines());
		cpyFile.readWrite();
		
		cpyFile.chk();
	}
}
