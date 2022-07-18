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

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;

/**
 * Copy a File (Continuous (no Line Marker)) to a Mainframe-VB file
 * 
 * @author Bruce Martin
 *
 */
public class CopyFileToVb {

	private static final  String TEMP_DIR = "G:\\Temp\\CobolData\\";
	
    String copybookName = TEMP_DIR + "FCUSDAT.cbl";
    String dataIn       = TEMP_DIR + "ZOS.FCUSTDAT.bin";
    String dataOut      = TEMP_DIR + "ZOS.FCUSTDAT.vb.bin";


	public void run() throws FileNotFoundException, IOException {
		AbstractLineReader r 
				= JRecordInterface1.COBOL
					.newIOBuilder(copybookName)
						.setFileOrganization(IFileStructureConstants.IO_CONTINOUS_NO_LINE_MARKER)
						.setFont("cp037")
					.newReader(dataIn);
		AbstractLineWriter w = JRecordInterface1.COBOL
					.newIOBuilder(copybookName)
						.setFileOrganization(IFileStructureConstants.IO_VB)
						.setFont("cp037")
					.newWriter(dataOut);
		AbstractLine l;
		
		while( (l = r.read()) != null ) {
			w.write(l);
		}
		
		w.close();
		r.close();
	}
	
	public static void main(String[] args) throws FileNotFoundException, IOException {
		(new CopyFileToVb())
			.run();
	}
}
