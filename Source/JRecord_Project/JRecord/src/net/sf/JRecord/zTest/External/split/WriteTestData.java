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

package net.sf.JRecord.zTest.External.split;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.ICopybookDialects;


/**
 *                   
 * @author Bruce Martin
 *
 */
public class WriteTestData {

	private LayoutDetail schema;
	
    public static void main(String[] args) throws Exception {
    	new WriteTestData();
    }
    
    
    private WriteTestData() throws Exception {
    	writeRecordDefinition("example.cbl");
 /*   	loadRecordDefinition("example1.cbl");
    	loadRecordDefinition("example2.cbl");
       	loadRecordDefinition("example3.cbl");*/
   	
    	//readFile();
    }
    
    
    /**
     * Load RecordLayout (schema) from the Cobol copybook
     * @throws Exception
     */
    private void writeRecordDefinition(String copybook) throws Exception{
    	
    	String copyName = this.getClass().getResource(copybook).getFile();

    	CobolCopybookLoader loaderXML = new CobolCopybookLoader();

    	ExternalRecord extlayoutCBL = loaderXML.loadCopyBook(
    			copyName , CopybookLoader.SPLIT_HIGHEST_REPEATING, 0,
				/* Font name */"", ICopybookDialects.FMT_MAINFRAME, 0, new TextLog());
    	
    	schema = extlayoutCBL.asLayoutDetail();
    	
    	System.out.println();
    	System.out.print("Schema: " + copybook + " Record Count:" +schema.getRecordCount());
    	
    	for (int i = 0; i < schema.getRecordCount(); i++) {
    		RecordDetail record = schema.getRecord(i);
    		System.out.println("\t), ");
			System.out.print("\t new RecordDtls(\"" + record.getRecordName() + "\", ");
			String sep = "  ";
			for (int j = 0; j < record.getFieldCount(); j++) {
				FieldDetail field = record.getField(j);
				if (j % 3 == 0) {
					System.out.println();
					System.out.print("\t\t");
				}
				System.out.print(sep + "new FieldDtls(\"" + field.getName() + "\", " + field.getPos() + ", " + field.getLen()
						+ ", " + field.getType()+ ")" );
				sep = ", ";
			}
    		System.out.println();
    	}
    }    
 }
