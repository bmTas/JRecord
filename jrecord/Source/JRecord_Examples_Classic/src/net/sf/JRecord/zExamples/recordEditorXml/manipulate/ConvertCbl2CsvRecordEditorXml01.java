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
      
package net.sf.JRecord.zExamples.recordEditorXml.manipulate;

import java.io.ByteArrayInputStream;
import java.io.FileOutputStream;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.CsvParser.CsvParserManagerChar;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.CopybookWriterManager;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.zTest.Common.TstConstants;


/**
 * Purpose: This program will convert a Cobol Copybook into a Csv Copybook with the same field
 * names and matching types
 *
 * @author Bruce Martin
 *
 */
public class ConvertCbl2CsvRecordEditorXml01 {

	
	private static String cobolCopybook
			= "              03  DTAR020-KCODE-STORE-KEY.                        \n"
			+ "                  05 DTAR020-KEYCODE-NO      PIC X(08).           \n"
			+ "                  05 DTAR020-STORE-NO        PIC S9(03)   COMP-3. \n"
			+ "              03  DTAR020-DATE               PIC S9(07)   COMP-3. \n"
			+ "              03  DTAR020-DEPT-NO            PIC S9(03)   COMP-3. \n"
			+ "              03  DTAR020-QTY-SOLD           PIC S9(9)    COMP-3. \n"
			+ "              03  DTAR020-SALE-PRICE         PIC S9(9)V99 COMP-3. \n";
	
	private static byte[] copyBookBytes = cobolCopybook.getBytes();
	
	


    public static void main(String[] args) throws Exception {
    	CobolCopybookLoader loaderCBL = new CobolCopybookLoader();
    	ExternalRecord extlayoutCBL = loaderCBL.loadCopyBook(
    	    new ByteArrayInputStream(copyBookBytes),
    	    Conversion.getCopyBookId("DTAR020.cbl"),
    	    CopybookLoader.SPLIT_NONE, 0, "", ICopybookDialects.FMT_FUJITSU, 0, new TextLog());
    	
    	extlayoutCBL.setRecordName("Csv_DTAR020");
     	updateFields(extlayoutCBL);
    	
    	CopybookWriterManager writerManager = CopybookWriterManager.getInstance();
    	net.sf.JRecord.External.base.CopybookWriter writer
    			= writerManager.get(CopybookWriterManager.RECORD_EDITOR_XML_WRITER);

    	writer.writeCopyBook(new FileOutputStream(TstConstants.TEMP_DIRECTORY + "DTAR020_Csv.Xml"), extlayoutCBL, null);
    }
    
    
    /**
     * This method converts Fixed width fields to Csv Fields
     * @param rec Schema to be update
     */
    private static void updateFields(ExternalRecord rec) {
    	if (rec.getNumberOfRecords() == 0) {
    		updateFieldsForRecord(rec);
    	} else {
    		for (int i = 0; i < rec.getNumberOfRecords(); i++) {
    			updateFields(rec.getRecord(i));
    		}
    	}
    }
    
    
    /**
     * This method converts all Fixed width fields to Csv Fields
     * @param rec Schema to be update
     */
    private static void updateFieldsForRecord(ExternalRecord rec) {
    	ExternalField recordField;
    	rec.setFileStructure(Constants.IO_NAME_1ST_LINE);
    	rec.setRecordType(Constants.rtDelimited);
    	rec.setQuote("\"");
    	rec.setDelimiter("<Tab>");
    	rec.setRecordStyle(CsvParserManagerChar.BASIC_CSV_PARSER);
    	
    	for (int i = 0; i < rec.getNumberOfRecordFields(); i++) {
    		recordField = rec.getRecordField(i);
    		recordField.setPos(i+1);
    		recordField.setLen(Constants.NULL_INTEGER);
    		recordField.setDecimal(0);
    		recordField.setCobolName("");
    		if (TypeManager.isNumeric(recordField.getType())) {
    			recordField.setType(Type.ftNumAnyDecimal);
    		} else {
    			recordField.setType(Type.ftChar);
    		}
    	}
    }
}

