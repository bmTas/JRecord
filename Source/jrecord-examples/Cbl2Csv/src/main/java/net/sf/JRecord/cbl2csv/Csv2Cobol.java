/*
 * @Author Bruce Martin
 * Created on 26/01/2006
 *
 * Purpose:
 */
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

package net.sf.JRecord.cbl2csv;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.cbl2csv.args.CommonCsv2CblCode;
import net.sf.JRecord.cbl2csv.args.ParseArgsCobol2Csv;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.def.IO.builders.ICsvIOBuilder;
import net.sf.JRecord.util.copy.IFieldNameTbl;
import net.sf.JRecord.utilityClasses.Copy;



/**
 * copy a "Cobol" file to a CSV file
 *
 * <p>At the moment (19-Oct-2014) this should be considered a Test program; 
 * so use it with <b>Caution</
 *
 * @author Bruce Martin
 *
 */
public class Csv2Cobol {


//	/**
//	 * Basic CSV schema; it will be modified as required.
//	 */
//	private static String csvXml 
//					= "<RECORD RECORDNAME=\"Generic CSV\" COPYBOOK=\"\" "
//					+     "DELIMITER=\"|\" DESCRIPTION=\"Generic CSV\" "
//					+     "FILESTRUCTURE=\"" + IFileStructureConstants.IO_NAME_1ST_LINE + "\" STYLE=\"0\" RECORDTYPE=\"Delimited\" LIST=\"Y\" "
//					+     "QUOTE=\"\" RecSep=\"default\">"
//					+        "<FIELDS><FIELD NAME=\"Field 1\" POSITION=\"1\" TYPE=\"Char\"/></FIELDS>"
//					+ "</RECORD>";
//	
    /**
     * convert a Csv Data file to a Cobol Datafile. It does this by <ul>
     *   <li> Loading the Cobol Copybook
     *   <li> Creating Csv-Copybook from the input parameters
     *   <li> Calling a Standard Copy Method (using input / Output Layouts)
     * </ul>
     * 
     * @param arguments program arguments
     */
    public static void main(String[] arguments) { 

		try {
		    ParseArgsCobol2Csv csvArgs = new ParseArgsCobol2Csv(false, arguments);
		    
		    CommonBits.setUseCsvLine(true); // Use the new CsvLine !!!

		    if (csvArgs.infilePresent) {
		    	ICobolIOBuilder iobOut = JRecordInterface1.COBOL
		        		.newIOBuilder(csvArgs.copybookName);
		        doCopy(csvArgs, iobOut, 
		        		new FileInputStream(csvArgs.infile), new FileOutputStream(csvArgs.outfile));
		    }
		} catch (Exception e) {
			e.printStackTrace();
		}
    }

	/**
	 * @param csvArgs
	 * @param iobOut
	 * @throws IOException
	 * @throws FileNotFoundException
	 */
	public static void doCopy(ParseArgsCobol2Csv csvArgs, ICobolIOBuilder iobOut, InputStream inStream, OutputStream outStream) 
			throws IOException, FileNotFoundException {
		ICsvIOBuilder iobIn = JRecordInterface1.CSV
				.newIOBuilder(csvArgs.sep, csvArgs.quote)
					.setFont(csvArgs.inFont)
					.setParser(csvArgs.csvParser)
					.setFileOrganization(csvArgs.inputFileStructure);
		LayoutDetail outSchema = iobOut.getLayout();

        iobOut	 	.setFont(csvArgs.outFont)
        			.setDialect(csvArgs.binFormat)
        			.setFileOrganization(csvArgs.outputFileStructure);

        if (! CommonBits.areFieldNamesOnTheFirstLine(csvArgs.inputFileStructure)) {
        	CommonCsv2CblCode.updateCsvNames(iobOut.getLayout(), csvArgs, iobIn.defineFields());
        }
		Copy.copyFileUsingMap(
				iobIn.newReader(inStream), 
				iobOut.newWriter(outStream), outSchema, getNameList(csvArgs, outSchema), iobOut);
	}
    
    private static IFieldNameTbl getNameList(ParseArgsCobol2Csv csvArgs, LayoutDetail schema) {
    	//ArrayList<String> list = new ArrayList<String>(schema.getRecord(0).getFieldCount());
    	FieldNamesTbl names = new FieldNamesTbl(schema.getRecord(0).getFieldCount());
    	FieldDetail  f;
    	
    	for (int i = 0; i < schema.getRecordCount(); i++) {
    		for (int j = 0; j < schema.getRecord(0).getFieldCount(); j++) {
    			f = schema.getField(i, j);
    			names.addNames(csvArgs.updateName(f.getLookupName()), csvArgs.updateNameAlt(f.getLookupName()));
    		}
    	}
      	
    	return names;
    }
    
    private static class FieldNamesTbl implements IFieldNameTbl {

    	private final List<String> fieldNames1,  fieldNames2 ;
    	
    	private FieldNamesTbl(int fieldCount) {
    		fieldNames1 = new ArrayList<>(fieldCount);
       		fieldNames2 = new ArrayList<>(fieldCount);
    	}
    	
    	public void addNames(String name1, String name2) {
    		fieldNames1.add(name1);
    		fieldNames2.add(name1 == null || name1.equals(name2) ? null : name2);
    	}
  

    	@Override
    	public int getRowCount() {
    		return fieldNames1.size();
    	}

    	@Override
    	public int getColumnCount(int row) {
    		return fieldNames2.get(row) == null ? 1 : 2;
    	}

    	@Override
    	public String getName(int row, int column) {
    		return column == 0 ? fieldNames1.get(row) : fieldNames2.get(row);
    	}

    }

    
//    /**
//     * This method updates field names, converting cobol '-' to _ and (,) to _
//     *  
//     * @param rec Schema to be updated
//     */
//    private static void updateFieldNames(ExternalRecord rec, ParseArgsCobol2Csv csvArgs) {
//    	ArrayList<DependingOn> dependingOn = rec.getDependingOn();
//    	if (dependingOn != null && dependingOn.size() > 0) {
//    		for (int i = 0; i < dependingOn.size(); i++) {
//    			DependingOn dep = dependingOn.get(i);
//    			dependingOn.set(
//    					i, 
//    					new DependingOn(
//    							csvArgs.updateName(dep.getVariableName()), 
//    							dep.getPosition(), dep.getOccursLength(), dep.getOccursMaxLength()));
//    		}
//    	}
//    	if (rec.getNumberOfRecords() == 0) {
//    		ExternalField recordField;
//    		for (int i = 0; i < rec.getNumberOfRecordFields(); i++) {
//    			recordField = rec.getRecordField(i);
//    			recordField.setName(csvArgs.updateName(recordField.getName()));
//    		}
//    	} else {
//    		for (int i = 0; i < rec.getNumberOfRecords(); i++) {
//    			updateFieldNames(rec.getRecord(i), csvArgs);
//    		}
//    	}
//    }
//
//    
//
//    
//    
//    /**
//     * Create a Csv Layout
//     * 
//     * @param csvArgs the arguments supplied to the program
//     * 
//     * @return requested layout
//     * @throws Exception any exception thrown
//     */
//    private static LayoutDetail getCsvLayout(ParseArgsCobol2Csv csvArgs) throws Exception {
//    	ExternalRecord csvRec 
//    			= ExternalRecord.newCsvRecord("Csv", IFileStructureConstants.IO_NAME_1ST_LINE, csvArgs.inFont, csvArgs.sep, csvArgs.quote)
//    							.asExternalRecord();
//    	
//    	csvRec.setRecordStyle(csvArgs.csvParser);
//    	
//    	return csvRec.asLayoutDetail();
//    }
}
