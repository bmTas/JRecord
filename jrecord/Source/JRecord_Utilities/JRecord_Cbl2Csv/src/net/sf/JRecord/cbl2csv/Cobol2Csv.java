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

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.cbl2csv.args.CommonCsv2CblCode;
import net.sf.JRecord.cbl2csv.args.ParseArgsCobol2Csv;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.def.IO.builders.ICsvIOBuilder;
import net.sf.JRecord.def.IO.builders.IDefineCsvFields;
import net.sf.JRecord.utilityClasses.Copy;



/**
 * copy a "Cobol" data file to a CSV file (mathing fields names)
 * 
 * <p>At the moment (19-Oct-2014) this should be considered a Test program; 
 * so use it with <b>Caution</
 *
 * @author Bruce Martin
 *
 */
public class Cobol2Csv {


    /**
     * convert a cobol file to a CSV file. It does this by <ul>
     *   <li> Loading the Cobol Copybook
     *   <li> Converting the Cobol Copybook to a Csv-Copybook
     *   <li> Calling a Standard Copy Method (using input / Output Layouts)
     * </ul>
     * 
     * @param arguments program arguments
     */
    public static void main(String[] arguments) { 

		try {
		    ParseArgsCobol2Csv csvArgs = new ParseArgsCobol2Csv(arguments);

		    CommonBits.setUseCsvLine(true); // Use the new CsvLine !!!
		    if (csvArgs.infilePresent) {
//		    			// Load the Cobol Copybook and set the file-structure (supplied as an input parameter 
//		        schema = SchemaLoader.loadSchema(csvArgs.copybookName, CopybookLoader.SPLIT_NONE, csvArgs.inFont, csvArgs.binFormat);
//		        schema.setFileStructure(csvArgs.inputFileStructure);
//
//		        cobolLayout = schema.asLayoutDetail();     // Create the Cobol layout
		        ICobolIOBuilder iobCbl = JRecordInterface1.COBOL
		        					.newIOBuilder(csvArgs.copybookName)
		        					.setOptimizeTypes(false);

		        runCobol2Csv(csvArgs, iobCbl, 
		        		new FileInputStream(csvArgs.infile),
		        		new FileOutputStream(csvArgs.outfile));
		    }
		} catch (Exception e) {
			System.out.println();
			System.out.println();
			e.printStackTrace();
		}
    }


	/**
	 * This method will convert a Cobol Data File to a Csv File.
	 * The method is used in automatic testing
	 * 
	 * @param csvArgs csr Arguments
	 * @param iobCbl Cobol IO Builder
	 * @param iobCsv Csv IOBuilder
	 * 
	 * @throws IOException
	 * @throws FileNotFoundException
	 */
	public static void runCobol2Csv(
			ParseArgsCobol2Csv csvArgs,
			ICobolIOBuilder iobCbl, 
			InputStream inStream,
			OutputStream outStream) throws IOException,
			FileNotFoundException {

        iobCbl 		.setFileOrganization(csvArgs.inputFileStructure)
					.setFont(csvArgs.inFont)
					.setDialect(csvArgs.binFormat);

        ICsvIOBuilder iobCsv = JRecordInterface1.CSV
				.newIOBuilder(csvArgs.sep, csvArgs.quote)
					.setFont(csvArgs.outFont)
					.setParser(csvArgs.csvParser)
					.setFileOrganization(csvArgs.outputFileStructure);

		IDefineCsvFields defineFields = iobCsv.defineFields();
		LayoutDetail cobolLayout = iobCbl.getLayout();
		
		if (cobolLayout.getRecordCount() != 1) {
			System.out.println("Expecting exactly one record, not " + cobolLayout.getRecordCount());
		} else {
			CommonCsv2CblCode.updateCsvNames(cobolLayout, csvArgs, defineFields);    // Update the field names (change -(,) to _)
		    defineFields.endOfRecord();

//					csvLayout = iobCsv.getLayout();
//					                                         
//					                                           // Since we are sure the field number's match
//					                                           // in the input/output files
//					                                           // lets copy and match by field number
//					Copy.copyFileByFieldNumber(cobolLayout, csvArgs.infile, csvLayout, csvArgs.outfile);
//					                                           // This method sets
//					                                           //    Output_Field[i] = Input_Field[i]
//					                                           // for all the output fields
			
			Copy.copyFileByFieldNumber(iobCbl.newReader(inStream), iobCsv.newWriter(outStream), iobCsv.getLayout());
		}
	}
    
    
 
    
 
//    
//    /**
//     * This method converts Fixed width fields to Csv Fields
//     * @param rec Schema to be update
//     */
//    private static void updateFields(ExternalRecord rec, ParseArgsCobol2Csv csvArgs) {
//    	rec.setFileStructure(csvArgs.outputFileStructure);
//    	rec.setRecordType(Constants.rtDelimited);
//    	rec.setQuote(csvArgs.quote);
//    	rec.setDelimiter(csvArgs.sep);
//    	rec.setRecordStyle(ParserManager.BASIC_CSV_PARSER);
//    	rec.setFontName(csvArgs.outFont);
//    	rec.setRecordStyle(csvArgs.csvParser);
//    	
//    	if (rec.getNumberOfRecords() == 0) {
//    		updateFieldsForRecord(rec, csvArgs);
//    	} else {
//    		for (int i = 0; i < rec.getNumberOfRecords(); i++) {
//    			updateFields(rec.getRecord(i), csvArgs);
//    		}
//    	}
//    }
   
    	
    
//    /**
//     * This method converts Fixed width fields to Csv Fields
//     * @param rec schema to be updated
//     */
//    private static void updateFieldsForRecord(ExternalRecord rec, ParseArgsCobol2Csv csvArgs) {
//    	ExternalField recordField;
//    	
//    	for (int i = 0; i < rec.getNumberOfRecordFields(); i++) {
//    		recordField = rec.getRecordField(i);
//    		recordField.setPos(i+1);
//    		recordField.setLen(Constants.NULL_INTEGER);
//    		recordField.setDecimal(0);
//    		recordField.setCobolName("");
//    		if (TypeManager.isNumeric(recordField.getType())) {
//    			recordField.setType(Type.ftNumAnyDecimal);
//    		} else {
//    			recordField.setType(Type.ftChar);
//    		}
//    	}
//    }

}
