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

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.cbl2csv.args.ParseArgsCobol2Csv;
import net.sf.JRecord.cbl2csv.args.RecordSelect;
import net.sf.JRecord.cbl2csv.imp.CobolToCsvBldr;
import net.sf.JRecord.cbl2csv.imp.ICobolToCsvBldr;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;



/**
 * copy a "Cobol" data file to a CSV file (mathing fields names)
 * 
 * <p>At the moment (19-Oct-2014) this should be considered a Test program; 
 * so use it with <b>Caution</
 *
 * @author Bruce Martin
 *
 */
public class Cobol2CsvMultiRecord {


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
		    ParseArgsCobol2Csv csvArgs = ParseArgsCobol2Csv.multiRecArgs(arguments);

		    CommonBits.setUseCsvLine(true); // Use the new CsvLine !!!
		    if (csvArgs.infilePresent) {
		        ICobolIOBuilder iobCbl = JRecordInterface1.COBOL
		        					.newIOBuilder(csvArgs.copybookName)
		        					.setDialect(csvArgs.binFormat)
		        					.setFileOrganization(csvArgs.inputFileStructure)
		        					.setFont(csvArgs.inFont)
		        					.setSplitCopybook(csvArgs.split)
		        					.setOptimizeTypes(csvArgs.reportInvalid);
				
				for (RecordSelect rs : csvArgs.recordSelect) {
					iobCbl.setRecordSelection(rs.recordName, newFieldSelection(rs.fieldName, rs.value));
				}

				ICobolToCsvBldr csvBldr = CobolToCsvBldr.newMultiRecordCsvBuilder()
								.setSeparator(csvArgs.sep)
								.setQuote(csvArgs.quote)
								.setOutputCharacterSet(csvArgs.outFont)
								.setLineReader(iobCbl.newReader(csvArgs.infile))
								.setUpdateFieldName(csvArgs.updateFieldName)
								.setWriteRecordName(csvArgs.addRecordNameToCsv)
								.setReportInvalidFields(csvArgs.reportInvalid)
							;
				String outFileName = csvArgs.outfile;
				
				if (csvArgs.lowValuesTxt != null) {
					csvBldr.setLowValueTxt(csvArgs.lowValuesTxt);
				}
				if (csvArgs.highValuesTxt != null) {
					csvBldr.setHighValueTxt(csvArgs.highValuesTxt);
				}
				if (csvArgs.numericSpaceTxt != null) {
					csvBldr.setNumericSpacesTxt(csvArgs.numericSpaceTxt);
				}
				
				if (outFileName.indexOf("${record}") >= 0) {
					csvBldr.setOutputFile(outFileName, "${record}");
				} else if (outFileName.indexOf("&record;") >= 0) {
					csvBldr.setOutputFile(outFileName, "&record;");
				} else if (outFileName.indexOf("{record}") >= 0) {
					csvBldr.setOutputFile(outFileName, "{record}");
				} else{
					csvBldr.setOutputFile(outFileName);
				}
				
				csvBldr.run();
		    }
		} catch (Exception e) {
			System.out.println();
			System.out.println();
			e.printStackTrace();
		}
    }

	/**
	 * Create a Record-Selection based on field / value test
	 * @param fieldName name of the field to be checked
	 * @param value value to be tested against
	 * @return requested check
	 */
    public static ExternalFieldSelection newFieldSelection(String fieldName, String value) {
    	ExternalFieldSelection r = new ExternalFieldSelection(fieldName, value);
    	r.setCaseSensitive(false);
    	return r;
    }

}
