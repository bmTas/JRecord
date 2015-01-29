/*
 * @Author Bruce Martin
 * Created on 26/01/2006
 *
 * Purpose:
 */
package net.sf.JRecord.zExamples.cobol.toCsv;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.CsvParser.ParserManager;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.utilityClasses.Copy;
import net.sf.JRecord.utilityClasses.SchemaLoader;




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
		    LayoutDetail cobolLayout, csvLayout;
		    ExternalRecord schema;
		   
		    ParseArgsCobol2Csv csvArgs = new ParseArgsCobol2Csv(arguments);

		    CommonBits.setUseCsvLine(true); // Use the new CsvLine !!!
		    if (csvArgs.infilePresent) {
		    			// Load the Cobol Copybook and set the file-structure (supplied as an input parameter 
		        schema = SchemaLoader.loadSchema(csvArgs.copybookName, CopybookLoader.SPLIT_NONE, csvArgs.inFont, csvArgs.binFormat);
		        schema.setFileStructure(csvArgs.fileStructure);
		        
		        updateFieldNames(schema, csvArgs);         // Update the field names (change -(,) to _)
		                                                   // This must be done before the Cobol layout is generated
		                                                   // Otherwise the field-names will not match
		        
		        cobolLayout = schema.asLayoutDetail();     // Create the Cobol layout
				
				updateFields(schema, csvArgs);

				csvLayout = schema.asLayoutDetail();       // Create Csv Layout
				                                         
				                                           // Since we are sure the field number's match
				                                           // in the input/output files
				                                           // lets copy and match by field number
				Copy.copyFileByFieldNumber(cobolLayout, csvArgs.infile, csvLayout, csvArgs.outfile);
				                                           // This method sets
				                                           //    Output_Field[i] = Input_Field[i]
				                                           // for all the output fields
		    }
		} catch (Exception e) {
			e.printStackTrace();
		}
    }
    
    
    /**
     * This method updates field names, converting cobol '-' to _ and (,) to _
     *  
     * @param rec Schema to be updated
     */
    private static void updateFieldNames(ExternalRecord rec, ParseArgsCobol2Csv csvArgs) {
    	if (rec.getNumberOfRecords() == 0) {
    		ExternalField recordField;
    		for (int i = 0; i < rec.getNumberOfRecordFields(); i++) {
    			recordField = rec.getRecordField(i);
    			recordField.setName(csvArgs.updateName(recordField.getName()));
    		}
    	} else {
    		for (int i = 0; i < rec.getNumberOfRecords(); i++) {
    			updateFieldNames(rec.getRecord(i), csvArgs);
    		}
    	}
    }

    
 
    
    /**
     * This method converts Fixed width fields to Csv Fields
     * @param rec Schema to be update
     */
    private static void updateFields(ExternalRecord rec, ParseArgsCobol2Csv csvArgs) {
    	rec.setFileStructure(Constants.IO_NAME_1ST_LINE);
    	rec.setRecordType(Constants.rtDelimited);
    	rec.setQuote(csvArgs.quote);
    	rec.setDelimiter(csvArgs.sep);
    	rec.setRecordStyle(ParserManager.BASIC_CSV_PARSER);
    	rec.setFontName(csvArgs.outFont);
    	rec.setRecordStyle(csvArgs.csvParser);
    	
    	if (rec.getNumberOfRecords() == 0) {
    		updateFieldsForRecord(rec, csvArgs);
    	} else {
    		for (int i = 0; i < rec.getNumberOfRecords(); i++) {
    			updateFields(rec.getRecord(i), csvArgs);
    		}
    	}
    }
   
    	
    
    /**
     * This method converts Fixed width fields to Csv Fields
     * @param rec schema to be updated
     */
    private static void updateFieldsForRecord(ExternalRecord rec, ParseArgsCobol2Csv csvArgs) {
    	ExternalField recordField;
    	
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
