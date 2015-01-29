/*
 * @Author Bruce Martin
 * Created on 26/01/2006
 *
 * Purpose:
 */
package net.sf.JRecord.zExamples.cobol.toCsv;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.utilityClasses.Copy;
import net.sf.JRecord.utilityClasses.SchemaLoader;




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
//					+     "FILESTRUCTURE=\"" + Constants.IO_NAME_1ST_LINE + "\" STYLE=\"0\" RECORDTYPE=\"Delimited\" LIST=\"Y\" "
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
		    LayoutDetail cobolLayout;
		    ExternalRecord schema;
		   
		    ParseArgsCobol2Csv csvArgs = new ParseArgsCobol2Csv(arguments);
		    
		    CommonBits.setUseCsvLine(true); // Use the new CsvLine !!!

		    if (csvArgs.infilePresent) {
		    			// Load the Cobol Copybook and set the file-structure (supplied as an input parameter 
		        schema = SchemaLoader.loadSchema(csvArgs.copybookName, CopybookLoader.SPLIT_NONE,csvArgs.outFont, csvArgs.binFormat);
		        schema.setFileStructure(csvArgs.fileStructure);
		        
		        updateFieldNames(schema, csvArgs);         // Update the field names (change -(,) to _)
		                                                   // This must be done before the Cobol layout is generated
		                                                   // Otherwise the field-names will not match
		        
		        cobolLayout = schema.asLayoutDetail();     // Create the Cobol layout
				


		        										   // We do not know the field order, so match on field name
				Copy.copyFileByMatchingFieldNames(getCsvLayout(csvArgs), csvArgs.infile, cobolLayout, csvArgs.outfile);
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
     * Create a Csv Layout
     * 
     * @param csvArgs the arguments supplied to the program
     * 
     * @return requested layout
     * @throws Exception any exception thrown
     */
    private static LayoutDetail getCsvLayout(ParseArgsCobol2Csv csvArgs) throws Exception {
    	ExternalRecord csvRec 
    			= ExternalRecord.newCsvRecord("Csv", Constants.IO_NAME_1ST_LINE, csvArgs.inFont, csvArgs.sep, csvArgs.quote)
    							.asExternalRecord();
    	
    	csvRec.setRecordStyle(csvArgs.csvParser);
    	
    	return csvRec.asLayoutDetail();
    }
}
