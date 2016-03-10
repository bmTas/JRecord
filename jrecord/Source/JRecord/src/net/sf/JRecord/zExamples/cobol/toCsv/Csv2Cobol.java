/*
 * @Author Bruce Martin
 * Created on 26/01/2006
 *
 * Purpose:
 */
package net.sf.JRecord.zExamples.cobol.toCsv;

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
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.def.IO.builders.ICsvIOBuilder;
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
		    ParseArgsCobol2Csv csvArgs = new ParseArgsCobol2Csv(true, arguments);
		    
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
					.setFileOrganization(Constants.IO_UNICODE_NAME_1ST_LINE);
		LayoutDetail outSchema = iobOut.getLayout();

        iobOut	 	.setFont(csvArgs.outFont)
        			.setDialect(csvArgs.binFormat)
        			.setFileOrganization(csvArgs.outputFileStructure);

		Copy.copyFileUsingMap(
				iobIn.newReader(inStream), 
				iobOut.newWriter(outStream), outSchema, getNameList(csvArgs, outSchema), iobOut);
	}
    
    private static List<String> getNameList(ParseArgsCobol2Csv csvArgs, LayoutDetail schema) {
    	ArrayList<String> list = new ArrayList<String>(schema.getRecord(0).getFieldCount());
    	FieldDetail  f;
    	
    	for (int i = 0; i < schema.getRecordCount(); i++) {
    		for (int j = 0; j < schema.getRecord(0).getFieldCount(); j++) {
    			f = schema.getField(i, j);
    			list.add(csvArgs.updateName(f.getLookupName()));
    		}
    	}
    	
    	return list;
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
//    			= ExternalRecord.newCsvRecord("Csv", Constants.IO_NAME_1ST_LINE, csvArgs.inFont, csvArgs.sep, csvArgs.quote)
//    							.asExternalRecord();
//    	
//    	csvRec.setRecordStyle(csvArgs.csvParser);
//    	
//    	return csvRec.asLayoutDetail();
//    }
}
