/*
 * @Author Bruce Martin
 * Created on 26/01/2006
 *
 * Purpose:
 */
package net.sf.JRecord.zExamples.cobol.toCsv;

import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.Log.TextLog;
import net.sf.cb2xml.def.Cb2xmlConstants;




/**
 * copy a "Cobol" file to a CSV file
 *
 * @author Bruce Martin
 *
 */
public class Cobol2CsvAlternative {


    /**
     * convert a cobol file to a CSV file. It does this by reading the input
     * file using the Cobol Copybook and writing using Csv-output class (Fix2Csv)
     * 
     * @param arguments program arguments
     */
    public static void main(String[] arguments) { 
		CobolCopybookLoader conv = new CobolCopybookLoader();

		try {
		    LayoutDetail layout;
		    ExternalRecord schema;
		   
		    ParseArgsCobol2Csv csvArgs = new ParseArgsCobol2Csv(arguments);

		    if (csvArgs.infilePresent) {
		    			// Load the Cobol Copybook and set the file-structure (supplied as an input parameter 
		        schema = conv.loadCopyBook(csvArgs.copybookName, CopybookLoader.SPLIT_NONE, 0, csvArgs.inFont, 
		        		Cb2xmlConstants.USE_STANDARD_COLUMNS, csvArgs.binFormat, 0, new TextLog());
		        schema.setFileStructure(csvArgs.inputFileStructure);
				layout = schema.asLayoutDetail();

						// Now copy copy the input file to the output Csv file
		        new Fixed2Csv(layout, csvArgs.infile, csvArgs.outfile, csvArgs.outFont, csvArgs.sep, csvArgs.quote, csvArgs); 
		    }
		} catch (Exception e) {
			e.printStackTrace();
		}
    }
}
