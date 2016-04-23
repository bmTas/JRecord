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
