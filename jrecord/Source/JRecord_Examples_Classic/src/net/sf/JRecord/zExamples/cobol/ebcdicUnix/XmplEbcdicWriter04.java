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
      
package net.sf.JRecord.zExamples.cobol.ebcdicUnix;

import java.io.ByteArrayInputStream;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.zTest.Common.TstConstants;


/**
 * Writing an ebcidic file using \n line (record) separators
 * Using a LineIOProvider
 * 
 * @author Bruce Martin
 *
 */

public class XmplEbcdicWriter04 {

	static String charset = "cp037";
	static String cobolCopyBook = "        01  A-Line     Pic x(40).";
	
	public static void main(String[] args)  {
		String fileName =  TstConstants.TEMP_DIRECTORY
                + "cp037FixedWidth_04.txt";
		try {
			ExternalRecord schemaBldr = getCobolLayout(ICopybookDialects.FMT_MAINFRAME, cobolCopyBook);
			byte[] eolBytes = "\n".getBytes(charset);
			schemaBldr.setRecordSep(eolBytes);
			schemaBldr.setRecSepList(Constants.CR_STRING);
			
			LayoutDetail schema = schemaBldr.asLayoutDetail();
			AbstractLineWriter w = LineIOProvider.getInstance().getLineWriter(Constants.IO_TEXT_LINE);

			System.out.println("Output File: " + fileName);
			w.open(fileName);
			
			for (int i = 1; i <120; i++) {
				w.write(new Line(schema, "Line " + i));
			}
			w.close();
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		
	}
	
	
	private static ExternalRecord getCobolLayout(int cobolDialect, String cobolCopybook) throws RecordException {
		CobolCopybookLoader loader = new CobolCopybookLoader();
		ByteArrayInputStream bs = new ByteArrayInputStream(cobolCopybook.getBytes());


		return loader.loadCopyBook(bs, "ZZ-TTT4", CopybookLoader.SPLIT_NONE, 0, charset, cobolDialect, 0, new TextLog());
	}
}
