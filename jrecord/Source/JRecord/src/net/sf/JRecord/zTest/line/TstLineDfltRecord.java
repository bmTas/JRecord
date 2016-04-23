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

package net.sf.JRecord.zTest.line;

import java.io.ByteArrayInputStream;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.RecordEditorXmlLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.LineIOProvider;
import junit.framework.TestCase;

/**
 * Test Record-ID selection and the default record selection
 * 
 * @author Bruce Martin
 *
 */
public class TstLineDfltRecord extends TestCase {

	private static final String XML_SCHEMA 
				= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
	    		+ "<RECORD RECORDNAME=\"Pershing_ACCF_ACCT_Record_Master1\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"GroupOfRecords\" LIST=\"Y\" QUOTE=\"\" RecSep=\"default\" SYSTEMNAME=\"Other\">"
	    		+ "	<RECORDS>"
	    		+ "		<RECORD RECORDNAME=\"Pershing_ACCF_ACCT_CIA\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" PARENT=\"Pershing_ACCF_ACCT_CIB\" QUOTE=\"\" RecSep=\"default\" SYSTEMNAME=\"Other\" TESTFIELD=\"Record_ID\" TESTVALUE=\"CIA\">"
	    		+ "			<FIELDS>"
	    		+ "				<FIELD NAME=\"Record_ID\" DESCRIPTION=\"Detail Record A\" POSITION=\"1\" LENGTH=\"3\" TYPE=\"Char\"/>"
	    		+ "			</FIELDS>"
	    		+ "		</RECORD>"
	    		+ "		<RECORD RECORDNAME=\"Pershing_ACCF_ACCT_CIL\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" PARENT=\"Pershing_ACCF_ACCT_CIB\" QUOTE=\"\" RecSep=\"default\" SYSTEMNAME=\"Other\" TESTFIELD=\"Record_ID\" TESTVALUE=\"CIL\">"
	    		+ "			<FIELDS>"
	    		+ "				<FIELD NAME=\"Record_ID\" DESCRIPTION=\"Detail Record L\" POSITION=\"1\" LENGTH=\"3\" TYPE=\"Char\"/>"
	    		+ "			</FIELDS>"
	    		+ "		</RECORD>"
	    		+ "		<RECORD RECORDNAME=\"Pershing_ACCF_ACCT_Header1\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\" SYSTEMNAME=\"Other\" TESTFIELD=\"Record_ID\" TESTVALUE=\"BOF\">"
	    		+ "			<FIELDS>"
	    		+ "				<FIELD NAME=\"BOF\" DESCRIPTION=\"Beginning of File\" POSITION=\"1\" LENGTH=\"18\" TYPE=\"Char\"/>"
	    		+ "			</FIELDS>"
	    		+ "		</RECORD>"
	    		+ "		<RECORD RECORDNAME=\"Pershing_ACCF_ACCT_Other\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\" SYSTEMNAME=\"Other\" TESTVALUE=\"*\">"
	    		+ "			<FIELDS>"
	    		+ "				<FIELD NAME=\"Record_ID\" POSITION=\"1\" LENGTH=\"3\" TYPE=\"Char\"/>"
	    		+ "				<FIELD NAME=\"f1\" POSITION=\"4\" LENGTH=\"80\" TYPE=\"Char\"/>"
	    		+ "			</FIELDS>"
	    		+ "		</RECORD>"
	    		+ "	</RECORDS>"
	    		+ "</RECORD>";
	
	private static final String[] FILE 
			=	{ "BOF", "CIA", "CIB", "CIC", "CIW", "CI4", "CIG", "CIH"
				, "CIX", "CIG", "CIH", "CIG", "CIH", "CIG", "CIH", "CII" 
				, "CIL", "CIA", "CIB", "CIC", "CIW", "CI4", "CII", "CIL"
				, "CIA", "CIB", "CIC", "CIW", "CI4", "CII", "CIA", "CIB" 
				, "CIC", "CIW", "CI4", "CIG", "CIH", "CIX", "CIG", "CIH" 
				, "CIG", "CIH", "CIG", "CIH", "CIG", "CIH", "CII", "CIL" 
				, "CIA", "CIB", "CIC", "CIW", "CI4", "CII", "CIL", "CIA" 
				, "CIB", "CIC", "CIW", "CI4", "CII", "CIA", "CIB", "CIC"
				, "CIW", "CI4", "CIG", "CIH", "CIX", "CIG", "CIH", "CIG" 
				, "CIH", "CIG", "CIH", "CII", "CIL", "CIA", "CIB", "CIC"
				, "CIW", "CI4", "CII", "CIL", "CIA", "CIB", "CIC", "CIW"
				, "CI4", "CII", "CIA", "CIB", "CIC", "CIW", "CI4", "CIG"
				, "CIH", "CIG", "CIH", "CIG", "CIH", "CII", "CIA", "CIB"
				, "CIC", "CIW", "CI4", "CIG", "CIH", "CIX", "CIG", "CIH"
				, "CIX", "CII", "CIJ", "CIK", "CIL", "CIN", "CIA", "CIB" 
				, "CIC", "CIW", "CI4", "CIG", "CIH", "CIX", "CIG", "CIH" 
				, "CIX", "CII", "CIJ", "CIK", "CIL", "CIN", "CIA", "CIB"
				, "CIC", "CIW", "CI4", "CIG", "CIH", "CIX", "CIG", "CIH"
				, "CIX", "CII", "CIJ", "CIK", "CIL", "CIN", "CIA", "CIB"
				, "CIC", "CIW", "CI4", "CIG", "CIH", "CIX", "CIG", "CIH"
				, "CIX", "CII", "CIJ", "CIK", "CIL", "CIN", "CIA", "CIB" 
				, "CIC", "CIW", "CI4", "CIG", "CIH", "CIX", "CIG", "CIH"
				, "CIX", "CII", "CIJ", "CIK", "CIL", "CIN", "CIA", "CIB"
				, "CIC", "CIW", "CI4", "CIG", "CIH", "CIX", "CIG", "CIH"
				, "CIX", "CII", "CIJ", "CIK", "CIL", "CIN", "CIA", "CIB" 
				, "CIC", "CIW", "CI4", "CIG", "CIH", "CIX", "CIG", "CIH"
				, "CIX", "CII", "CIJ", "CIK", "CIL", "CIN", "CIA", "CIB"
				, "CIC", "CIW", "CI4", "CIG", "CIH", "CIX", "CIG", "CIH"
				, "CIX", "CII", "CIJ", "CIK", "CIL", "CIN", "CIA", "CIB" 
				, "CIC", "CIW", "CI4", "CIG", "CIH", "CIX", "CIG", "CIH"
				, "CIX", "CII", "CIJ", "CIK", "CIL", "CIN", "CIA", "CIB"
				, "CIC", "CIW", "CI4", "CIG", "CIH", "CIX", "CII", "CIJ" 
				, "CIK", "CIL", "CIN", "CIA", "CIB", "CIC", "CIW", "CI4" 
				, "CIG", "CIH"};
	
	/**
	 * Check Record Selection / 
	 */
	public void test01() throws Exception {
		int[] expected = getExpected();
		LayoutDetail schema =(new RecordEditorXmlLoader())
			.loadCopyBook(new ByteArrayInputStream(XML_SCHEMA.getBytes()), "xx")
			.asLayoutDetail();
		AbstractLineReader r = LineIOProvider.getInstance().getLineReader(schema);
		AbstractLine l;
		int i = 0;
		
		r.open(new ByteArrayInputStream(getFileBytes()), schema);
		
		while ((l = r.read()) != null) {
			assertEquals("line: " + (i+1), expected[i++], l.getPreferredLayoutIdx());
		}
		r.close();
	}
	
	private int[] getExpected() {
		int id;
		int[] ret = new int[FILE.length];
		int i = 0;
		
		for (String s : FILE) {
			id = 3;
			if ("BOF".equals(s)) {
				id = 2;
			} else if ("CIA".equals(s)) {
				id = 0;
			} else if ("CIL".equals(s)) {
				id = 1;
			}
			ret[i++] = id;
		}
		
		return ret;
	}
	
	private byte[] getFileBytes() {
		StringBuilder b = new StringBuilder();
		String sep = "";
		
		for (String s : FILE) {
			b.append(sep).append(s);
			sep = "\n";
		}
		
		return b.toString().getBytes();
	}

}
