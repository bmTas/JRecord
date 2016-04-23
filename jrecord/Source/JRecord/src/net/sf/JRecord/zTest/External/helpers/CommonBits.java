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

package net.sf.JRecord.zTest.External.helpers;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import junit.framework.TestCase;

public class CommonBits {

	public static final String[][] CSV_FILE_DETAILS = {
			{"69684558", "20", "40118", "280", "1", "19.00"},
			{"69684558", "20", "40118", "280", "-1", "-19.00"},
			{"69684558", "20", "40118", "280", "1", "5.01"},
			{"69694158", "20", "40118", "280", "1", "19.00"},
			{"69694158", "20", "40118", "280", "-1", "-19.00"},
			{"69694158", "20", "40118", "280", "1", "5.01"},
			{"63604808", "20", "40118", "170", "1", "4.87"},
			{"62684671", "20", "40118", "685", "1", "69.99"},
			{"62684671", "20", "40118", "685", "-1", "-69.99"},
			{"64634429", "20", "40118", "957", "1", "3.99"},
			{"66624458", "20", "40118", "957", "1", "0.89"},
			{"63674861", "20", "40118", "957", "10", "2.70"},
			{"65674532", "20", "40118", "929", "1", "3.59"},
			{"64614401", "59", "40118", "957", "1", "1.99"},
			{"64614401", "59", "40118", "957", "1", "1.99"},
			{"61664713", "59", "40118", "335", "1", "17.99"},
			{"61664713", "59", "40118", "335", "-1", "-17.99"},
			{"68634752", "59", "40118", "410", "1", "8.99"},
			{"60614487", "59", "40118", "878", "1", "5.95"},
			{"63644339", "59", "40118", "878", "1", "12.65"},
			{"60694698", "59", "40118", "620", "1", "3.99"},
			{"60664659", "59", "40118", "620", "1", "3.99"},
			{"62684217", "59", "40118", "957", "1", "9.99"},
			{"67674686", "59", "40118", "929", "1", "3.99"},
			{"61684613", "59", "40118", "335", "1", "12.99"},
			{"64624770", "59", "40118", "957", "1", "2.59"},
			{"69694814", "166", "40118", "360", "1", "2.50"},
			{"69694814", "166", "40118", "360", "1", "2.50"},
			{"69644164", "166", "40118", "193", "1", "21.59"},
			{"62684907", "166", "40118", "375", "1", "13.99"},
			{"62694193", "166", "40118", "375", "1", "13.99"},
			{"62694193", "166", "40118", "375", "-1", "-13.99"},	
	};
	
	
	
	public static void compare(InputStream in1, InputStream in2, boolean skipFirst) throws IOException {
		BufferedReader r1 = new BufferedReader(new InputStreamReader(in1));
		BufferedReader r2 = new BufferedReader(new InputStreamReader(in2));
		String s1, s2;
		
		if (skipFirst) {
			s1 = r1.readLine();
			s2 = r2.readLine();
		}
		s2 = r2.readLine();
		while ((s1 = r1.readLine()) != null && s2 != null) {
			TestCase.assertEquals(s1, s2);
			s2 = r2.readLine();
		}
		TestCase.assertTrue(s1 == null || "".equals(s1));
		TestCase.assertTrue(s2 == null || "".equals(s2));
		r1.close(); 
		r2.close();
	}
	
	
	public static void compareToExpected(AbstractLineReader reader, String[][] expected) throws IOException {
    	AbstractLine saleRecord;
    	int lineNum = 0;
        while ((saleRecord = reader.read()) != null) {
        	for (int i = 0; i < expected[lineNum].length; i++) {
        		TestCase.assertEquals(expected[lineNum][i], saleRecord.getFieldValue(0, i).asString());
        	}
            lineNum += 1;
        }
        reader.close();

	}
}
