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

package net.sf.JRecord.zTest.ByteIO;

import java.io.FileWriter;
import java.io.IOException;

import junit.framework.TestCase;
import net.sf.JRecord.ByteIO.ByteTextReader;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.zTest.Common.TstConstants;

public class TstBinTextReader  extends TestCase {

	private final static String[] poLines = {
			   "H1453490000006060286225      040909        00  200 0501020501075965        LADIES KNICFT",
			   "D100007000000000000000022222500000000 43314531000000054540000007       2075359        45614531       DONKEY 24-006607 SHWL WRAP CARD",
			   "S1504300000001504500000001506500000001507600000001507900000001515100000001507200000001    00000000    00000000    00000000",
			   "D100004000000000014832000000000000000 05614944000000054540000004       2075360        5614944        MILK 24-006607 SHWL WRAP CARD",
			   "S1504500000001507600000001507900000001333149440001    00000000    00000000    00000000    00000000    00000000    00000000",
			   "H1453490000006060286225      040909        00  200 0501020501075965        LADIES KNICFT",
			   "D100007000000000000000022222500000000 43314531000000054540000007       2075359        45614531       DONKEY 24-006607 SHWL WRAP CARD",
			   "S1504300000001504500000001506500000001507600000001507900000001515100000001507200000001    00000000    00000000    00000000",
			   "D100004000000000014832000000000000000 05614944000000054540000004       2075360        5614944        MILK 24-006607 SHWL WRAP CARD",
			   "S1504500000001507600000001507900000001333149440001    00000000    00000000    00000000    00000000    00000000    00000000",
			   "H1453490000006060286225      040909        00  200 0501020501075965        LADIES KNICFT",
			   "D100007000000000000000022222500000000 43314531000000054540000007       2075359        45614531       DONKEY 24-006607 SHWL WRAP CARD",
			   "S1504300000001504500000001506500000001507600000001507900000001515100000001507200000001    00000000    00000000    00000000",
			   "D100004000000000014832000000000000000 05614944000000054540000004       2075360        5614944        MILK 24-006607 SHWL WRAP CARD",
			   "S1504500000001507600000001507900000001333149440001    00000000    00000000    00000000    00000000    00000000    00000000",
			   "H1453490000006060286225      040909        00  200 0501020501075965        LADIES KNICFT",
			   "D100007000000000000000022222500000000 43314531000000054540000007       2075359        45614531       DONKEY 24-006607 SHWL WRAP CARD",
			   "S1504300000001504500000001506500000001507600000001507900000001515100000001507200000001    00000000    00000000    00000000",
			   "D100004000000000014832000000000000000 05614944000000054540000004       2075360        5614944        MILK 24-006607 SHWL WRAP CARD",
			   "S1504500000001507600000001507900000001333149440001    00000000    00000000    00000000    00000000    00000000    00000000",
			   "H1453490000006060286225      040909        00  200 0501020501075965        LADIES KNICFT",
			   "D100007000000000000000022222500000000 43314531000000054540000007       2075359        45614531       DONKEY 24-006607 SHWL WRAP CARD",
			   "S1504300000001504500000001506500000001507600000001507900000001515100000001507200000001    00000000    00000000    00000000",
			   "D100004000000000014832000000000000000 05614944000000054540000004       2075360        5614944        MILK 24-006607 SHWL WRAP CARD",
			   "S1504500000001507600000001507900000001333149440001    00000000    00000000    00000000    00000000    00000000    00000000"
			};
	private final static String[] poLines2 = new String[poLines.length * 200];
	
	static {
		for (int i = 0; i < 200; i++) {
			for (int j = 0; j < poLines.length; j++) {
				poLines2[i * poLines.length + j] = poLines[j];
			}
		}
	}
	
	
    private static final String TMP_DIRECTORY = TstConstants.TEMP_DIRECTORY;

    public void testMac() throws IOException {
    	System.out.println("--- Testing Mac File ");
    	tstAFile(new String(Constants.LF_BYTES));
    }

    public void testLinux() throws IOException {
       	System.out.println("--- Testing Linux File ");
    	tstAFile(new String(Constants.CR_BYTES));
    }

    public void testWindows() throws IOException {
       	System.out.println("--- Testing Windows File ");
    	tstAFile(new String(Constants.CRLF_BYTES));
    }

	private void tstAFile(String eol) throws IOException {
		tstAFile(poLines, eol);
		tstAFile(poLines2, eol);
	}

	private void tstAFile(String[] lines, String eol) throws IOException {
		int i;
		String fileName = TMP_DIRECTORY + "TextTestFile.tmp";
		ByteTextReader r = new ByteTextReader();
		String s;
		
		writeAFile(fileName, lines, eol);
		
		r.open(fileName);
		for (i = 0; i < lines.length; i++) {
			s = new String(r.read());
			
			if (! lines[i].equals(s)) {
				System.out.println();
				System.out.println("--- Error on line " +i);
				System.out.println("\t" + lines[i] + "<<");
				System.out.println("\t" + s + "<<");
				System.out.println();
				assertEquals(lines[i], s);
			}
		}
		
		assertTrue("Expecting end of File, but there was data", r.read() == null);
	}
	
	
    /**
     * writes byte array to a file
     *
     * @param name major part of the file name
     * @param line data to write to the file
     * @param eol end of line character string
     *
     * @throws IOException any IO errors
     */
    private void writeAFile(String name, String[] line, String eol)
    throws IOException  {
        int i;
        FileWriter f = new FileWriter(name);
        System.out.println("--- Output file ~ " + name);

		for (i = 0; i < line.length; i++) {
		    f.write(line[i]);
		    f.write(eol);
		}

        f.close();
    }

}
