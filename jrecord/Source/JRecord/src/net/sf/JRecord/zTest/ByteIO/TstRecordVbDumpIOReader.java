/*
 * @Author Bruce Martin
 * Created on 28/08/2005
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

package net.sf.JRecord.zTest.ByteIO;

import java.io.IOException;
import java.util.Arrays;

import junit.framework.TestCase;
import net.sf.JRecord.ByteIO.AbstractByteReader;
import net.sf.JRecord.ByteIO.VbDumpByteReader;
import net.sf.JRecord.zTest.Common.IO;
import net.sf.JRecord.zTest.Common.TstConstants;
import net.sf.JRecord.zTest.Common.TstData;

/**
 *
 *
 * @author Bruce Martin
 *
 */
public class TstRecordVbDumpIOReader extends TestCase {


	//private CopybookLoader copybookInt = new CobolCopybookLoader();

    private static final String TMP_DIRECTORY = TstConstants.TEMP_DIRECTORY;
    //private boolean writeFiles = true;


	//private boolean dbUpdated = false;


    /**
     * @see TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();

     }


    /**
     * @see TestCase#tearDown()
     */
    protected void tearDown() throws Exception {
        super.tearDown();

    }


    public void testBinReadDtar0020() throws Exception {

        String dtar020CopybookName = "DTAR020";
        String dtar020FileName = TMP_DIRECTORY + dtar020CopybookName + ".tmp";
    	byte[][] dtar020Lines = /*(byte[][])*/ TstData.DTAR020_LINES.clone();

    	testAfile(dtar020FileName, dtar020Lines);
    }

    public void testBinReadDtar107() throws Exception {

        String dtar107CopybookName = "DTAR107";
        String dtar107FileName = TMP_DIRECTORY + dtar107CopybookName + ".tmp";
    	byte[][] dtar107Lines = /*(byte[][])*/ TstData.DTAR107_LINES.clone();

    	testAfile(dtar107FileName, dtar107Lines);
    }

    public void testBinReadCombined() throws Exception {

        String dtar107CopybookName = "xxxx";
        String dtar107FileName = TMP_DIRECTORY + dtar107CopybookName + ".tmp";
    	byte[][] dtar107Lines = new byte[TstData.DTAR020_LINES.length
    	                               + TstData.DTAR107_LINES.length][];
    	int i, j;

    	j = 0;
    	for (i = 0; i < TstData.DTAR020_LINES.length; i++) {
    	    dtar107Lines[j++] = TstData.DTAR020_LINES[i];
    	}
    	for (i = 0; i < TstData.DTAR107_LINES.length; i++) {
    	    dtar107Lines[j++] = TstData.DTAR107_LINES[i];
    	}

    	testAfile(dtar107FileName, dtar107Lines);
    }

    public void testBinReadFutjisu() throws Exception {

        String copybookName = "xxxx";
        String dtar107FileName = TMP_DIRECTORY + copybookName + ".tmp";
 
    	testAfile(dtar107FileName, TstData.FUTJISU_LINES);
    }


    public void testAfile(String fileName, byte[][] lines) throws IOException {

        int i, j;
        int copies = 5000;
        byte[][] largeFile = new byte[lines.length * copies][];

        for (i = 0; i < copies; i++) {
            for (j = 0; j < lines.length; j++) {
                largeFile[i * lines.length + j]
                          = lines[j];
            }
        }

        binReadCheck("Standard >> ", fileName, lines);
        binReadCheck("   Large >> ", fileName, largeFile);
        System.out.println(".. end ..");
    }

    private void binReadCheck(String id,  String fileName, byte[][] lines2Test)
    throws IOException {
        AbstractByteReader tReader = new VbDumpByteReader();
        byte[] line;
        int i = 0;
        boolean b;

        System.out.println(id + "Bin Read");
        writeAFile(fileName, lines2Test);
        tReader.open(fileName);

        while ((line = tReader.read()) != null) {
            b = Arrays.equals(lines2Test[i], line);
            if (!b) {
                System.out.println("");
                System.out.println(id + "Error Line " + i
                        + " lengths > " + lines2Test[i].length + " " + line.length);
                System.out.println("  Expected: " + new String(lines2Test[i],  "CP037"));
                System.out.println("       Got: " + new String(line, "CP037"));
                System.out.println("");

                assertTrue(id + "Bin Line " + i + " is not correct ", b);
            }
            i += 1;
        }

        assertEquals(id + "Expected to read " + lines2Test.length
                   + " got " + i, lines2Test.length, i);

        tReader.close();
    }



    /**
     * writes byte array to a file
     *
     * @param name major part of the file name
     * @param bytes data to write to the file
     *
     * @throws IOException any IO errors
     */
    private void writeAFile(String name, byte[][] bytes)
    throws IOException  {
        IO.writeVbDumpFile(name, bytes);
    }

}
