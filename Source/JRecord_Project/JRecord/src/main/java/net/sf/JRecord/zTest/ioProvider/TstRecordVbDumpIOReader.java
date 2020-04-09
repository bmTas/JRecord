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

package net.sf.JRecord.zTest.ioProvider;

import java.io.IOException;
import java.util.Arrays;

import junit.framework.TestCase;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.ToLayoutDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Numeric.ICopybookDialects;
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


	private CopybookLoader copybookInt = new CobolCopybookLoader();

    private static final String TMP_DIRECTORY = TstConstants.TEMP_DIRECTORY;


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


    public void testBinReadDtar020() throws Exception {

        String dtar020CopybookName = "DTAR020";
        String dtar020FileName = TMP_DIRECTORY + dtar020CopybookName + ".tmp";
    	byte[][] dtar020Lines = /*(byte[][])*/ TstData.DTAR020_LINES.clone();
    	ExternalRecord externalLayout = copybookInt.loadCopyBook(
                TstConstants.COBOL_DIRECTORY + dtar020CopybookName + ".cbl",
                CopybookLoader.SPLIT_NONE, 0, "cp037",
                ICopybookDialects.FMT_MAINFRAME, 0, null
        );
    	externalLayout.setFileStructure(Constants.IO_VB);
    	LayoutDetail dtar020CopyBook = ToLayoutDetail.getInstance().getLayout(externalLayout);

    	testAfile(dtar020FileName, dtar020CopyBook, dtar020Lines);
    }

    public void testBinReadDtar107() throws Exception {

        String dtar107CopybookName = "DTAR107";
        String dtar107FileName = TMP_DIRECTORY + dtar107CopybookName + ".tmp";
    	byte[][] dtar107Lines = /*(byte[][])*/ TstData.DTAR107_LINES.clone();
    	LayoutDetail dtar107CopyBook = ToLayoutDetail.getInstance().getLayout(
                copybookInt.loadCopyBook(
                        TstConstants.COBOL_DIRECTORY + dtar107CopybookName + ".cbl",
                        CopybookLoader.SPLIT_NONE, 0, "cp037",
                        ICopybookDialects.FMT_MAINFRAME, 0, null
                ));

    	testAfile(dtar107FileName, dtar107CopyBook, dtar107Lines);
    }


    public void testBinReadRBIV() throws Exception {

        String copybookName = "RBIVCopy";
        String dtar107FileName = TMP_DIRECTORY + copybookName + ".tmp";
    	byte[][] lines = /*(byte[][])*/ TstData.FUTJISU_LINES.clone();
    	LayoutDetail copyBook = ToLayoutDetail.getInstance().getLayout(
                copybookInt.loadCopyBook(
                        TstConstants.COBOL_DIRECTORY + copybookName + ".cbl",
                        CopybookLoader.SPLIT_NONE, 0, "",
                        ICopybookDialects.FMT_MAINFRAME, 0, null
                ));

    	testAfile(dtar107FileName, copyBook, lines);
    }


    public void testAfile(String fileName, LayoutDetail copyBook, byte[][] lines) 
    throws IOException, RecordException {

        int i, j;
        int copies = 5000;
        byte[][] largeFile = new byte[lines.length * copies][];

        for (i = 0; i < copies; i++) {
            for (j = 0; j < lines.length; j++) {
                largeFile[i * lines.length + j]
                          = lines[j];
            }
        }

        binReadCheck("Standard >> ", fileName, copyBook, lines);
        binReadCheck("   Large >> ", fileName, copyBook, largeFile);
        System.out.println(".. end ..");
    }

    private void binReadCheck(String id,  String fileName, LayoutDetail copyBook,
            byte[][] lines2Test)
    throws IOException, RecordException {
        AbstractLineReader tReader = LineIOProvider.getInstance().getLineReader(Constants.IO_VB_DUMP);
        AbstractLine line;
        int i = 0;
        boolean b;

        System.out.println(id + "Bin Read");
        writeAFile(fileName, lines2Test);
        tReader.open(fileName, copyBook);

        while ((line = tReader.read()) != null) {
            b = Arrays.equals(lines2Test[i], line.getData());
            if (!b) {
                System.out.println("");
                System.out.println(id + "Error Line " + i
                        + " lengths > " + lines2Test[i].length + " " + line.getData().length);
                System.out.println("  Expected: " + new String(lines2Test[i],  "CP037"));
                System.out.println("       Got: " + new String(line.getData(), "CP037"));
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
