/*
 * @Author Bruce Martin
 * Created on 28/08/2005
 *
 * Purpose:
 */
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
import net.sf.JRecord.External.ToLayoutDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Numeric.Convert;
import net.sf.JRecord.zTest.Common.IO;
import net.sf.JRecord.zTest.Common.TstConstants;
import net.sf.JRecord.zTest.Common.TstData;

/**
 *
 *
 * @author Bruce Martin
 *
 */
public class TstRecordIOReader extends TestCase {


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
    	LayoutDetail dtar020CopyBook = ToLayoutDetail.getInstance().getLayout(
                copybookInt.loadCopyBook(
                        TstConstants.COBOL_DIRECTORY + dtar020CopybookName + ".cbl",
                        CopybookLoader.SPLIT_NONE, 0, "cp037",
                        Convert.FMT_MAINFRAME, 0, null
                ));

    	testAfile(dtar020FileName, dtar020CopyBook, dtar020Lines);
    }

    public void testBinReadDtar107() throws Exception {

        String dtar107CopybookName = "DTAR107";
        String dtar107FileName = TMP_DIRECTORY + dtar107CopybookName + ".tmp";
    	byte[][] dtar107Lines = /* (byte[][]) */ TstData.DTAR107_LINES.clone();
    	LayoutDetail dtar107CopyBook = ToLayoutDetail.getInstance().getLayout(
                copybookInt.loadCopyBook(
                        TstConstants.COBOL_DIRECTORY + dtar107CopybookName + ".cbl",
                        CopybookLoader.SPLIT_NONE, 0, "cp037",
                        Convert.FMT_MAINFRAME, 0, null
                ));

    	testAfile(dtar107FileName, dtar107CopyBook, dtar107Lines);
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
        AbstractLineReader tReader = LineIOProvider.getInstance().getLineReader(Constants.IO_FIXED_LENGTH);
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
                System.out.println(id + "Error Line " + i);
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
        IO.writeFbFile(name, bytes);
    }

}
