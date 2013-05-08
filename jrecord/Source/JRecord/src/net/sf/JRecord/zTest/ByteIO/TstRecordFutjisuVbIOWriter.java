/*
 * @Author Bruce Martin
 * Created on 28/08/2005
 *
 * Purpose:
 */
package net.sf.JRecord.zTest.ByteIO;

import java.io.IOException;
import java.util.Arrays;

import junit.framework.TestCase;
import net.sf.JRecord.ByteIO.AbstractByteReader;
import net.sf.JRecord.ByteIO.FujitsuVbByteReader;
import net.sf.JRecord.ByteIO.FujitsuVbByteWriter;
import net.sf.JRecord.zTest.Common.IO;
import net.sf.JRecord.zTest.Common.TstConstants;
import net.sf.JRecord.zTest.Common.TstData;

/**
 *
 *
 * @author Bruce Martin
 *
 */
public class TstRecordFutjisuVbIOWriter extends TestCase {


	private static final String TMP_DIRECTORY = TstConstants.TEMP_DIRECTORY;
 
    private final String fileName = TMP_DIRECTORY + "VbTestFile.tmp";
	private final byte[][] dtar020Lines = TstData.DTAR020_LINES;



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


    public void testBinWrite1() throws Exception {

        tst1file(dtar020Lines);
    }


    public void testBinWrite2() throws Exception {

        tst1file(TstData.DTAR107_LINES);
    }


    public void testBinWrite3() throws Exception {

        tst1file(TstData.FUTJISU_LINES);
    }

    public void testBinWrite4() throws Exception {
        int l = TstData.FUTJISU_LINES.length
        	  + TstData.DTAR107_LINES.length
        	  + dtar020Lines.length;
        byte[][] lines = new byte[l][];
        int i, j;

        j = 0;
        for (i = 0; i < TstData.DTAR107_LINES.length; i++) {
            lines[j++] = TstData.DTAR107_LINES[i];
        }
        for (i = 0; i < dtar020Lines.length; i++) {
            lines[j++] = dtar020Lines[i];
        }
        for (i = 0; i < TstData.FUTJISU_LINES.length; i++) {
            lines[j++] = TstData.FUTJISU_LINES[i];
         }
        System.out.println("... " + j);


        tst1file(lines);
    }

    public void tst1file(byte[][] lines)
    throws IOException {

        int i, j;
        int copies = 5000;
        byte[][] largeFile = new byte[lines.length * copies][];

        for (i = 0; i < copies; i++) {
            for (j = 0; j < lines.length; j++) {
                largeFile[i * lines.length + j]
                          = lines[j];
            }
        }

        binReadCheck("Standard >> ", lines);
        binReadCheck("   Large >> ", largeFile);
        System.out.println(".. end ..");
    }

    private void binReadCheck(String id, byte[][] lines2Test)
    throws IOException {
        AbstractByteReader tReader = new FujitsuVbByteReader();
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
                System.out.println(id + "Error Line " + i);
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
     * @param details file layout details
     *
     * @throws IOException any IO errors
     */
    private void writeAFile(String name, byte[][] bytes)
    throws IOException  {

        IO.writeAFile(new FujitsuVbByteWriter(), name, bytes);
    }

}
