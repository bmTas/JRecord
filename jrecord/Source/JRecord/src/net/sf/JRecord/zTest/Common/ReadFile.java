/*
 * @Author Bruce Martin
 * Created on 18/03/2007
 *
 * Purpose:
 */
package net.sf.JRecord.zTest.Common;

import net.sf.JRecord.ByteIO.FixedLengthByteReader;

/**
 *
 *
 * @author Bruce Martin
 *
 */
public class ReadFile {

    private static final int    DTAR107_RECORD_LENGTH = 54;
    private static final String DTAR107_FILE
    	= TstConstants.SAMPLE_DIRECTORY + "DTAR107.bin";
    /**
     *
     */
    public ReadFile(String filename, int len) {
        super();
        int i;
        byte[] line;
        FixedLengthByteReader r = new FixedLengthByteReader(len);

        try {
            r.open(filename);

            while ((line = r.read()) != null) {
                System.out.print("        {");
                for (i = 0; i < line.length; i++) {
                    System.out.print(line[i] + ", ");
                    if (i % 19 == 18) {
                        System.out.println();
                        System.out.print("         ");
                    }
                }
                System.out.println("},");
            }
            r.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        new ReadFile(DTAR107_FILE, DTAR107_RECORD_LENGTH);
    }
}
