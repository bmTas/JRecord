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
public class ReadFileFujitsu {

    private static final int    DTAR107_RECORD_LENGTH = 54;
    private static final String DTAR107_FILE
    	= TstConstants.SAMPLE_DIRECTORY + "FujitsuVariableWidthFile.seq";
    /**
     *
     */
    public ReadFileFujitsu(String filename, int len) {
        super();
        int i, linenum;
        byte[] line;
        FixedLengthByteReader r = new FixedLengthByteReader(len);

        try {
            r.open(filename);
            linenum = 1;

            while (linenum++ < 16 && (line = r.read()) != null) {
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
        new ReadFileFujitsu(DTAR107_FILE, DTAR107_RECORD_LENGTH);
    }
}
