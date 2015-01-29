/*
 * @Author Bruce Martin
 * Created on 10/09/2005
 *
 * Purpose: Provide examples of
 *
 *    LineProvider,  LineIOProvider, AbsLineReader -etc
 *
 * Requirements:
 *
 * 1) Check the values in Constants.java are correct !!!
 *
 */
package net.sf.JRecord.zExamples.cobol.readWrite;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.Numeric.Convert;
import net.sf.cb2xml.def.Cb2xmlConstants;

/**
 *
 * Purpose: Provide sample program to use in Example bat / shell scripts
 *
 *
 * @author Bruce Martin
 * Created on 10/09/2005
 */
public final class XmplLineIOD {

    private String salesFile        = "DTAR020.bin";
    private String copybookName     = "DTAR020.cbl";

    /**
     * Example of LineReader / LineWrite classes
     */
    private XmplLineIOD() {
        super();

        int lineNum = 0;
        AbstractLine saleRecord;

        try {
            int fileStructure = Constants.IO_FIXED_LENGTH;
            CobolIoProvider ioProvider = CobolIoProvider.getInstance();
            AbstractLineReader reader  = ioProvider.getLineReader(
                   fileStructure, Convert.FMT_MAINFRAME,
                    CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS, 
                    copybookName, salesFile
            );

            while ((saleRecord = reader.read()) != null) {
                lineNum += 1;

                // Note: JRecord drops the Copybook name from the start / end of
                //       a field name, so DTAR020-KEYCODE-NO becomes KEYCODE-NO

                System.out.println(saleRecord.getFieldValue("KEYCODE-NO").asString()
                        + " " + saleRecord.getFieldValue("QTY-SOLD").asString()
                        + " " + saleRecord.getFieldValue("SALE-PRICE").asString());
            }

            reader.close();
        } catch (Exception e) {
            System.out.println("~~> " + lineNum + " " + e.getMessage());
            System.out.println();

            e.printStackTrace();
        }
    }


    /**
     * LineIO example 2
     *
     * @param args program arguments
     */
    public static void main(String[] args) {
        new XmplLineIOD();
    }
}
