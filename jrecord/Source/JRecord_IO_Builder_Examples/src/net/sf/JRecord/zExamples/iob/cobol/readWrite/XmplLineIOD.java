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
package net.sf.JRecord.zExamples.iob.cobol.readWrite;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zTest.Common.TstConstants;


/**
 *
 * Purpose: Provide sample program to use in Example bat / shell scripts
 *
 *
 * @author Bruce Martin
 * Created on 10/09/2005
 */
public final class XmplLineIOD {

    private String salesFile        = TstConstants.SAMPLE_DIRECTORY + "DTAR020.bin";
    private String copybookName     = TstConstants.COBOL_DIRECTORY + "DTAR020.cbl";

    /**
     * Example of LineReader / LineWrite classes
     */
    private XmplLineIOD() {
        super();

        int lineNum = 0;
        AbstractLine saleRecord;
        ICobolIOBuilder ioBldr = JRecordInterface1.COBOL
        		.newIOBuilder(copybookName)
        			.setDialect( ICopybookDialects.FMT_MAINFRAME)
        			.setFont("cp037")
        			.setFileOrganization(Constants.IO_FIXED_LENGTH)
        			.setDropCopybookNameFromFields(true);

        try {
        	AbstractLineReader reader = ioBldr.newReader(salesFile);

            while ((saleRecord = reader.read()) != null) {
                lineNum += 1;

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
