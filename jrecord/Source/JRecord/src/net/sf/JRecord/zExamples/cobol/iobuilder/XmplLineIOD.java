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
package net.sf.JRecord.zExamples.cobol.iobuilder;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;


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
        	ICobolIOBuilder iob = CobolIoProvider.getInstance()
					.newIOBuilder(copybookName)
						.setFont("cp037")                                   // US EBCDIC
						.setFileOrganization(Constants.IO_FIXED_LENGTH_RECORDS);  
        	AbstractLineReader reader = iob.newReader(salesFile);

            while ((saleRecord = reader.read()) != null) {
                lineNum += 1;

                System.out.println(saleRecord.getFieldValue("DTAR020-KEYCODE-NO").asString()
                        + " " + saleRecord.getFieldValue("DTAR020-QTY-SOLD").asString()
                        + " " + saleRecord.getFieldValue("DTAR020-SALE-PRICE").asString());
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
