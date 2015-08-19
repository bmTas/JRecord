/*
 * @Author Bruce Martin
 * Created on 9/09/2005
 *
 * Purpose:
 *   This Example program demonstrates Reading a file using Line Based
 * Routines
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
 * This Example program demonstrates Reading a file using Line Based
 * Routines
 *
 * @author Bruce Martin
 *
 */
public final class XmplLineIO1 {

    /**
     *
     */
    private XmplLineIO1() {
        super();

        String vendorFile          = TstConstants.SAMPLE_DIRECTORY
		                           + "Ams_VendorDownload_20041229.txt";
        String copybookName        = TstConstants.COBOL_DIRECTORY
		                           + "AmsVendor.cbl";
        
        ICobolIOBuilder ioBldr = JRecordInterface1.COBOL
        		.newIOBuilder(copybookName)
        			.setDialect( ICopybookDialects.FMT_INTEL)
        			.setFileOrganization(Constants.IO_TEXT_LINE);
        			
        //CobolIoProvider ioProvider = CobolIoProvider.getInstance();
        AbstractLine line;
        int lineNumber = 0;

        try {
        	AbstractLineReader reader  = ioBldr.newReader(vendorFile);
        			

            System.out.println("  Vendor \t Name");
            System.out.println("  ===========================================");


            while ((line = reader.read()) != null) {
                lineNumber += 1;
                    System.out.println(
                            "  "    + line.getFieldValue("Vendor-Number").asString()
                          + "  \t " + line.getFieldValue("Vendor-Name").asString());

            }

            reader.close();

        } catch (Exception e) {
            System.out.println("Error Line " + lineNumber + " " + e.getMessage());
            System.out.println("      File " + vendorFile);
            System.out.println();
            e.printStackTrace();
        }

        System.out.println();
        System.out.println("Lines Read " + lineNumber);
    }


    /**
     * LineIO example
     *
     * @param args program arguments
     */
    public static void main(String[] args) {
        new XmplLineIO1();
    }
}
