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
package net.sf.JRecord.zExamples.userExtendedLine;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LineProvider;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.Numeric.Convert;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 *
 * Purpose: Provide examples for using
 *
 *    LineProvider - LineProviders (DTAR0020provider) creates a
 *                   Line (or extension of Line). If you want to use your
 *                   own line in the record editor, you must pass a
 *                   LineProvider (which will create your line rather than
 * 					 the RecordEditor's Line).
 *
 * CobolIOProvider - LineIOProvider will return a LineReader / LineWrite
 *                   appropriate to the File structure (avaiable via
 *                   LayoutDetails.getFileStructure()
 *
 *   AbsLineReader - Line-Readers are used to read "Line's" from a file
 *                   System defined Line-Readers are:
 *                   * TextLineReader - Read Text files
 *                   * FixedLengthLineReader - read fixed record length binary
 *                     files.
 *                   * BinaryLineReader - reads a binary file with the
 *                     the Line class calculating record lengths
 *
 *   AbsLineWriter - Line-Writers write "Line's" to a file
 *                   System defined Line-Readers are:
 *                   * TextLineWriter   writes "Line's" to a text   file
 *                   * BinaryLineWriter writes "Line's" to a binary file
 *
 *            Line - Extending the Line class and using these extended
 *                   classes
 *
 * @author Bruce Martin
 * Created on 10/09/2005
 */
public final class XmplLineIO4 {

    private static final double GST_CONVERSION
    								= 1.1;
    private String installDir       = TstConstants.SAMPLE_DIRECTORY;
    private String salesFile        = installDir + "DTAR020.bin";
    private String salesFileOut     = installDir + "DTAR020out.bin";
    private String copybookName     = TstConstants.COBOL_DIRECTORY
	   + "DTAR020.cbl";

    /**
     * Example of LineReader / LineWrite classes
     */
    private XmplLineIO4() {
        super();

        int lineNum = 0;
        double gstExclusive;
        LineDTAR0020 saleRecord;

        try {
            int fileStructure = Constants.IO_FIXED_LENGTH;
            CobolIoProvider ioProvider = CobolIoProvider.getInstance();
            AbstractLineReader reader  = ioProvider.getLineReader(
                    fileStructure, Convert.FMT_MAINFRAME,
                    CopybookLoader.SPLIT_NONE, copybookName, salesFile,
                    new DTAR0020provider()
            );

            AbstractLineWriter writer  = ioProvider.getLineWriter(fileStructure, salesFileOut);

            while ((saleRecord = (LineDTAR0020) reader.read()) != null) {
                lineNum += 1;

                System.out.print(saleRecord.getKeycode()
                        + " " + saleRecord.getQuantity()
                        + " " + saleRecord.getSalesRetail());

                gstExclusive = saleRecord.getSalesRetail() / GST_CONVERSION;
                saleRecord.setSalesRetail(gstExclusive);
                writer.write(saleRecord);

                System.out.println(" " + saleRecord.getSalesRetail());
            }

            reader.close();
            writer.close();
        } catch (Exception e) {
            System.out.println("~~> " + lineNum + " " + e.getMessage());
            System.out.println();

            e.printStackTrace();
        }
    }



    /**
     * Create line provider for DTAR0020lines
     *
     *
     * @author Bruce Martin
     *
     */
    private class DTAR0020provider implements LineProvider {

        /**
         * @see net.sf.JRecord.LineProvider#getLine
         */
        public AbstractLine getLine(LayoutDetail recordDescription) {
            return new LineDTAR0020(recordDescription);
        }

        /**
         * @see net.sf.JRecord.Details.LineProvider#getLine
         */
        public AbstractLine getLine(LayoutDetail recordDescription, byte[] lineBytes) {
            return new LineDTAR0020(recordDescription, lineBytes);
        }

        /**
         * @see net.sf.JRecord.Details.LineProvider#getLine
         */
        public AbstractLine getLine(LayoutDetail recordDescription, String linesText) {
            return new LineDTAR0020(recordDescription, linesText);
        }
    }

    /**
     * LineIO example 2
     *
     * @param args program arguments
     */
    public static void main(String[] args) {
        new XmplLineIO4();
    }
}
