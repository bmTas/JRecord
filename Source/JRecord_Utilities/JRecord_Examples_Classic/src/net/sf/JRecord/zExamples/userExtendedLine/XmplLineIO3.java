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
/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord "Classic" Interface examples
 *    
 *    Sub-Project purpose: Examples of using JRecord Classic (or old interface)
 *                        to perform IO on Cobol Data files
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
      
package net.sf.JRecord.zExamples.userExtendedLine;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LineProvider;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 *
 * Purpose: Provide examples for using
 *
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
 *
 * @author Bruce Martin
 * Created on 10/09/2005
 */
public final class XmplLineIO3 {

    private static final double GST_CONVERSION  = 1.1;
    private String installDir     = TstConstants.SAMPLE_DIRECTORY;
    private String salesFile      = installDir + "DTAR020.bin";
    private String salesFileOut   = installDir + "DTAR020out.bin";
    private String copybookName   = TstConstants.COBOL_DIRECTORY + "DTAR020.cbl";

    /**
     * Example of LineReader / LineWrite classes
     */
    private XmplLineIO3() {
        super();

        int lineNum = 0;
        double gstExclusive;
        AbstractLine saleRecord;

        try {
            int fileStructure = Constants.IO_FIXED_LENGTH;
            CobolIoProvider ioProvider = CobolIoProvider.getInstance();
            AbstractLineReader reader  = ioProvider.getLineReader(
                   fileStructure, ICopybookDialects.FMT_MAINFRAME,
                    CopybookLoader.SPLIT_NONE, copybookName, salesFile,
                    new DTAR0020provider()
            );
            int fldNum = 0;
            FieldDetail keycodeField = reader.getLayout().getField(0, fldNum++);
            fldNum = 4;
            FieldDetail qtyField     = reader.getLayout().getField(0, fldNum++);
            FieldDetail salesField   = reader.getLayout().getField(0, fldNum++);

            AbstractLineWriter writer  = ioProvider.getLineWriter(fileStructure, salesFileOut);

            while ((saleRecord = reader.read()) != null) {
                lineNum += 1;

                System.out.print(saleRecord.getFieldValue(keycodeField).asString()
                        + " " + saleRecord.getFieldValue(qtyField).asString()
                        + " " + saleRecord.getFieldValue(salesField).asString());

                gstExclusive =saleRecord.getFieldValue(salesField).asDouble() / GST_CONVERSION;
                saleRecord.setField(salesField, new Double(gstExclusive));
                writer.write(saleRecord);

                System.out.println(" " + saleRecord.getFieldValue(salesField).asString());
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
        new XmplLineIO3();
    }
}
