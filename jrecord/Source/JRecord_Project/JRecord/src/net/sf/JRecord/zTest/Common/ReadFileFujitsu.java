/*
 * @Author Bruce Martin
 * Created on 18/03/2007
 *
 * Purpose:
 */
/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
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
