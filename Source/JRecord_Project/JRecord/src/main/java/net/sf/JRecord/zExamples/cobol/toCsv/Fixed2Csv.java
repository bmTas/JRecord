/*
 * @Author Bruce Martin
 * Created on 26/01/2006
 *
 * Purpose:
 *    Convert a fixed field width file to a CSV file
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

package net.sf.JRecord.zExamples.cobol.toCsv;

import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.LineIOProvider; 


/**
 * This program copies a Fixed-Width data file to a Csv data file
 *  
 *
 * @author Bruce Martin
 *
 */
public class Fixed2Csv {

    /**
     * Copy a File described by a layout to a CSV file
     *
     * @param layout record layout
     * @param infile input file to be copied
     * @param outfile output file
     * @param sep field seperator
     *
     * @throws IOException any io-error that occurs when opening
     *         the input file
     */
    public Fixed2Csv(final LayoutDetail layout, 
    		       final String infile,   final String outfile,
    		       final String font,
    		       final String sep,	  final String quote,
    		       final IUpdateFieldName updateFldName)
    throws IOException {
        super();
        LineIOProvider ioProvider = new LineIOProvider();
        AbstractLineReader reader = ioProvider.getLineReader(layout);

        reader.open(infile, layout);

        copyFile(reader, outfile, font, sep, quote, updateFldName);
    }


    /**
     * Copy a File described by a layout to a CSV file
     *
     * @param reader input reader
     * @param outfile output file
     * @param sep field seperator
     */
    public Fixed2Csv(final AbstractLineReader reader,
            	   final String outfile,
            	   final String font,
            	   final String sep,
            	   final String quote, 
            	   final IUpdateFieldName updateFldName) {
        super();
        try {
            copyFile(reader, outfile, font, sep, quote, updateFldName);
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    /**
     * Copy a file describe by a record-layout to a CSV file
     *
     * @param reader input reader
     * @param outfile output file
     * @param sep field seperator
     *
     * @throws IOException any error that occurs
     */
    @SuppressWarnings("deprecation")
	private void copyFile(AbstractLineReader reader, String outfile, String font, String sep, String quote, IUpdateFieldName updateFldName)
    throws IOException {
        AbstractLine line;
        int idx, i;
        LayoutDetail layout = reader.getLayout();

        RecordDetail rec = layout.getRecord(0);
        OutputStreamWriter fileWriter;
        if (font == null || font.length() == 0) {
        	fileWriter = new FileWriter(outfile);
        } else {
        	fileWriter = new OutputStreamWriter(new FileOutputStream(outfile), font);
        }
        BufferedWriter writer = new BufferedWriter(fileWriter);
        
        if (updateFldName == null) {
        	updateFldName = new IUpdateFieldName() {
				@Override public String updateName(String name) {	return name;	}
			};
        }

        writer.write(updateFldName.updateName(rec.getField(0).getName()));
        for (i = 1; i < rec.getFieldCount(); i++) {
            writer.write(sep + updateFldName.updateName(rec.getField(i).getName()));
        }
        writer.newLine();

        while ((line = reader.read()) != null) {
            idx = line.getPreferredLayoutIdx();

            if (idx >= 0) {
                writer.write(formatField(line.getField(idx, 0), sep, quote));
                for (i = 1; i < layout.getRecord(idx).getFieldCount(); i++) {
                    writer.write(sep + formatField(line.getField(idx, i), sep, quote));
                }    
            }
            writer.newLine();
        }

        writer.close();
        reader.close();
    }

    private String formatField(Object value, String sep, String quote) {
    	String v;
    	if (value == null) {
    		v = "";
    	} else {
    		v = value.toString();
    		if (quote.length() == 0) {
    			v = value.toString();
    		} else if (v.indexOf(quote) >= 0) {
	    		StringBuilder sb = new StringBuilder(v);
	    		Conversion.replace(sb, quote, quote + quote);
	    		v = quote + sb.toString() + quote;
	    	} else if (v.indexOf(sep) >= 0 || v.indexOf('\n') > 0) {
	    		v = quote + v + quote;
	    	}
    	}
    	
    	return v;
    }
//
//    /**
//     * Convert Cobol/RecordEditor Name to standard name
//     *
//     * @param name current field name
//     *
//     * @return new name
//     */
//    private static String fixName(String name) {
//        StringBuilder newName = new StringBuilder(name);
//
//        if (newName.charAt(newName.length() -1) == ')') {
//        	newName.setLength(newName.length() - 1);
//        }
//        Conversion.replace(newName, "-", "_");
//        Conversion.replace(newName, " ", "_");
//        Conversion.replace(newName, "(", "_");
//        Conversion.replace(newName, ",", "_");
//        Conversion.replace(newName, ")", "_");
//        Conversion.replace(newName, "___", "_");
//        Conversion.replace(newName, "__", "_");
//      
//       return newName.toString();
//    }
}
