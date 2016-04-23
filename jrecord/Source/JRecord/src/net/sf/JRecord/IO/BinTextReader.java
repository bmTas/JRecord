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

package net.sf.JRecord.IO;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.ByteIO.ByteTextReader;
import net.sf.JRecord.ByteIO.IByteReader;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.CsvParser.BinaryCsvParser;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LineProvider;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.Types.Type;

/**
 * This class will read and AbstractLine from a standard Windows/*nix Text file. It is similar in function
 * to <b>TextLineReader</b>, the difference being <b>TextLineReader</b> uses Standard BufferedReader 
 * (String based) Class to the reading and can not handle Hex (i.e. x'FF') values correctly, while this class 
 * uses streams and is able to handle Hex values. This class is based on <b>ByteTextReader</b>.
 * <p>This class was written to support hex field (x'FF') separators in csv (delimited) files.
 * 
 * @see TextLineReader
 * @see ByteTextReader
 * 
 * @author Bruce Martin
 * @version 0.68
 *
 */
public class BinTextReader extends LineReaderWrapper {

	private String defaultQuote  = "'";
	private boolean readNames;
	
	public BinTextReader(LineProvider provider, boolean nameOn1stLine) {
		this(provider, nameOn1stLine, new ByteTextReader());
	}

	public BinTextReader(LineProvider provider, boolean nameOn1stLine, IByteReader reader) {
		super(provider, reader);

		readNames = nameOn1stLine;
	}

	   /**
     * @see net.sf.JRecord.IO.AbstractLineReader#open(java.io.InputStream, net.sf.JRecord.Details.LayoutDetail)
     */
    public void open(InputStream inputStream, LayoutDetail layout)
    throws IOException {

        super.open(inputStream, layout);

		if (readNames) {
		    createLayout(super.rawRead());
		}
    }


    /**
     * create a layout
     *
     * @param pReader file read
     *
     * @throws IOException sny IO error that occurs
     */
    protected void createLayout(byte[] line) throws IOException {
        LayoutDetail layout;
	    
        RecordDetail rec = null;
	    int fieldType = Type.ftChar;
        int decimal   = 0;
        int format    = 0;
        int parser    = 0;
        int structure = Constants.IO_NAME_1ST_LINE;
        String param  = "";
        byte[] delim  = {0}; 
        String delimStr = "x'00'";
        String quote  = defaultQuote;
        String font   = "";
        byte[] recordSep = Constants.SYSTEM_EOL_BYTES;
        boolean embeddedCr = false;

	    try {
	    	int ts = getLayout().getFileStructure();
	    	if (ts != Constants.IO_GENERIC_CSV) {
	    		structure = ts;
	    	}
	    	delim     = getLayout().getDelimiterBytes();
	    	delimStr = getLayout().getDelimiter();
	        rec = getLayout().getRecord(0);
	        quote     = rec.getQuote();
	        parser    = rec.getRecordStyle();
	        fieldType = rec.getField(0).getType();
	        decimal   = rec.getField(0).getDecimal();
	        format    = rec.getField(0).getFormat();
	        param     = rec.getField(0).getParamater();
	        recordSep = getLayout().getRecordSep();
	        font      = getLayout().getFontName();

	        if (rec instanceof RecordDetail) {
	        	embeddedCr = ((RecordDetail) rec).isEmbeddedNewLine();
	        }
	    } catch (Exception e) {
        }
	    
	    //System.out.println(" Quote  ->" + quote + " " + (getLayout() == null));

	    layout = createLayout(line, rec, 
	    		recordSep, font,  delim, delimStr,
                parser, fieldType, decimal, format, 
                param, quote, structure, embeddedCr);
	    //System.out.println(" Quote  ->");

	    if (layout != null) {
	        setLayout(layout);
	    }
    }

    /**
     * create a Layout from the first line in the file
     * @param line line being built
     * @param recordSep record seperator
     * @param fontName font name
     * @param delimiter field delitmiier
     * @param delimStr field delitmiier as a string
     * @param parser Identifier of the CSV parser to use
     * @param fieldType field type
     * @param decimal number of decimal places
     * @param format format to use
     * @param param param to add to each field
     * @param quote Quote
     * @param structure file structure
     * @param embeddedCr wether there is embedded Cr in the file
     * @return Create a Layout description form a supplied line (first line of a file ?)
     * + other details
     * @throws IOException any error
     */
    public static LayoutDetail createLayout(byte[] line, RecordDetail rec,
    		byte[] recordSep,
            String fontName, byte[] delimiter, String delimStr, int parser,
            int fieldType, int decimal, int format, String param,
            String quote, int structure, boolean embeddedCr) throws IOException {

    	int fldType, idx;
        int i;
        LayoutDetail ret = null;
        String s;

        if (line != null) {
            //RecordDetail rec = getLayout().getRecord(0);
        	
        	BinaryCsvParser csvPaser = new BinaryCsvParser(delimiter[0]);
            //String fontName = fontname;
            int len = csvPaser.countTokens(line);
            FieldDetail[] flds = new FieldDetail[len];
            RecordDetail[] recs = new RecordDetail[1];

            if (fieldType < 0) {
                fieldType = Type.ftChar;
            }

            for (i = 0; i < len; i++) {
                s = csvPaser.getValue(line, i+1, fontName);
                fldType = fieldType;
                if (rec != null 
                && (idx = rec.getFieldIndex(s)) >= 0) {
                	fldType = rec.getField(idx).getType();
                }
                flds[i] = new FieldDetail(s, s, fldType, decimal,
                        fontName, format, param);
                flds[i].setPosOnly(i + 1);
            }

            recs[0] =  new RecordDetail("", Constants.rtDelimited, delimStr, quote, fontName, flds, parser, null, embeddedCr);
            		
            	//	new RecordDetail("", "", "", Constants.rtDelimited,
            	//	delimStr, quote, fontName, flds, parser);

            try {
                ret =
                    new LayoutDetail("", recs, "",
                        Constants.rtDelimited,
                        recordSep, "", fontName, null,
                        structure
                    );
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return ret;
    }

}
