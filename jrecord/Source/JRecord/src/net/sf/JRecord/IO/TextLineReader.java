/*
 * Purpose: Record orientated reading of Text files
 *
 * @Author Bruce Martin
 * Created on 27/08/2005
 *
 * # Version 0.60 Bruce Martin 2007/02/16
 *   - Started work on seperating Record section out, so removing
 *     all reference to the Common module and used a new Constants
 *     module
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

package net.sf.JRecord.IO;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.CsvParser.ICsvCharLineParser;
import net.sf.JRecord.CsvParser.CsvDefinition;
import net.sf.JRecord.CsvParser.CsvParserManagerChar;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LineProvider;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.charIO.ICharReader;
import net.sf.JRecord.charIO.StandardCharReader;
import net.sf.JRecord.detailsBasic.CsvCharDetails;


/**
 * This class reads "Line's" from a text file
 *
 * @author Bruce Martin
 *
 */
public class TextLineReader extends BasicTextLineReader {

	private final boolean namesInFile;

    private CsvCharDetails defaultDelim  = CsvCharDetails.DEFAULT_DELIMITER;
    private CsvCharDetails defaultQuote  = CsvCharDetails.SINGLE_QUOTE;
    private final ICharReader reader;



	/**
	 * This class provides record oriented reading of a Text files
	 */
	public TextLineReader() {
	    super();
	    namesInFile = false;
	    reader = new StandardCharReader();
	}


	/**
	 * This class provides record oriented reading of a Text files
	 * using a user supplied Line Provider.
	 * <p><b>Note:</b> A line provider creates lines. This allows
	 * you to use your own Line's
	 *
	 * @param provider line provider
	 */
	public TextLineReader(final LineProvider provider) {
	    this(provider, false);
	}

	/**
	 *  This class provides record oriented reading of a Text files
	 * using a user supplied Line Provider.
	 * <p><b>Note:</> A line provider creates lines. This allows
	 * you to use your own Line's
	 *
	 * @param provider line provider used to create lines
	 * @param namesOn1stLine wether names are stored on the first line of
	 *        a file
	 */
	public TextLineReader(final LineProvider provider,
	        			  final boolean namesOn1stLine) {
	    this(provider, namesOn1stLine, new StandardCharReader());
	}

	/**
	 * Get Text-Line-Reader for a supplied CharReader 
	 * @param provider Line provider
	 * @param namesOn1stLine wether the file has names on the first line
	 * @param r reader
	 */
	public TextLineReader(final LineProvider provider,
			  final boolean namesOn1stLine,
			  ICharReader r) {
		super(provider);
		
		reader = r;
		namesInFile = namesOn1stLine;

	}


    /**
     * @see net.sf.JRecord.IO.AbstractLineReader#open(java.io.InputStream, net.sf.JRecord.Details.LayoutDetail)
     */
    public void open(InputStream inputStream, LayoutDetail layout)
    throws IOException {
    	String font = "";
		if (layout != null) {
			font = layout.getFontName();
		}
    	
    	super.open(reader, inputStream, layout, font);

		if (namesInFile) {
			createLayout(getReader(), inputStream, font);
		}
    }


    /**
     * create a layout
     *
     * @param pReader file read
     *
     * @throws IOException sny IO error that occurs
     */
    protected void createLayout(ICharReader pReader, InputStream inputStream, String font) throws IOException {
        LayoutDetail layout;

        RecordDetail rec = null;
	    int fieldType = Type.ftChar;
        int decimal   = 0;
        int format    = 0;
        int parser    = 0;
        int structure = Constants.IO_NAME_1ST_LINE;
        String param  = "";
        CsvCharDetails delim  = defaultDelim;
        CsvCharDetails quote  = defaultQuote;

        byte[] recordSep = Constants.SYSTEM_EOL_BYTES;
        boolean embeddedCr = false;

	    try {
	    	LayoutDetail baseLayout = getLayout();
			int ts = baseLayout.getFileStructure();
	    	if (ts != Constants.IO_GENERIC_CSV) {
	    		structure = ts;
	    	}

	    	delim     = baseLayout.getDelimiterDetails();
	        rec = baseLayout.getRecord(0);
	        quote     = rec.getQuoteDefinition();
	        parser    = rec.getRecordStyle(); 

	        fieldType = Type.ftChar;
	        decimal   = 0;
	        format    = 0;
	        param     = "";

	        if (rec.getFieldCount() == 1) {
		        fieldType = rec.getField(0).getType();
		        decimal   = rec.getField(0).getDecimal();
		        format    = rec.getField(0).getFormat();
		        param     = rec.getField(0).getParamater();
	        }
	        recordSep = baseLayout.getRecordSep();
	        font      = baseLayout.getFontName();

	        //if (rec instanceof RecordDetail) {
	        embeddedCr = rec.isEmbeddedNewLine();
	        //}
	    } catch (Exception e) {
        }

	    //System.out.println(" Quote  ->" + quote + " " + (getLayout() == null));

	    layout = createLayout(pReader.read(), rec,
	    		recordSep, structure, font,  delim,
                quote, parser, fieldType, decimal, format, param, embeddedCr);
	    //System.out.println(" Quote  ->");

	    if (layout != null) {
	        setLayout(layout);
	    }
    }

    /**
     * create a Layout from the first line in the file
     * @param line line being built
     * @param recordSep record seperator
     * @param structure File structure
     * @param fontName font name
     * @param delimiter field delimiter
     * @param quote Quote Character to use
     * @param style Identifier of the CSV parser to use
     * @param defaultFieldType field type
     * @param defaultDecimal number of decimal places
     * @param defaultFormat format to use
     * @param defaultParam param to add to each field
     * @throws IOException any error
     */
    public static LayoutDetail createLayout(String line, RecordDetail rec,
    		byte[] recordSep,
    		int structure,
            String fontName, CsvCharDetails delimiter, CsvCharDetails quote, int style,
            int defaultFieldType, int defaultDecimal, int defaultFormat, String defaultParam,
            boolean embeddedCr) throws IOException {

    	int fldType, idx;
        //int i = 0;
        LayoutDetail ret = null;
        String s;
        int decimal; int format; String param;
        IFieldDetail fldDetail;

        if (line != null) {
        	ICsvCharLineParser parser = CsvParserManagerChar.getInstance().get(style);
        	List<String> colNames = parser.getColumnNames(line, new CsvDefinition(delimiter, quote));


            int len = colNames.size();
            FieldDetail[] flds = new FieldDetail[len];
            RecordDetail[] recs = new RecordDetail[1];

            if (defaultFieldType < 0) {
            	defaultFieldType = Type.ftChar;
            }

            for (int i = 0; i < colNames.size(); i++) {
                s = colNames.get(i);
                fldType = defaultFieldType;
                decimal = defaultDecimal;
                format = defaultFormat;
                param = defaultParam;
                if (rec != null
                && (idx = rec.getFieldIndex(s)) >= 0) {
                	fldDetail = rec.getField(idx);
                	fldType = fldDetail.getType();
                	decimal = fldDetail.getDecimal();
                    format = fldDetail.getFormat();
                    param = fldDetail.getParamater();
                }
                flds[i] = new FieldDetail(s, s, fldType, decimal,
                        fontName, format, param);
                flds[i].setPosOnly(i + 1);
            }

            recs[0] = new RecordDetail("", Constants.rtDelimited,
                    delimiter.jrDefinition(), quote.jrDefinition(), fontName, flds, style, null, embeddedCr); 

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


    /**
     * set default delimiter
     * @param theDefaultDelim new default field delimeter
     */
	public void setDefaultDelim(String theDefaultDelim) {
		this.defaultDelim = CsvCharDetails.newDelimDefinition(theDefaultDelim, "");
	}


	public void setDefaultQuote(String theDefaultQuote) {
		this.defaultQuote = CsvCharDetails.newQuoteDefinition(theDefaultQuote, "");
	}


	/**
	 * @return the defaultDelim
	 */
	public String getDefaultDelim() {
		return defaultDelim.asString();
	}


	/**
	 * @return the defaultQuote
	 */
	public String getDefaultQuote() {
		return defaultQuote.asString();
	}

}
