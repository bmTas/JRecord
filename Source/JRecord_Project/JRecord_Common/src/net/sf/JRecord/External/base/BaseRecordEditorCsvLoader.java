/*
 * @Author Bruce Martin
 * Created on 7/02/2007
 *
 * Purpose:
 * Demonstrate writing a CopybookLoader (ie a class to read
 * a record layout or Copybook from an external file).
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

package net.sf.JRecord.External.base;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.CsvParser.BasicCsvLineParser;
import net.sf.JRecord.CsvParser.CsvDefinition;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Types.Type;

/**
 * This class reads a Record Layout (Copybook) stored in a  tab delimited file.
 * Fields in the file are<ol>
 * <li>Starting Position
 * <li>Length
 * <li>Number of places after the decimal point
 * <li>Field Name
 * <li>Field Type (String [or char], num, mainframe_zoned, fuji_zoned)
 * <li>Decimal - number of places after the decimal point
 * <li>Format Field Format (used in the Record Editor)
 * <li>Parameter - parameter for the Type / Format
 * </ol>
 *
 * <pre>
 *   <b>Usage:</b>
 *        CopybookLoader loader = new RecordEditorCsvLoader(",");
 *        LayoutDetail layout = loader.loadCopyBook(copybookName, 0, 0, "", 0, 0, null).asLayoutDetail();
 * </pre>
 *
 * @author Bruce Martin
 *
 */
public class BaseRecordEditorCsvLoader<XRecord extends BaseExternalRecord<XRecord>>  {

    //private static HashMap typeConv = new HashMap();


    private final String delimiter;
    private final IExernalRecordBuilder<XRecord> recBuilder;

    public BaseRecordEditorCsvLoader(IExernalRecordBuilder<XRecord> recBuilder, String fieldSeperator) {
    	//TypeList
    	this.delimiter = fieldSeperator;
    	this.recBuilder = recBuilder;

    }

	public final XRecord loadCopyBook(String copyBookFile,
			int splitCopybookOption, int dbIdx, String font, int binFormat,
			int systemId, AbsSSLogger log) throws Exception {
		return loadCopyBook(copyBookFile, splitCopybookOption, dbIdx, font, CommonBits.getDefaultCobolTextFormat(), binFormat, systemId, log);
	}

    /**
     * load a copybook
     */
    public XRecord loadCopyBook(String copyBookFile,
            int splitCopybookOption, int dbIdx, String font, int copybookFormat, int binFormat,
            int systemId, AbsSSLogger log) throws IOException {

    	return loadCopyBook(new FileReader(copyBookFile), copyBookFile, splitCopybookOption, dbIdx, font,
    			copybookFormat, binFormat, systemId, log);
    }
    
    /**
     * load a copybook
     */
    public XRecord loadCopyBook(InputStream in, String copyBookFile,
            int splitCopybookOption, int dbIdx, String font, int copybookFormat, int binFormat,
            int systemId, AbsSSLogger log) throws IOException {

    	return loadCopyBook(new InputStreamReader(in), copyBookFile, splitCopybookOption, dbIdx, font,
    			copybookFormat, binFormat, systemId, log);
    }
    
    public XRecord loadCopyBook(Reader r, String copyBookFile,
            int splitCopybookOption, int dbIdx, String font, int copybookFormat, int binFormat,
            int systemId, AbsSSLogger log) throws IOException {

        int rt = Constants.rtRecordLayout;
        log = TextLog.getLog(log);
        
        if (binFormat == ICopybookDialects.FMT_MAINFRAME) {
            rt = Constants.rtBinaryRecord;
        }

        XRecord rec = recBuilder.getNullRecord(
			        		Conversion.getCopyBookId(copyBookFile),
			                rt,
			                font);
        rec.setNew(true);

        insertFields(log, rec, r, copyBookFile, dbIdx);

        return rec;
    }


    /**
     * Add fields to the copybook
     * @param rec copybook
     * @param copyBookFile copybook file
     */
    private final void insertFields(AbsSSLogger log, XRecord rec, String copyBookFile, int dbIdx) {
        try {
        	insertFields(log, rec, new FileReader(copyBookFile), copyBookFile, dbIdx);
        } catch (Exception e) {
            System.out.println("Error Opening file "
                    + copyBookFile
                    + ": " + e.getMessage());
            e.printStackTrace();
            log.logMsg(AbsSSLogger.SHOW, "Error Loading Copybook: " + e.getMessage());
            log.logException(AbsSSLogger.SHOW, e);
        }
    }

    public final void insertFields(AbsSSLogger log, XRecord rec,
    		Reader  in, String layoutName, int dbIdx) {

        String s, name, typeStr, description, formatStr, param;
        String directory = null;
        BasicCsvLineParser t = BasicCsvLineParser.getInstance();
        String[] fields = new String[8];
        XRecord sr;
        ExternalField field;
        int pos, len, decimal, type, j, format;
        int idx = 0;
        int i = 1;
        int inputLine = 1;
        log = TextLog.getLog(log);
        
        try {
            BufferedReader r = new BufferedReader(in);
    		CsvDefinition csvDef = new CsvDefinition(delimiter, null);
            while ((s = r.readLine()) != null) {
                if (!s.trim().startsWith("#")) {
                    //t = new StringTokenizer(s, seperator);

                	for (j = 0; j < fields.length; j++) {
						fields[j] = t.getField(j, s, csvDef);
             //   		System.out.print("\t" + fields[j]);
                		if (fields[j] == null) {
                			fields[j] = "";
                			break;
                		}
                	}

                	idx = 1;

                    try {
                     	if (Constants.RECORD_NAME.equalsIgnoreCase(fields[0])) {
	                    	rec.setFileStructure(ExternalConversion.getFileStructure(dbIdx, fields[idx++]));
	                    	rec.setRecordType(ExternalConversion.getRecordType(dbIdx, fields[idx++]));
	                    	rec.setDelimiter(fields[idx++]);
	                    	rec.setRecordStyle(ExternalConversion.getRecordStyle(dbIdx, fields[idx++]));
	                    	rec.setQuote(fields[idx++]);
	                    	rec.setListChar(fields[idx++]);
	                    	rec.setDescription(fixDescription(fields[idx++]));
                    	} else if (Constants.SUB_RECORD_NAME.equalsIgnoreCase(fields[0])) {
                    		sr = recBuilder.getNullRecord(fields[idx++], Constants.rtRecordLayout, "");
                    		try {
                    			ExternalFieldSelection f = new ExternalFieldSelection();
                    			f.setFieldName(fields[idx++]);
                    			f.setFieldValue(fields[idx++]);
	                    		sr.setRecordSelection(f);

	                       		sr.setParentRecord(Integer.parseInt(fields[idx++]));
	                    	} catch (Exception e) {
                    			log.logMsg(AbsSSLogger.SHOW, "Error File: " + layoutName
                    					+ " line " + inputLine + " Field Number: " + idx
    	                                + " : " + e.getMessage());
							}
                       		rec.addRecord(sr);

                       		if (directory == null) {
                       			directory = getDirectory(layoutName);
                       			System.out.println("Directory: >" + directory + "<");
                       		}

                       		System.out.println("File >" + directory + fields[1] + Constants.TXT_EXTENSION);
                       		insertFields(log, sr, directory + fields[1] + Constants.TXT_EXTENSION, dbIdx);
                       	} else {
                    		idx = 0;
	                        pos  = toInt(fields[idx++], Constants.NULL_INTEGER);
	                        //System.out.println(fields[0] + " ! " + fields[1]);
	                        len  = toInt(fields[idx++], Constants.NULL_INTEGER);
	                        name = fields[idx++];
	                        description = fixDescription(fields[idx++]);
	                        typeStr = fields[idx++];

	                       	decimal = toInt(fields[idx++], 0);
	                        formatStr = fields[idx++];
	                        param     = fields[idx++];

	                        type = Type.ftChar;
	                        if (typeStr != null && ! "".equals(typeStr)) {
	                            typeStr = typeStr.toLowerCase();
	                            type = ExternalConversion.getType(dbIdx, typeStr);
	                        }
	                        format = 0;
	                        if (formatStr != null && ! "".equals(typeStr)) {
	                        	formatStr = formatStr.toLowerCase();
	                            format = ExternalConversion.getFormat(dbIdx, formatStr);
	                        }


	                        //System.out.println("\t Type: " + type);

	                        field = new ExternalField(pos, len, name, description, type,
	                                decimal, format, param, "", "", i);
	                        rec.addRecordField(field);
	                        i += 1;
                    	}
	                } catch (Exception e) {
	                    log.logMsg(AbsSSLogger.SHOW, "Error File: " + layoutName
	                    			+ " Adding line " + inputLine + " Field Number: " + idx
	                                + " : " + e.getMessage());
	                    e.printStackTrace();
	                }

                    inputLine += 1;
                }
            }
        } catch (Exception e) {
            System.out.println("Error Adding line " + inputLine
                    + " from file " + layoutName
                    + ": " + e.getMessage());
            e.printStackTrace();
            log.logMsg(AbsSSLogger.SHOW, "Error Loading Copybook: " + e.getMessage());
            log.logException(AbsSSLogger.SHOW, e);
        }
    }

    private int toInt(String v, int defaultValue) {
    	if (v != null && v.length() > 0) {
    		try {
				defaultValue = Integer.parseInt(v);
			} catch (NumberFormatException e) {
			}
    	}
    	
    	return defaultValue;
    }


    private static String getDirectory(String fileName) {
    	int e = Math.max(
    			fileName.lastIndexOf('/'),
    			fileName.lastIndexOf('\\'));

    	if (e > 0) {
    		e += 1;
    	}
    	return fileName.substring(0, e);
    }


	/**
	 * Fix up carriage returns
	 * @param description input description
	 * @return description with \n's replaced
	 */
	private String fixDescription(String description) {

		if (description == null || description.indexOf('\n') == 0) {
			return description;
		}
		StringBuilder b = new StringBuilder(description);

		Conversion.replace(b,  "\\n", "\n");
		return b.toString();
	}
}