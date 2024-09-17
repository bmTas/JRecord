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

package net.sf.JRecord.External;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Objects;

import javax.xml.parsers.ParserConfigurationException;

import lombok.extern.slf4j.Slf4j;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.CsvParser.BasicCsvLineParser;
import net.sf.JRecord.CsvParser.CsvDefinition;
import net.sf.JRecord.CsvParser.CsvParserManagerChar;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Types.Type;

import org.xml.sax.SAXException;

/**
 * Class to load DB Table definition extract (in particular DB2). It intended for use
 * with Database CSV Extracts.
 * With DB2 UDB you can use the following to create a combined copybook file for each Table:
 *
 * <pre>
 *    SELECT 'EXPORT TO "C:\Data\RecordEditor\CsvCopybooks\'  || TabName || '.Csv" OF DEL MESSAGES "C:\export.txt"  '
 *    || 'SELECT ''DB_DVMT_'' || c.TABSCHEMA, c.TABNAME, c.COLNAME, c.COLNO, c.TYPENAME, c.LENGTH, c.SCALE '
 *    ||  ' FROM SYSCAT.COLUMNS AS c '
 *    || ' where c.TABSCHEMA = ''' || TABSCHEMA || ''' and c.TABNAME = ''' || TabName || ''' '
 *    || ' ORDER BY  c.TABNAME ASC, c.COLNO ASC; '
 *    FROM SYSCAT.TABLES where TABSCHEMA = 'ETB';;
 * </pre
 *
 * @author Bruce Martin
 *
 */

@Slf4j
public class DbCsvCopybookLoader extends BaseCopybookLoader {

	private static final HashMap<String, Integer> typeConv = new HashMap<>();
	static {
		typeConv.put("CHARACTER", Type.ftChar);
		typeConv.put("DATE", Type.ftChar);
		typeConv.put("DECIMAL", Type.ftNumLeftJustified);
		typeConv.put("INTEGER", Type.ftNumLeftJustified);
		typeConv.put("SMALLINT", Type.ftNumLeftJustified);
		typeConv.put("TIME", Type.ftChar);
		typeConv.put("TIMESTAMP", Type.ftChar);
		typeConv.put("VARCHAR", Type.ftChar);
	}

    public ExternalRecord loadCopyBook(String copyBookFile,
			int splitCopybookOption, int dbIdx, String font, int copybookFormat, int binFormat,
			int systemId, AbsSSLogger log) throws IOException, SAXException,
			ParserConfigurationException {
		int rt = Constants.rtGroupOfRecords;

		ExternalRecord rec = ExternalRecord.getNullRecord(
				Conversion.getCopyBookId(copyBookFile),
				rt,
				font);

		insertFields(rec, copyBookFile, dbIdx, font);

		if (rec.getNumberOfRecords() == 1) {
			rec = rec.getRecord(0);
		}

		return rec;
	}

	/**
	 * Add fields to the copybook
	 * @param copyBookFile copybook file
	 */
	@SuppressWarnings("deprecation")
	private void insertFields(ExternalRecord parentRec, String copyBookFile, int dbIdx, String font) {
		String s, name, typeStr, system, lastSystem, lastCopybookName, copybookName;
		BasicCsvLineParser t = BasicCsvLineParser.getInstance();
		String[] fields = new String[7];
		ExternalField field;
		int pos, len, decimal, type, j, idx;
		int i = 1;
		int inputLine = 1;
		int rt = Constants.rtDelimited;
		ExternalRecord rec = null;

		lastSystem = null;
		lastCopybookName = null;
        try (BufferedReader r = new BufferedReader(new FileReader(copyBookFile))) {
            while ((s = r.readLine()) != null) {
                if (!s.trim().startsWith("#")) {
                    for (j = 0; j < fields.length; j++) {
                        String delimiter = ",";
                        fields[j] = removeQuotes(t.getField(j, s, new CsvDefinition(delimiter, "\"")));
                    }


                    try {
                        idx = 0;
                        system = fields[idx++];
                        copybookName = fields[idx++];
                        name = fields[idx++];
                        pos = Integer.parseInt(fields[idx++]) + 1;
                        typeStr = fields[idx++];
                        len = Integer.parseInt(fields[idx++]);

                        decimal = Integer.parseInt(fields[idx]);

                        type = Type.ftChar;
                        if (typeStr != null && !typeStr.isEmpty()) {
                            typeStr = typeStr.toUpperCase();
                            if (typeConv.containsKey(typeStr)) {
                                type = typeConv.get(typeStr);
                            }
                        }

                        field = new ExternalField(pos, len, name, "", type,
                                decimal, 0, "", "", "", i);

                        if (rec == null || (!Objects.equals(system, lastSystem)) || (!copybookName.equals(lastCopybookName))) {
                            rec = ExternalRecord.getNullRecord(
                                    copybookName,
                                    rt,
                                    font);
                            parentRec.addRecord(rec);
                            rec.setNew(true);
                            rec.setDelimiter(",");
                            rec.setListChar("Y");
                            rec.setQuote("\"");
                            rec.setRecordStyle(CsvParserManagerChar.DB_CSV_PARSER);
                            rec.setSystemName(system.trim());

                            lastSystem = system;
                            lastCopybookName = copybookName;
                        }
                        rec.addRecordField(field);
                        i += 1;
                    } catch (Exception e) {
                        log.error("Error Adding line {} from file {} : ",  inputLine, copyBookFile, e);
                    }
                    inputLine += 1;
                }
            }
        } catch (Exception e) {
            log.error("Error Adding line {} from file {} : {}",  inputLine, copyBookFile, e.getMessage(), e);
        }
        // TODO: handle exception
    }

	private static String removeQuotes(String s) {
		if (s != null && s.startsWith("\"")) {
			s = s.substring(1, s.length() - 1);
		}
		return s;
	}


}
