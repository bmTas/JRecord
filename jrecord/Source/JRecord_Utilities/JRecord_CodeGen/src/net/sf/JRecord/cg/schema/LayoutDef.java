/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord CodeGen
 *    
 *    Sub-Project purpose: Generate Java - JRecord source code 
 *                        to read/write cobol data files.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: GPL 3 or later
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU General Public License
 *    as published by the Free Software Foundation; either
 *    version 3.0 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.cg.schema;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.cgen.def.ILayoutDetails4gen;
import net.sf.JRecord.cgen.support.Code2JRecordConstants;

/**
 * Class to represent a Layout (file schema) for use in Code Generation
 * @author Bruce Martin
 *
 */
public class LayoutDef extends JavaDetails {
	private final ILayoutDetails4gen schema;
	private final ArrayList<RecordDef> records = new ArrayList<RecordDef>();
	private final String schemaShortName;
	private final boolean xmlSchema;
	private final CodeGenFileName schemaName;
	
	public LayoutDef(ILayoutDetails4gen schema, String schemaName, String className) {
		super(schema.getLayoutName(), Conversion.getCopyBookId(schemaName), className);
		this.schema = schema;
		this.schemaName = new CodeGenFileName(schemaName);
		this.xmlSchema = schemaName.toLowerCase().endsWith(".xml");
		if (this.schemaName == null) {
			schemaShortName = "";
		} else { 
			schemaShortName = (new File(schemaName)).getName();
		}
		
		schemaName = Conversion.getCopyBookId(schemaName);
		records.ensureCapacity(schema.getRecordCount()); 
		for (int i = 0; i < schema.getRecordCount(); i++) {
			records.add(new RecordDef( 
					schema.getRecord(i), 
					schemaName ,
					className == null ? null : schema.getRecord(i).getRecordName(),
					schema.isCsvLayout()));
		}

	}

	/**
	 * get the actual schema
	 * @return the schema
	 */
	public final ILayoutDetails4gen getSchema() {
		return schema;
	}

	/**
	 * get the Records in the schema
	 * @return the records
	 */
	public final List<RecordDef> getRecords() {
		return records;
	}
	
	/**
	 * Get the JRecord Record-Type-Code
	 * 
	 * @return JRecord Record-Type-Code
	 */
	public String getJRecordLayoutType() {
		return Code2JRecordConstants.getRecordTypeName(schema.getLayoutType());
	}
	
	/**
	 * Get the JRecord IO-Code-name
	 * @return
	 */
	public String getJRecordIoType() {
		return Code2JRecordConstants.getJRecordIoTypeName(schema.getFileStructure());
	}

	/**
	 * Get the File Schema name (Cobol Copybook)
	 * 
	 * @return the Schema name (Cobol Copybook)
	 */
	public final CodeGenFileName getSchemaName() {
		return schemaName;
	}

	/**
	 * @return the schemaShortName
	 */
	public final String getSchemaShortName() {
		return schemaShortName;
	}

	/**
	 * @return the xmlSchema
	 */
	public final boolean isXmlSchema() {
		return xmlSchema;
	}

	/**
	 * Get the Csv field delimiter
	 * 
	 * @return Csv field delimiter
	 * @see net.sf.JRecord.cgen.def.ILayoutDetails4gen#getDelimiter()
	 */
	public String getDelimiter() {
		String d = schema.getDelimiterDetails().jrDefinition();
		if ("\t".equals(d)) {
			d = "\\t";
		}
		return d;
	}

	/**
	 * Get the Csv Quote char
	 * @return Csv Quote char
	 */
	public String getQuote() {
		String q = records.get(0).getRecord().getQuoteDefinition().jrDefinition();
		if ("\"".equals(q)) {
			q = "\\\"";
		}
		
		return q;
	}
	
	/**
	 * are field names are on the first line of a Csv file ??
	 * @return if field names are on the first line of a Csv file
	 */
	public boolean areFieldNamesOnTheFirstLine() {
		return CommonBits.areFieldNamesOnTheFirstLine(schema.getFileStructure());
	}
	
	/**
	 * wether Csv column names have been defined
	 * @return wether Csv column names have been defined
	 */
	public boolean areThereColumnNames() {
		return areFieldNamesOnTheFirstLine()
			|| (   schema.getRecordCount() > 0
				&& schema.getRecord(0).getFieldCount() > 0
				&& ! "A".equals(schema.getRecord(0).getField(0).getName())
				);
	}
}
