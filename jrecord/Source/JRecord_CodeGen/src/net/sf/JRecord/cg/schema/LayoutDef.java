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
import net.sf.JRecord.cg.common.CCode;
import net.sf.JRecord.cgen.defc.ILayoutDetails4gen;

/**
 * Class to represent a schema for use in Code Generation
 * @author Bruce Martin
 *
 */
public class LayoutDef extends JavaDetails {
	private final ILayoutDetails4gen schema;
	private final ArrayList<RecordDef> records = new ArrayList<RecordDef>();
	private final String schemaShortName;
	private final boolean xmlSchema;
	private final CodeGenFileName schemaName;
	
	public LayoutDef(ILayoutDetails4gen schema, String schemaName) {
		super(schema.getLayoutName(), Conversion.getCopyBookId(schemaName));
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
			records.add(new RecordDef( schema.getRecord(i), schemaName ));
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
	
	
	public String getJRecordLayoutType() {
		return CCode.getRecordTypeName(schema.getLayoutType());
	}
	
	public String getJRecordIoType() {
		return CCode.getJRecordIoTypeName(schema.getFileStructure());
	}

	/**
	 * @return the schemaName
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
	 * @return
	 * @see net.sf.JRecord.cgen.defc.ILayoutDetails4gen#getDelimiter()
	 */
	public String getDelimiter() {
		String d = schema.getDelimiter();
		if ("\t".equals(d)) {
			d = "\\t";
		}
		return d;
	}

	public String getQuote() {
		String q = records.get(0).getRecord().getQuote();
		if ("\"".equals(q)) {
			q = "\\\"";
		}
		
		return q;
	}
	
	public boolean areFieldNamesOnTheFirstLine() {
		return CommonBits.areFieldNamesOnTheFirstLine(schema.getFileStructure());
	}
	
	public boolean areThereColumnNames() {
		return areFieldNamesOnTheFirstLine()
			|| (   schema.getRecordCount() > 0
				&& schema.getRecord(0).getFieldCount() > 0
				&& ! "A".equals(schema.getRecord(0).getField(0).getName())
				);
	}
}
