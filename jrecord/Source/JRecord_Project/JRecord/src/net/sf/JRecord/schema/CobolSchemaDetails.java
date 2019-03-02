/**
 * 
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

package net.sf.JRecord.schema;

import java.util.List;

import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.IO.builders.SchemaIOBuilder;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;
import net.sf.JRecord.schema.jaxb.ItemRecordDtls;

/**
 * Class Holds <ul>
 * <li>JRecord schema (LayoutDetail)
 * <li>"Cobol Item Tree" for each Record
 * <li>"Cobol Copybook definition 
 * <li>An IOBuilder
 * <li>General Copybook Information  
 * </ul>
 * 
 * @author Bruce Martin
 *
 */
public final class CobolSchemaDetails {

	/** Standard JRecord schema */
	public final LayoutDetail schema;
	
	/**
	 * Holds a list of Cobol-Item-Tree's one for each record 
	 * in the JRecord-Schema (LayoutDetail).
	 * Each "Item" holds the Cobol-Field + JRecord Field definition/Array Details
	 *  
	 *  <pre>
	 * So that
	 *   
	 *     JRecord Schema                           Cobol "Item" tree
	 *    +--------------+                         +-----------------+    
	 *   schema.getRecord(index)      <--->       recordItems.get(index)
	 * </pre>
	 */
	public final List<ItemRecordDtls> recordItems;
	
	
	/**
	 * Holds the Cobol-Item-Tree complete with<ul>
	 * <li>JRecord Field/Array definitions
	 * <li>Cobol copybook details like picture, usage, occurs position length etc.
	 * <li>Array Definition details
	 * </ul> 
	 */
//	public final ICopybook cobolCopybook;
	
	/** IOBuilder for the schema */
	public final ISchemaIOBuilder ioBuilder;
	
	/** Adhoc Schema (Cobol Copybook) information  */
	public final ISchemaInformation copybookInformation;
	
	
	protected CobolSchemaDetails(LayoutDetail schema,
			List<ItemRecordDtls> cobolItems, //ICopybook cobolCopybook, 
			ISchemaIOBuilder ioBuilder,
			ISchemaInformation copybookDetails) {
		super();
		this.schema = schema;
		this.recordItems = cobolItems;
//		this.cobolCopybook = cobolCopybook;
		this.ioBuilder = ioBuilder;
		this.copybookInformation = copybookDetails;
	}
	
	public final ISchemaIOBuilder newIOBuilder() {
		return SchemaIOBuilder.newSchemaIOBuilder(schema);
	}
}
