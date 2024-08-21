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

import java.util.Map;

import net.sf.JRecord.schema.fieldRename.IGetRecordFieldByName;
import net.sf.JRecord.schema.jaxb.IItem;

/**
 * Schema Adhoc-Information class
 * 
 * @author Bruce Martin
 *
 */
public interface ISchemaInformation {


	public static final int D_NO_DUPLICATES = 1;
	public static final int D_NO_DUPLICATES_IN_RECORD = 2;
	public static final int D_DUPLICATES = 3;

//	/**
//	 * These values allow you to "Rename the Cobol Names"
//	 */
//	public static final int RO_LEAVE_ASIS = 0;
//	public static final int RO_MINUS_TO_UNDERSCORE = 1;
//	public static final int RO_CAMEL_CASE = 2;

	/**
	 * Get the Map of "Array's" by Level-Name (Group / Field names)
	 * 
	 * @return Map of "Cobol Array Definitions" by "Updated-Cobol-Name"
	 */
	public abstract Map<String, ? extends IItem> getArrayItems();

	/**
	 * Map hold Record-Hierarchy-Level by record name
	 * e.g. If you have a "Record-Hierarchy" of
	 * 
	 * Purchase-Order
	 *     +----------- Pack-Record
	 *                       +-----------Location-Record
	 *                  
	 * The Record-Hierarchy Map will hold
	 * 
	 *   Purchase-Order  --> 0  
	 *   Pack-Record     --> 1
	 *   Location-Record --> 2
	 *  
	 * @return
	 */
	public abstract Map<String, Integer> getRecordHierarchyMap();

//	/**
//	 * Get the Record-Index for a name
//	 * @param name Record-Name
//	 * @return Record-Index
//	 */
//	public abstract int getRecordIndex(String name);

	/**
	 *  If you have Record-Hierarchy like:
	 *  
	 * 	Purchase-Order
	 *     +----------- Pack-Record
	 *                       +-----------Location-Record
     *
     * The Hierarchy Depth would be 3
	 * @return The Hierarchy Depth
	 * 
	 */
	public abstract int getMaxRecordHierarchyLevel();

	/**
	 * @return Type of field name duplicates in the copybook, possible values are<ul
	 * <li>D_NO_DUPLICATES
	 * <li>D_NO_DUPLICATES_IN_RECORD 
	 * <li>D_DUPLICATES
	 * </ul>
	 */
	public abstract int getDuplicateFieldsStatus();

	/**
	 * Reformat the Cobol name to output format (Camel Case etc)
	 * @param name Cobol field name
	 * @return reformatted field name
	 */
	public abstract String updateName(String name);

	/**
	 * @return Get class that can find a field from the Record/Field name
	 */
	public abstract IGetRecordFieldByName getFieldLookup();

	/**
	 * @return Wether there are redefined binary field.
	 * Used in cb2xml to decide on validility checks for
	 * Redefined fields
	 */
	public abstract boolean isRedefinedBinaryField();

}