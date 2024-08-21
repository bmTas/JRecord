/*
 * @Author Bruce Martin
 * Created on 13/04/2007
 *
 * Purpose:
 */
/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
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
      
package net.sf.JRecord.CsvParser;

import java.util.List;

/**
 * Interface describing a CSV line parser -
 * A class to break a <b>line</b> into the fields using a field-Seperator String
 *
 * @author Bruce Martin
 *
 */
public interface ICsvByteLineParser {

	/**
	 * Controls whether Column names on the first line are in Quotes
	 * @return whether Column names on the first line are in Quotes
	 */
	public abstract boolean isQuoteInColumnNames();

	/**
	 * Extract a field from a string
	 * @param fieldNumber  field to retrieve
	 * @param line line to parse for fields
     * @param csvDefinition Csv details like delimiter, quote etc
     * @return requested field
	 */
    public abstract String getField(int fieldNumber, byte[] line, ICsvDefinition csvDefinition);


    /**
     * Update the value of a field in a line
     * @param fieldNumber field to be updated
     * @param fieldType Type of Field (Text / Numeric / Date)
     * @param line line to update
     * @param csvDefinition Csv details like delimiter, quote etc
     * @param newValue new value of the field
     * @return updated line
     */
    public abstract byte[] setFieldByteLine(int fieldNumber, int fieldType, byte[] line, ICsvDefinition csvDefinition, String newValue);

    /**
     * Get all the fields in a line
     * @param line line to disect
     * @param csvDefinition Csv-Definition
     * @return list of fields
     */
    public abstract List<String> getFieldList(byte[] line,  ICsvDefinition csvDefinition);
    
    
    /**
     * This method converts a Line into a list of column names
     *
     * @param line line containing column names
     * @param csvDefinition Csv details like delimiter, quote etc
     *
     * @return list of column names
     */
    public List<String> getColumnNames(byte[] line, ICsvDefinition csvDefinition);


    /**
     * This method converts a list of column names to a line to be written to the file
     *
     * @param names column names
     * @param csvDefinition Csv details like delimiter, quote etc
     *
     * @return column name line
     */
    public byte[] getColumnNameByteLine(List<String> names, ICsvDefinition csvDefinition);

//    /**
//     * Get The file Structure (i.e. Reader / Writer for the CSV file)
//     * @param csvDefinition Csv Definition details.
//     * @param namesOnFirstLine wether names are on the first line 
//     * @return calculate the file-structure (or file organisation)
//     */
//    public int getFileStructure(ICsvDefinition csvDefinition, boolean namesOnFirstLine, boolean binary) ;
//    

	/**
	 * Format field list as a Csv Line
	 * @param fields fields to be organised as a line
	 * @param lineDef Csv Line Definition
	 * @param fieldTypes Field types
	 * @return Formatted Csv line
	 */
	public byte[] formatFieldListByte(List<? extends Object> fields, ICsvDefinition lineDef, int[] fieldTypes);

}
