/*
 * @Author Bruce Martin
 * Created on 13/04/2007
 *
 * Purpose:
 */
package net.sf.JRecord.CsvParser;

import java.util.List;

/**
 * Interface describing a CSV line parser -
 * A class to break a <b>line</b> into the fields using a field-Seperator String
 *
 * @author Bruce Martin
 *
 */
public interface ICsvLineParser {

	/**
	 * Controls wether Column names on the first line are in Quotes
	 * @return wether Column names on the first line are in Quotes
	 */
	public abstract boolean isQuoteInColumnNames();

	/**
	 * Extract a field from a string
	 * @param fieldNumber  field to retrieve
	 * @param line line to parse for fields
     * @param csvDefinition Csv details like delimiter, quote etc
	 */
    public abstract String getField(int fieldNumber, String line, ICsvDefinition csvDefinition);


    /**
     * Update the value of a field in a line
     * @param fieldNumber field to be updated
     * @param fieldType Type of Field (Text / Numeric / Date)
     * @param line line to update
     * @param csvDefinition Csv details like delimiter, quote etc
     * @param newValue new value of the field
     * @return updated line
     */
    public abstract String setField(int fieldNumber, int fieldType, String line, ICsvDefinition csvDefinition, String newValue);

    /**
     * Get all the fields in a line
     * @param line line to disect
     * @param csvDefinition Csv-Definition
     * @return list of fields
     */
    public abstract List<String> getFieldList(String line,  ICsvDefinition csvDefinition);
    
    
    /**
     * This method converts a Line into a list of column names
     *
     * @param line line containing column names
     * @param csvDefinition Csv details like delimiter, quote etc
     *
     * @return list of column names
     */
    public List<String> getColumnNames(String line, ICsvDefinition csvDefinition);


    /**
     * This method converts a list of column names to a line to be written to the file
     *
     * @param names column names
     * @param csvDefinition Csv details like delimiter, quote etc
     *
     * @return column name line
     */
    public String getColumnNameLine(List<String> names, ICsvDefinition csvDefinition);

    /**
     * Get The file Structure (i.e. Reader / Writer for the CSV file)
     * @param csvDefinition Csv Definition details.
     * @param namesOnFirstLine wether names are on the first line 
     * @return
     */
    public int getFileStructure(ICsvDefinition csvDefinition, boolean namesOnFirstLine, boolean binary) ;
    

	/**
	 * Format field list as a Csv Line
	 * @param fields fields to be organised as a line
	 * @param lineDef Csv Line Definition
	 * @param fieldTypes Field types
	 * @return Formatted Csv line
	 */
	public String formatFieldList(List<? extends Object> fields, ICsvDefinition lineDef, int[] fieldTypes);

}
