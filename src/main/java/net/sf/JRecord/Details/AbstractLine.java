/*
 * @Author Bruce Martin
 * Created on 18/04/2007
 *
 * Purpose:
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

package net.sf.JRecord.Details;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.ILineFieldNames;
import net.sf.JRecord.Common.ISetData;

/**
 * Interface to represent one Line in a file. Used through out JRecord / RecordEditor
 *
 * <p>The one important method is getFieldValue
 *
 * <p>Creating:
 * <pre>
 *              AbstractLine outLine = <font color="brown"><b>new</b></font> Line(oLayout);
 *     or
 *              AbstractLine outLine = <font color="brown"><b>new</b></font> XmlLine(oLayout, recordIdx);
 * </pre>
 *
 * <p>Getting a field value:
 * <pre>
 * 	            <font color="brown"><b>long</b></font> sku = saleRecord.getFieldValue("<font color="blue"><b>KEYCODE-NO</b></font>").asLong();
 * </pre>
 *
 * <p>Updating a field:
 * <pre>
 * 	            saleRecord.getFieldValue("<font color="blue"><b>KEYCODE-NO</b></font>").set(1331);
 * </pre>
 *
 * @author Bruce Martin
 *
 */
public interface AbstractLine extends ILineFieldNames, IGetLayout, ISetData {
    /**
     *   This method completely replaces a line value.
     *   It is used to determine a record preferred record layout
     *
     * @param rec buffer holding the record
     * @param start Start of the record
     * @param len length of the record
     */
    void replace(final byte[] rec, final int start,
            final int len);

    /**
     * Get the field values as raw Text
     *
     * @param recordIdx Index of the current layout used to retrieve the field
     * @param fieldIdx Index of the current field
     *
     * @return field value (raw Text)
     */
    String getFieldText(final int recordIdx, final int fieldIdx);

    /**
     * Get the full line as text
     *
     * @return line as text
     */
    String getFullLine();

    /**
     * Get the field value as Hex
     *
     * @param recordIdx Index of the current layout used to retrieve the field
     * @param fieldIdx Index of the current field
     *
     * @return field value as a Hex String
     */
    String getFieldHex(final int recordIdx, final int fieldIdx);

    /**
     * Get the field as bytes
     * @param recordIdx record index (or identifier)
     * @param fieldIdx field index (or identifier)
     * @return field as bytes
     */
    byte[] getFieldBytes(final int recordIdx, final int fieldIdx);


    /**
     * Get the Preferred Record Layout Index for this record (alternate method)
     *
     * @return Index of the Record Layout based on the Values
     */
    int getPreferredLayoutIdxAlt();

    /**
     * Get the byte value for a specified position and length
     * @param start starting position
     * @param len length to extract
     * @return the requested bytes
     */
    byte[] getData(int start, int len);

    /**
     * get The dat in the line as an Array of Bytes
     * @return Returns the record.
     */
    byte[] getData();

    /**
     * Set the line value to the supplied string
     * @param newVal new value for the line
     */
    void setData(String newVal);
    /**
     * Set the line value to the supplied byte array
     * @param newVal new value for the line
     */
    void setData(byte[] newVal);

    /**
     * Set the record Layout - Description of the Line
     * @param pLayout The layouts to set.
     * @deprecated This was used in the RecordEditor where it's use could be
     * controlled. <b>Only</b> use it in JRecord if you know what you are doing.
     * Basically it works if the Layouts are <b>very</b> similar.
     */
    void setLayout(final LayoutDetail pLayout);

    /**
     * Get the Layout
     * @return Returns the layouts.
     */
    LayoutDetail getLayout();

    /**
     * Set Record Index to be used when writing this line
     * @param pWriteLayout The writeLayout to set.
     */
    void setWriteLayout(final int pWriteLayout);

    /**
     * Set the line provider
     *
     * @param pLineProvider The lineProvider to set.
     * 
     * @deprecated for use in JRecord
     */
    void setLineProvider(LineProvider pLineProvider);

    /**
     * Gets a fields value
     *
     * @param recordIdx Index of the RecordDescription to be used.
     * @param fieldIdx Index of the required field
     *
     * @return the request field (formated)
     *
     * @deprecated for use in JRecord, otherwise use {@link AbstractLine#getFieldValue(int, int)}
     */
    Object getField(final int recordIdx, final int fieldIdx);


    /**
     * Gets a fields value
     *
     * @param recordIdx Index of the RecordDescription to be used.
     * @param fieldIdx Index of the required field
     *
     * @return the request field (formated)
     */
    net.sf.JRecord.Details.fieldValue.IFieldValue  getFieldValue(final int recordIdx, final int fieldIdx);

    net.sf.JRecord.Details.fieldValue.IFieldValue getFieldValue(String fieldName);
    /**
     * Get a fields value
     *
     * @param field field to retrieve
     *
     * @return fields Value
     */
    net.sf.JRecord.Details.fieldValue.IFieldValue getFieldValue(IFieldDetail field);

    /**
     * Set a field via its name
     *
     * @param fieldName fieldName to be updated
     * @param value value to be applied to the field
     *
     * @deprecated use {@link AbstractLine#getFieldValue(IFieldDetail)}.set(..)
     */
    void setField(String fieldName, Object value);

    /**
     * Sets a field to a new value
     *
     * @param recordIdx record layout
     * @param fieldIdx field number in the record
     * @param val new value
     *
     * @deprecated for use in JRecord, use {@link AbstractLine#getFieldValue(int, int)}.set(..)
     */
    void setField(final int recordIdx, final int fieldIdx,
            Object val);

    /**
     * Set a fields value
     *
     * @param field field to retrieve
     * @param value value to set the field to
     *
     * @deprecated for use in JRecord, use {@link AbstractLine#getFieldValue(IFieldDetail)}.set(..)
     */
    void setField(IFieldDetail field, Object value);

    /**
     * Set the field with a Text value - ie update the field
     * without using any formatting
     *
     * @param recordIdx record layout
     * @param fieldIdx field number in the record
     * @param value new value
     *
     */
    void setFieldText(final int recordIdx, final int fieldIdx,
            String value);

    /**
     * Set a field to a Hex value
     * @param recordIdx record index
     * @param fieldIdx field index
     * @param val hex value
     */
     String setFieldHex(final int recordIdx, final int fieldIdx,
            String val);


    /**
     * Get all fields for a Record (by name)
     * @param recordName name of the Record
     * @return iterator over fields
     */
    FieldIterator getFieldIterator(String recordName);

    /**
     * Get all fields for a Record (by record-index)
     * @param recordNumber index of the Record
     * @return iterator over fields
     */
    FieldIterator getFieldIterator(int recordNumber);

    /**
     * Check if a field is defined in the record (or line).
     * <br>For Csv / XML files, the field must exist in the "record"
     * <br>For Fixed width files<ul>
     *   <li>The record must be long enough to hold the field
     *   <li>The field value must not be hex zero's. This creates a potential problem with
     * Comp fields.
     * </ul>  
     * 
     * @param rec Record Index
     * @param fldNum Field Index
     * 
     * @return whether the field exists in the line
     */
    boolean isDefined(int rec, int fldNum);

    
    /**
    * Check if a field is defined in the record (or line).
     * <br>For Csv / XML files, the field must exist in the "record"
     * <br>For Fixed width files<ul>
     *   <li>The record must be long enough to hold the field
     *   <li>The field value must not be hex zero's. This creates a potential problem with
     * Comp fields.
     *   <li>If a Occurs Depending array field, the index must be less than the maximum array index
     *   (defined by the occurs depending field).
     * </ul>  
     * 
     * @param fieldToCheck field-definition
     * @return whether the field exists in the line
     */
    boolean isDefined(IFieldDetail fieldToCheck);

	/**
	 * This basically checks to see if the field is actually present in the line,
	 * It returns true if<ul>
	 * <li>Its a normal field (not an Occurs-Depending Array field)
	 * <li>If a Occurs Depending array field, the index must be less than the maximum array index
     *   (defined by the occurs depending field).
     * </ul>
	 * @param fieldToCheck field to check
	 * @return whether this field is actual present
	 */

	boolean isFieldInLine(IFieldDetail fieldToCheck);
	
	/**
	 * This does a basic field validation test. This is basically for checking Cobol Numeric 
	 * fields. It does not check Cobol Edited Numeric fields. This is new and there may
	 * be some errors.
	 * 
	 * @param fd Field to check
	 * @return whether the field is valid or not
	 */
	boolean isValid(IFieldDetail fd);
}