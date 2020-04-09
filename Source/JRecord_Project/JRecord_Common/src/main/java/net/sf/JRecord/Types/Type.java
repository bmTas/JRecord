/*
 * @Author Bruce Martin
 * Created on 27/08/2005
 *
 * Purpose: To format a field for display on the screen and convert it
 * back to the record format
 *
 *
 * Modification log:
 * On 2006/06/28 by Jean-Francois Gagnon:
 *    - Added new constants for Fujitsu Zoned Numeric
 *      and Separate Sign numeric
 *  Version 0.56 Bruce martin
 *    - Added new constants for null terminated char
 *      and null padded char
 *
 *  Version 0.60 Bruce Martin
 *    - Added getFieldType to help with sorting etc
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
 *    Copyright (c) 2005, Bruce Martin / Jean-Francois Gagnon, All Rights Reserved.
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
      
package net.sf.JRecord.Types;

import net.sf.JRecord.Common.IFieldDetail;

/**
 * A "Type" is the interface between the raw data in the file
 * and the internal format (what is to be displayed on the screen in the RecordEditor).
 * A "Type" class needs
 * to perform all conversions between how a field is stored
 * in a file and Java Strings for display on the screen.
 *
 * @author Bruce Martin
 *
 * @version 0.55
 */
public interface Type {

    public static final int BASE_16 = 16;

    public static final int USER_RANGE_START    = 1000;
    public static final int DEFAULT_USER_RANGE_SIZE = 75;
    public static final int LAST_SYSTEM_TYPE    = 250;

	public static final int NULL_INT             = -121;

	public static final int ftChar               = 0;
	public static final int ftCharRightJust      = 1;
	public static final int ftCharNullTerminated = 2;
	public static final int ftCharNullPadded     = 3;

	public static final int ftHex                = 4;
	public static final int ftNumLeftJustified   = 5;
	public static final int ftNumRightJustified  = 6;
	public static final int ftNumZeroPadded      = 7;
	public static final int ftAssumedDecimal     = 8;
	public static final int ftSignSeparateLead   = 9;
	public static final int ftSignSeparateTrail  = 10;
	public static final int ftSignSepLeadActualDecimal  = 44;
	public static final int ftSignSepTrailActualDecimal = 45;
	public static final int ftDecimal            = 11;
	public static final int ftBinaryInt          = 15;
	public static final int ftPostiveBinaryInt   = 16;
	public static final int ftFloat              = 17;
	public static final int ftDouble             = 18;
	public static final int ftNumAnyDecimal      = 19;
	public static final int ftPositiveNumAnyDecimal  = 20;
	public static final int ftBit                = 21;
	public static final int ftAssumedDecimalPositive = 22;
	public static final int ftBinaryIntPositive  = 23;

	public static final int ftNumZeroPaddedPN    = 24;
	public static final int ftNumZeroPaddedPositive  = 25;
	public static final int ftNumCommaDecimal  = 26;
	public static final int ftNumCommaDecimalPN  = 27;
	public static final int ftNumCommaDecimalPositive= 28;

	public static final int ftNumRightJustifiedPN  = 29;

	public static final int ftPackedDecimal      = 31;
	public static final int ftZonedNumeric       = 32;
//	public static final int ftZonedLeading       = 30;
	public static final int ftPackedDecimalPostive = 33;
//	public static final int ftZonedNumericPositive = 34;
	public static final int ftBinaryBigEndian    = 35;
	public static final int ftBinaryBigEndianPositive = 39;
	public static final int ftPositiveBinaryBigEndian = 36;
	public static final int ftRmComp             = 37;
	public static final int ftRmCompPositive     = 38;

	public static final int ftFjZonedNumeric     = 41;
	public static final int ftNumRightJustCommaDp   = 42;
	public static final int ftNumRightJustCommaDpPN = 43;
	public static final int ftGnuCblZonedNumeric    = 46;
	
	/**
	 * Use ftMultiLineChar instead
	 */
	@Deprecated 
	public static final int ftCharMultiLine      = 51;


	public static final int ftDate               = 71;
	public static final int ftDateYMD            = 72;
	public static final int ftDateYYMD           = 73;
	public static final int ftDateDMY            = 74;
	public static final int ftDateDMYY           = 75;

	public static final int ftCharRestOfFixedRecord = 80;
	public static final int ftCharRestOfRecord   = 81;
	public static final int ftCharNoTrim		 = 82;

	public static final int ftProtoField         = 91;
	public static final int ftAvroField          = 91;
	public static final int ftArrayField         = 92;
	public static final int ftComboItemField     = 93;
	public static final int ftAvroUnionField     = 94;

	public static final int ftCheckBoxY          = 109;
	public static final int ftCheckBoxTrue       = 110;
	public static final int ftCheckBoxYN         = 111;
	public static final int ftCheckBoxTF         = 112;
	public static final int ftCheckBoxBoolean    = 114;

	public static final int ftCsvArray           = 115;
	public static final int ftXmlNameTag         = 116;
	public static final int ftMultiLineEdit      = 117;
	public static final int ftMultiLineChar      = 118; /* used in PO / Tip Files */
	public static final int ftHtmlField          = 119;
	
	public static final int ftRecordEditorType   = 130;
	public static final int ftNumOrEmpty         = 131;

	/** @deprecated  Small <= 9 bytes Packed decimal - internal JRecord use **/
	public static final int ftPackedDecimalSmall    = 140;
	/** @deprecated  Small <= 9 bytes Positive (unsigned) Packed decimal  - internal JRecord use **/
	public static final int ftPackedDecimalSmallPostive = 141;
	/** @deprecated Small <= 8 bytes Big Endian Integer  - internal JRecord use **/
	public static final int ftIntBigEndianSmall     = 142;
	/** @deprecated  Small <= 8 bytes Positive Big Endian Integer  - internal JRecord use **/
	public static final int ftIntBigEndianPositive  = 143;
	/** @deprecated  Small <= 7 bytes Big Endian Unsigned int  - internal JRecord use **/
	public static final int ftUIntBigEndianSmall    = 144;
	/** @deprecated  Small <= 8 bytes Little Endian Integer  - internal JRecord use **/
	public static final int ftIntSmall              = 145;
	/** @deprecated  Small <= 8 bytes Positive Little Endian Integer  - internal JRecord use **/
	public static final int ftIntPositiveSmall      = 146;
	/** @deprecated  Small <= 7 bytes Little Endian Unsigned int  - internal JRecord use **/
	public static final int ftUIntSmall             = 147;

	/** @deprecated  Small <= 16 bytesZoned Decimal  - internal JRecord use **/
	public static final int ftZonedEbcdicSmall      = 150;
	/** @deprecated  Small <= 16 bytesZoned Decimal  - internal JRecord use **/
	public static final int ftZonedEbcdicSmallPositive = 151;
	/** @deprecated  Small <= 16 bytesZoned Decimal  - internal JRecord use **/
	public static final int ftZonedAsciiSmall       = 152;
	/** @deprecated  Small <= 16 bytesZoned Decimal  - internal JRecord use **/
	public static final int ftZonedAsciiSmallPositive = 153;

	public static final int NT_TEXT              = 1;
	public static final int NT_DATE              = 11;
	public static final int NT_NUMBER            = 21;

    /**
     * Extracts a field out of a data record
     *
     * @param data record which is to have the field extracted
     * @param position position in the record
     * @param currField Field Details
     *
     * @return the request field (formated)
     */
    public abstract Object getField(final byte[] data,
            						final int position,
            						final IFieldDetail currField);


    /**
     * Sets a field to a new value in the supplied data record
     *
     * @param data record which is to be update
     * @param position position in the record
     * @param field Field Details
     * @param val new value
     *
     * @return updated record
     */
    public abstract byte[] setField(final byte[] data,
            					  final int position,
            					  final IFieldDetail field,
            					  Object val);

    /**
     * Format a value for storing in the record, it
     * has 3 uses
     * <ol compact>
     *  <li>Format the string for storing in a record.
     *  <li>Format for storing as a String in a comma / tab delimited files.
     *  <li>Validate a value
     * </ol>
     *
     * @param field field definition
     * @param val value to be formated
     *
     * @return value value as it is store in the record
      */
    public abstract String formatValueForRecord(IFieldDetail field, String val);


    /**
     * whether it is a binary field
     *
     * @return whether it is binary field
     */
    public abstract boolean isBinary();

    /**
     * Is this a Numeric Type
     *
     * @return whether it is a numeric Type
     */
    public abstract boolean isNumeric();


    /**
     * Get the type of field
     * @return type of field
     */
    public abstract int getFieldType();

    /**
     * Get the character used for the decimal point
     * @return the character that marks the decimal point (normally '.' but can be ','
     * for our German user's
     */
    public abstract char getDecimalChar();
}