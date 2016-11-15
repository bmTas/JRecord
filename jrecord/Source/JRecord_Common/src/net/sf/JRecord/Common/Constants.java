/*
 * @Author Bruce Martin
 * Created on 8/02/2007
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
      
package net.sf.JRecord.Common;

/**
 * Constants used in JRecord
 *
 * @author Bruce Martin
 *
 */
public interface Constants {

	public  static final int NULL_INTEGER	= -121;
	public static final int KEY_INDEX		= -765;
	public static final int FULL_LINE		= -101;

	public static final  int NUMBER_OF_COPYBOOK_SOURCES = 16;


    public static final byte BYTE_LF = 10;
    public static final byte BYTE_CR = 13;

	public static final String LINE_SEPERATOR  = System.getProperty("line.separator");

	public static final byte[] CRLF_BYTES      = {BYTE_CR, BYTE_LF};
	public static final byte[] LF_BYTES        = {BYTE_LF};
	public static final byte[] CR_BYTES        = {BYTE_CR};
	public static final byte[] SYSTEM_EOL_BYTES = LINE_SEPERATOR.getBytes();

   	/* record Type constatnts */
	public static final int rtBinaryRecord         = 0;
	public static final int rtRecordLayout         = 1;
	public static final int rtDelimited            = 2;
	public static final int rtDelimitedAndQuote    = 3;
	public static final int RT_XML                 = 6;
	public static final int rtGroupOfRecords       = 9;
	public static final int rtGroupOfBinaryRecords = 10;
	public static final int rtFixedLengthRecords   = 11;
	public static final int rtProtoRecord         = rtRecordLayout;

	public static final int FORMAT_DEFAULT  = 0;


	public static final int IO_DEFAULT      = 0;
	public static final int IO_STANDARD_TEXT_FILE    = 1;
		/** alias for IO_STANDARD_TEXT_FILE*/
    public static final int IO_TEXT_LINE    = IO_STANDARD_TEXT_FILE;
    public static final int IO_FIXED_LENGTH_RECORDS = 2;
    	/** Alias for IO_FIXED_LENGTH_RECORDS */
    public static final int IO_FIXED_LENGTH = IO_FIXED_LENGTH_RECORDS;
    public static final int IO_BINARY_IBM_4680 = 3;
    public static final int IO_VB           = 4;
    public static final int IO_VB_DUMP      = 5;
    public static final int IO_VB_FUJITSU   = 7;
    public static final int IO_VB_GNU_COBOL = 8;
    public static final int IO_VB_OPEN_COBOL= IO_VB_GNU_COBOL;
    public static final int IO_BIN_TEXT     = 9;
    public static final int IO_FIXED_LENGTH_CHAR = 10;
    
    /**
     * IO_CONTINOUS_NO_LINE_MARKER is for files where there are no 
     * <i>End-of-Line</i> markers. Records are based on Record-Lengths.
     * <p>If there are more than 1 record type, you <b>MUST</b> set 
     * record-Selection criteria <b>!!!</b> 
     * 
     * <p>Also <b>try and</b> <font color="red"><b>Avoid</b></font> using 
     * <b>IO_CONTINOUS_NO_LINE_MARKER</b> if you can, it is always going to be error prone. 
     * I have provided this option just in case there is no other choice.</p>
     */
    public static final int IO_CONTINOUS_NO_LINE_MARKER  = 11;
    
    /**
     * @deprecated This option is for reading Mainframe VBS files,
     * It was written based on information in the Manual and has had
     * very limited testing against real VBS files.
     * <p>I <b>strongly</b> suggest you <b>copy</b> the file from <b>VBS</b> to <b>VB</b>/<b>FB</b> on the mainframe
     * and use <b>IO_VB</b> / <b>IO_FIXED_LENGTH</b> instead of the IO_VBS option. Also you can only 
     * read VBS files, there is now Write option. 
     */
    public static final int IO_VBS  = 12;

    public static final int IO_UNKOWN_FORMAT = 21 /* RecordEditor Format */;
    public static final int IO_WIZARD        = 22 /* RecordEditor Format */;

    public static final int IO_MICROFOCUS = 31;

    public static final int IO_CSV              = 44;
    public static final int IO_BIN_CSV          = 45;
    public static final int IO_UNICODE_CSV      = 46;

    public static final int IO_CSV_NAME_1ST_LINE              = 47;
    public static final int IO_BIN_CSV_NAME_1ST_LINE          = 48;
    public static final int IO_UNICODE_CSV_NAME_1ST_LINE      = 49;

    public static final int IO_NAME_1ST_LINE    = 51;
    public static final int IO_GENERIC_CSV      = 52 /* RecordEditor Format */;
//    public static final int IO_GENERIC_CSV_UNICODE = 53 /* RecordEditor Format */;
    /**
     * IO_BIN_NAME_1ST_LINE is for internal use
     */
    public static final int IO_BIN_NAME_1ST_LINE     = 54;
    public static final int IO_UNICODE_NAME_1ST_LINE = 55;



    public static final int IO_XML_USE_LAYOUT   = 61;
    public static final int IO_XML_BUILD_LAYOUT = 62;

//    public static final int IO_EVALUATE_DEFAULT_BINARY    = 80;
//    public static final int IO_EVALUATE_DEFAULT_FIXED     = 81;
//    public static final int IO_EVALUATE_DEFAULT_BIN_TEXT  = 82;
//    public static final int IO_EVALUATE_DEFAULT_TEXT      = 83;

    public static final int IO_STANDARD_UNICODE_TEXT_FILE  = 90;
    	/** Alias for IO_STANDARD_UNICODE_TEXT_FILE */
    public static final int IO_UNICODE_TEXT = IO_STANDARD_UNICODE_TEXT_FILE;

    public static final int IO_PROTO_DELIMITED      = 71;
    public static final int IO_PROTO_SINGLE_MESSAGE = 72;
    public static final int IO_PROTO_SD_DELIMITED   = 73;
    public static final int IO_PROTO_SD_SINGLE_MESSAGE = 74;
    public static final int IO_PROTO_TEXT = 75;
    public static final int IO_PROTO_JSON = 76;

    public static final int IO_THRIFT_FILE  = 81;
    public static final int IO_AVRO_FILE    = 91;

    public static final int IO_GETTEXT_PO   = 101;
    public static final int IO_TIP          = 102;


    /* Provided for backward compatibility */
    @Deprecated public static final int DEFAULT_IO       = IO_DEFAULT;
    @Deprecated public static final int TEXT_LINE_IO     = IO_TEXT_LINE;
    @Deprecated public static final int FIXED_LENGTH_IO  = IO_FIXED_LENGTH;
    @Deprecated public static final int BINARY_IO        = IO_BINARY_IBM_4680;
    @Deprecated public static final int VB_LINE_IO       = IO_VB;
    @Deprecated public static final int VB_DUMP_LINE_IO  = IO_VB_DUMP;
    @Deprecated public static final int VB_FUJ_LINE_IO   = IO_VB_FUJITSU;
    @Deprecated public static final int NAME_1ST_LINE_IO = IO_NAME_1ST_LINE;

	public static final String DEFAULT_STRING  =  "default";
	public static final String CRLF_STRING     =  "<crlf>";
	public static final String CR_STRING       =  "<cr>";
	public static final String LF_STRING       =  "<lf>";

	public static final String FILE_SEPERATOR  =  System.getProperty("file.separator");


	public static final String RECORD_NAME     = "Record";
	public static final String SUB_RECORD_NAME = "SR";
	public static final String TXT_EXTENSION   = ".Txt" ;
	public static final String XML_EXTENSION   = ".Xml" ;

	public static final String RE_XML_RECORD      = "RECORD";
	public static final String RE_XML_RECORDS     = "RECORDS";
	public static final String RE_XML_FIELD       = "FIELD";
	public static final String RE_XML_FIELDS      = "FIELDS";
	public static final String RE_XML_TST_FIELD   = "TSTFIELD";
	public static final String RE_XML_TST_FIELDS  = "TSTFIELDS";
	public static final String RE_XML_AND_FIELDS  = "AND";
	public static final String RE_XML_OR_FIELDS   = "OR";

	public static final String RE_XML_COPYBOOK    = "COPYBOOK";
	public static final String RE_XML_DELIMITER   = "DELIMITER";
	public static final String RE_XML_DESCRIPTION = "DESCRIPTION";
	public static final String RE_XML_FONTNAME    = "FONTNAME";
	public static final String RE_XML_FILESTRUCTURE = "FILESTRUCTURE";
	public static final String RE_XML_LISTCHAR    = "LIST";
	public static final String RE_XML_PARENT      = "PARENT";
	public static final String RE_XML_QUOTE       = "QUOTE";
	public static final String RE_XML_RECORDNAME  = "RECORDNAME";
	public static final String RE_XML_RECORDLENTH = "RECORDLENGTH";
	public static final String RE_XML_RECORDTYPE  = "RECORDTYPE";
	public static final String RE_XML_EMBEDDED_CR = "EMBEDDEDCR";
	public static final String RE_XML_INIT_SPACES = "INITSPACES";
	public static final String RE_XML_RECORDSEP   = "RecSep";
	public static final String RE_XML_STYLE       = "STYLE";
	public static final String RE_XML_SYSTEMNAME  = "SYSTEMNAME";
	public static final String RE_XML_DEFAULTREC  = "DEFAULTRECORD";
	public static final String RE_XML_TESTFIELD   = "TESTFIELD";
	public static final String RE_XML_TESTVALUE   = "TESTVALUE";
	public static final String RE_XML_LINE_NO_FIELD_NAME   = "LINE_NO_FIELD_NAMES";

	public static final String RE_XML_NAME        = "NAME";
	public static final String RE_XML_VALUE       = "VALUE";
	public static final String RE_XML_OPERATOR    = "OPERATOR";
	public static final String RE_XML_DEFAULT     = "DEFAULT";
	public static final String RE_XML_COBOLNAME   = "COBOLNAME";
	public static final String RE_XML_GROUP_NAMES = "GROUPNAMES";
	public static final String RE_XML_PARAMETER   = "PARAMETER";
	public static final String RE_XML_CELLFORMAT  = "CELLFORMAT";
	public static final String RE_XML_POS         = "POSITION";
	public static final String RE_XML_LENGTH      = "LENGTH";
	//public static final String RE_XML_SUBKEY      = "SUBKEY";
	public static final String RE_XML_TYPE        = "TYPE";
	public static final String RE_XML_DECIMAL     = "DECIMAL";


	public static final String STARTS_WITH  = "Starts With";
	public static final String DOES_NOT_CONTAIN  = "Doesn't Contain";
	public static final String CONTAINS  = "Contains";
	public static final String EMPTY   = "Is Empty";
	public static final String NUM_EQ  = "= (Numeric)";
	public static final String NUM_GT  = "> (Numeric)";
	public static final String NUM_GE  = ">= (Numeric)";
	public static final String NUM_LT  = "< (Numeric)";
	public static final String NUM_LE  = "<= (Numeric)";
	public static final String TEXT_EQ = "= (Text)";
	public static final String TEXT_GT = "> (Text)";
	public static final String TEXT_GE = ">= (Text)";
	public static final String TEXT_LT = "< (Text)";
	public static final String TEXT_LE = "<= (Text)";
	public static final String NUM_NE  = "<> (Numeric)";
	public static final String TEXT_NE = "<> (Text)";

	public static final String[] VALID_COMPARISON_OPERATORS = {
		"=", "eq", "!=", "<>", "ne", ">", "gt", ">=", "ge", "<", "lt", "<=", "le",
		STARTS_WITH,  DOES_NOT_CONTAIN,  CONTAINS,
		NUM_EQ,   NUM_GT,   NUM_GE,   NUM_LT,   NUM_LE,
		TEXT_EQ,  TEXT_GT,  TEXT_GE,  TEXT_LT,  TEXT_LE,
		NUM_NE,   TEXT_NE, EMPTY
	};

}
