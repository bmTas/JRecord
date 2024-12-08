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

import java.nio.file.FileSystems;

/**
 * Constants used in JRecord
 *
 * @author Bruce Martin
 *
 */
public interface Constants extends IFileStructureConstants, ISelectionOperators {

	int NULL_INTEGER	= -121;
	int KEY_INDEX		= -765;
	int FULL_LINE		= -101;

	int NUMBER_OF_COPYBOOK_SOURCES = 16;


    byte BYTE_LF = 10;
    byte BYTE_CR = 13;

	String LINE_SEPERATOR  = System.lineSeparator();

	byte[] CRLF_BYTES      = {BYTE_CR, BYTE_LF};
	byte[] LF_BYTES        = {BYTE_LF};
	byte[] CR_BYTES        = {BYTE_CR};
	byte[] SYSTEM_EOL_BYTES = LINE_SEPERATOR.getBytes();

   	/* record Type constatnts */
	int rtBinaryRecord         = 0;
	int rtRecordLayout         = 1;
	int rtDelimited            = 2;
	int rtDelimitedAndQuote    = 3;
	int RT_XML                 = 6;
	int rtGroupOfRecords       = 9;
	int rtGroupOfBinaryRecords = 10;
	int rtFixedLengthRecords   = 11;
	int rtProtoRecord          = rtRecordLayout;

	int FORMAT_DEFAULT  = 0;


    /* Provided for backward compatibility */
    @Deprecated int DEFAULT_IO       = IO_DEFAULT;
    @Deprecated int TEXT_LINE_IO     = IO_TEXT_LINE;
    @Deprecated int FIXED_LENGTH_IO  = IO_FIXED_LENGTH;
    @Deprecated int BINARY_IO        = IO_BINARY_IBM_4680;
    @Deprecated int VB_LINE_IO       = IO_VB;
    @Deprecated int VB_DUMP_LINE_IO  = IO_VB_DUMP;
    @Deprecated int VB_FUJ_LINE_IO   = IO_VB_FUJITSU;
    @Deprecated int NAME_1ST_LINE_IO = IO_NAME_1ST_LINE;

	String DEFAULT_STRING  =  "default";
	String CRLF_STRING     =  "<crlf>";
	String CR_STRING       =  "<cr>";
	String LF_STRING       =  "<lf>";

	String FILE_SEPERATOR  = FileSystems.getDefault().getSeparator();


	String RECORD_NAME     = "Record";
	String SUB_RECORD_NAME = "SR";
	String TXT_EXTENSION   = ".Txt" ;
	String XML_EXTENSION   = ".Xml" ;

	String RE_XML_RECORD      = "RECORD";
	String RE_XML_RECORDS     = "RECORDS";
	String RE_XML_FIELD       = "FIELD";
	String RE_XML_FIELDS      = "FIELDS";
	String RE_XML_TST_FIELD   = "TSTFIELD";
	String RE_XML_TST_FIELDS  = "TSTFIELDS";
	String RE_XML_AND_FIELDS  = "AND";
	String RE_XML_OR_FIELDS   = "OR";
	String RE_XML_TRUE        = "TRUE";

	String RE_XML_PARSE       = "PARSE";
	String RE_XML_OVERRIDES   = "OVERRIDES";
	String RE_XML_OVERRIDE    = "OVERRIDE";
	String RE_XML_FILE_NAME = "FILENAME";


	String RE_XML_PARSE_LANGUAGE  = "LANGUAGE";

	String RE_XML_COPYBOOK    = "COPYBOOK";
	String RE_XML_COBOL_ITEMS = "ITEMS";
	String RE_XML_JRECORD_NAMING = "JRecNaming";
	String RE_XML_KEEP_FILLER = "KeepFiller";
	String RE_XML_DROP_COPYBOOK_FROM_FIELD = "DropCopybook";
	String RE_XML_COBOL_DIALECT = "DIALECT";
	String RE_XML_COPYBOOK_PREF = "CopybookPref";
	String RE_XML_DELIMITER   = "DELIMITER";
	String RE_XML_DESCRIPTION = "DESCRIPTION";
	String RE_XML_FONTNAME    = "FONTNAME";
	String RE_XML_FILESTRUCTURE = "FILESTRUCTURE";
	String RE_XML_LISTCHAR    = "LIST";
	String RE_XML_PARENT      = "PARENT";
	String RE_XML_QUOTE       = "QUOTE";
	String RE_XML_RECORDNAME  = "RECORDNAME";
	String RE_XML_RECORDLENTH = "RECORDLENGTH";
	String RE_XML_RECORDTYPE  = "RECORDTYPE";
	String RE_XML_EMBEDDED_CR = "EMBEDDEDCR";
	String RE_XML_INIT_SPACES = "INITSPACES";
	String RE_XML_RECORDSEP   = "RecSep";
	String RE_XML_STYLE       = "STYLE";
	String RE_XML_STYLE_PARAM = "LINEFORMATPARAM";
	String RE_XML_SYSTEMNAME  = "SYSTEMNAME";
	String RE_XML_DEFAULTREC  = "DEFAULTRECORD";
	String RE_XML_TESTFIELD   = "TESTFIELD";
	String RE_XML_TESTVALUE   = "TESTVALUE";
	String RE_XML_LINE_NO_FIELD_NAME   = "LINE_NO_FIELD_NAMES";

	String RE_XML_NAME        = "NAME";
	String RE_XML_VALUE       = "VALUE";
	String RE_XML_OPERATOR    = "OPERATOR";
	String RE_XML_DEFAULT     = "DEFAULT";
	String RE_XML_COBOLNAME   = "COBOLNAME";
	String RE_XML_GROUP_NAMES = "GROUPNAMES";
	String RE_XML_PARAMETER   = "PARAMETER";
	String RE_XML_CELLFORMAT  = "CELLFORMAT";
	String RE_XML_POS         = "POSITION";
	String RE_XML_LENGTH      = "LENGTH";
	String RE_XML_TYPE        = "TYPE";
	String RE_XML_DECIMAL     = "DECIMAL";


}
