/*
 * This class maanges the various line parsers
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

import net.sf.JRecord.Common.BasicNamedManager;

/**
 * Class to manage the various CSV Line parsers
 * <b>Warning</b> - This class must be kept in sync with <b>ParserManagerByte</b> class
 *
 * @author Bruce Martin
 */
public class CsvParserManagerChar extends BasicNamedManager<ICsvCharLineParser> implements ICsvParserIds {
	private static final boolean USE_NEW_CSV_PARSERS = true;


	private static CsvParserManagerChar instance = null;

//	private Parser[] list = new Parser[NUMBER_OF_PARSERS];
//	private String[] names = new String[NUMBER_OF_PARSERS];


	/**
	 * Manage the various line parsers
	 *
	 */
	public CsvParserManagerChar() {
		this(true);
	}

	private CsvParserManagerChar(boolean newformat) {
		super("Csv_Parsers", NUMBER_OF_PARSERS, NUMBER_OF_PARSERS, new ICsvCharLineParser[NUMBER_OF_PARSERS]);
//		

//		if (newformat) {
			defineNewParsers();
//		} else {
//			defineOldParsers();
//		}
	}
	
//	private void defineOldParsers() {
//		
//		register(BASIC_CSV_PARSER, "Basic Parser", BasicCsvLineParser.getInstance());
//		register(STANDARD_CSV_PARSER, "Parser - Matching Quotes", new StandardCsvLineParser());
//		register(DB_CSV_PARSER, "Parser - Quotes based on field Type", new StandardCsvLineParser(true));
//		register(BASIC_QUOTED_COL_NAME_CSV_PARSER, "Basic Parser Column names in quotes", new BasicCsvLineParser(true));
//		register(STANDARD_QUOTED_COL_NAME_CSV_PARSER, "Parser - Matching Quotes Column names in quotes",
//				new StandardCsvLineParser(false, true));
//		register(DB_QUOTED_COL_NAME_CSV_PARSER, "Parser - Quotes based on field Type, Column names in quotes",
//				new StandardCsvLineParser(true, true));
//		register(BASIC_ENSURE_CORRECT_NO_FIELDS,    "Basic - Delimiter all fields",     new BasicCsvLineParser(false, ICsvDefinition.SEP_FOR_EVERY_FIELD));
//		register(BASIC_ENSURE_CORRECT_NO_FIELDS_P1, "Basic - Delimiter all fields + 1", new BasicCsvLineParser(false, ICsvDefinition.SEP_FOR_EVERY_FIELD_PLUS_END));
//		register(BASIC_EMBEDDED_CR,    "Basic - Embedded Cr",  new BasicCsvLineParser(false, ICsvDefinition.NORMAL_SPLIT, true, false));
//		register(STANDARD_EMBEDDED_CR, "Standard - Embedded Cr",  new StandardCsvLineParser(false, true, true));
//
//		register(BASIC_EMBEDDED_CR_NAMES_IN_QUOTES,    "Basic - Embedded Cr Column names in quotes",  new BasicCsvLineParser(true, ICsvDefinition.NORMAL_SPLIT, true, false));
//		register(STANDARD_EMBEDDED_CR_NAMES_INQUOTE, "Standard - Embedded Cr Column names in quotes",  new StandardCsvLineParser(false, true, true));
//		register(STANDARD_EMBEDDED_CR_NAMES_TXT_INQUOTE, "Std - Embedded Cr Col names/Txt Fields in quotes",  new StandardCsvLineParser(true, true, true));
//		register(BASIC_CSV_PARSER_NEW_NUM, "Extended Basic Parser", BasicCsvLineParserExtended.getInstance());
//	}

	private void defineNewParsers() {
		
		register(EXTENDED_BASIC_CSV_PARSER, "Extended Basic Parser", BasicCsvLineParserExtended.getInstance());
		register(STANDARD_CSV_PARSER, "Parser - Matching Quotes", new StandardCsvLineParser());
		register(DB_CSV_PARSER, "Parser - Quotes based on field Type", new BasicCsvLineParserExtended(false, ICsvDefinition.NORMAL_SPLIT, false, true));
		register(BASIC_QUOTED_COL_NAME_CSV_PARSER, "X-Basic Parser Column names in quotes", new BasicCsvLineParserExtended(true));
		register(STANDARD_QUOTED_COL_NAME_CSV_PARSER, "Parser - Matching Quotes Column names in quotes",
				new StandardCsvLineParser(false, true));
		register(DB_QUOTED_COL_NAME_CSV_PARSER, "Parser - Quotes based on field Type, Column names in quotes",
				new StandardCsvLineParser(true, true));
		register(BASIC_ENSURE_CORRECT_NO_FIELDS,    "X-Basic - Delimiter all fields",     new BasicCsvLineParserExtended(false, ICsvDefinition.SEP_FOR_EVERY_FIELD));
		register(BASIC_ENSURE_CORRECT_NO_FIELDS_P1, "X-Basic - Delimiter all fields + 1", new BasicCsvLineParserExtended(false, ICsvDefinition.SEP_FOR_EVERY_FIELD_PLUS_END));
		register(BASIC_EMBEDDED_CR,    "X-Basic - Embedded Cr",  new BasicCsvLineParserExtended(false, ICsvDefinition.NORMAL_SPLIT, true, false));
		register(STANDARD_EMBEDDED_CR, "Standard - Embedded Cr",  new StandardCsvLineParser(false, true, true));

		register(BASIC_EMBEDDED_CR_NAMES_IN_QUOTES,    "X-Basic - Embedded Cr Column names in quotes",  new BasicCsvLineParserExtended(true, ICsvDefinition.NORMAL_SPLIT, true, false));
		register(STANDARD_EMBEDDED_CR_NAMES_INQUOTE, "Standard - Embedded Cr Column names in quotes",  new StandardCsvLineParser(false, true, true));
		register(STANDARD_EMBEDDED_CR_NAMES_TXT_INQUOTE, "Std - Embedded Cr Col names/Txt Fields in quotes",  new StandardCsvLineParser(true, true, true));
		register(BASIC_TXT_INQUOTE,    "X-Basic - Text fields in quotes",     new BasicCsvLineParserExtended(false, ICsvDefinition.NORMAL_SPLIT, false, true));
		register(BASIC_CSV_PARSER_NEW_NUM, "Basic Parser", BasicCsvLineParser.getInstance());
		register(BASIC_PARSER_QUOTE_BY_TYPE, "Basic Parser - Quotes based on field Type", new StandardCsvLineParser(true));
	}

	/**
	 * @see net.sf.JRecord.Common.BasicManager#get(int)
	 */
	@Override
	public ICsvCharLineParser get(int id) {
		if (id < 0 || id >= super.getNumberOfEntries()) {
			return super.get(0);
		}
		return super.get(id);
	}

	public static final boolean isUseNewCsvParsers() {
		return USE_NEW_CSV_PARSERS;
	}

//	public static final void setUseNewCsvParsers(boolean useNewCsvParsers) {
//		ParserManager.useNewCsvParsers = useNewCsvParsers;
//		
//		BASIC_CSV_PARSER = 0;
//		EXTENDED_BASIC_CSV_PARSER = BASIC_CSV_PARSER_NEW_NUM;
//		if (useNewCsvParsers) {
//			BASIC_CSV_PARSER = BASIC_CSV_PARSER_NEW_NUM;
//			EXTENDED_BASIC_CSV_PARSER = 0;
//		}
//	}


	/**
	 * get a ParserManager
	 * @return the instance
	 */
	public static CsvParserManagerChar getInstance() {
		if (instance == null) {
			instance = new CsvParserManagerChar(USE_NEW_CSV_PARSERS);
		}
		return instance;
	}

}
