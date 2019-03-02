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
 *
 * @author Bruce Martin
 */
public class CsvParserManagerByte extends BasicNamedManager<ICsvByteLineParser> implements ICsvParserIds {

	private static CsvParserManagerByte instance = null;

//	private Parser[] list = new Parser[NUMBER_OF_PARSERS];
//	private String[] names = new String[NUMBER_OF_PARSERS];

	private ICsvByteLineParser[] normalParsers = new ICsvByteLineParser[NUMBER_OF_PARSERS];

	/**
	 * Manage the various line parsers
	 *
	 */
	public CsvParserManagerByte() {
		super("Byte Csv_Parsers", NUMBER_OF_PARSERS, NUMBER_OF_PARSERS, new ICsvByteLineParser[NUMBER_OF_PARSERS]);
		
		register(EXTENDED_BASIC_CSV_PARSER, "Extended Basic Parser", BasicCsvByteLineParserExtended.getInstance());
		register(STANDARD_CSV_PARSER, "Parser - Matching Quotes", BasicCsvByteLineParserExtended.getInstance());
				//new StandardCsvLineParser());
		register(DB_CSV_PARSER, "Parser - Quotes based on field Type", new BasicCsvByteLineParserExtended(false, ICsvDefinition.NORMAL_SPLIT, false, true));
		register(BASIC_QUOTED_COL_NAME_CSV_PARSER, "X-Basic Parser Column names in quotes", new BasicCsvByteLineParserExtended(true));
		register(STANDARD_QUOTED_COL_NAME_CSV_PARSER, "Parser - Matching Quotes Column names in quotes",
				new BasicCsvByteLineParserExtended(true));
				//new StandardCsvLineParser(false, true));
		register(DB_QUOTED_COL_NAME_CSV_PARSER, "Parser - Quotes based on field Type, Column names in quotes",
				new BasicCsvByteLineParserExtended(true, ICsvDefinition.NORMAL_SPLIT, false, true));
				//new StandardCsvLineParser(true, true));
		register(BASIC_ENSURE_CORRECT_NO_FIELDS,    "X-Basic - Delimiter all fields",     new BasicCsvByteLineParserExtended(false, ICsvDefinition.SEP_FOR_EVERY_FIELD));
		register(BASIC_ENSURE_CORRECT_NO_FIELDS_P1, "X-Basic - Delimiter all fields + 1", new BasicCsvByteLineParserExtended(false, ICsvDefinition.SEP_FOR_EVERY_FIELD_PLUS_END));
		register(BASIC_EMBEDDED_CR,    "X-Basic - Embedded Cr",  new BasicCsvByteLineParserExtended(false, ICsvDefinition.NORMAL_SPLIT, true, false));
		register(STANDARD_EMBEDDED_CR, "Standard - Embedded Cr", 
				new BasicCsvByteLineParserExtended(false, ICsvDefinition.NORMAL_SPLIT, true, false));
				//new StandardCsvLineParser(false, true, true));

		register(BASIC_EMBEDDED_CR_NAMES_IN_QUOTES,    "X-Basic - Embedded Cr Column names in quotes",  new BasicCsvByteLineParserExtended(true, ICsvDefinition.NORMAL_SPLIT, true, false));
		register(STANDARD_EMBEDDED_CR_NAMES_INQUOTE, "Standard - Embedded Cr Column names in quotes",  
				new BasicCsvByteLineParserExtended(true, ICsvDefinition.NORMAL_SPLIT, true, false));
				//new StandardCsvLineParser(false, true, true));
		register(STANDARD_EMBEDDED_CR_NAMES_TXT_INQUOTE, "Std - Embedded Cr Col names/Txt Fields in quotes", 
				new BasicCsvByteLineParserExtended(true, ICsvDefinition.NORMAL_SPLIT, true, true));
				//new StandardCsvLineParser(true, true, true));
		register(BASIC_TXT_INQUOTE,    "X-Basic - Text fields in quotes",     new BasicCsvByteLineParserExtended(false, ICsvDefinition.NORMAL_SPLIT, false, true));
		register(BASIC_CSV_PARSER_NEW_NUM, "Basic Parser", 
				BasicCsvByteLineParserExtended.getInstance());
				//BasicCsvLineParser.getInstance());
		register(BASIC_PARSER_QUOTE_BY_TYPE, "Basic Parser - Quotes based on field Type", 
				new BasicCsvByteLineParserExtended(false, ICsvDefinition.NORMAL_SPLIT, false, true));
				//new StandardCsvLineParser(true));
	}

	/**
	 * get a ParserManager
	 * @return the instance
	 */
	public static CsvParserManagerByte getInstance() {
		if (instance == null) {
			instance = new CsvParserManagerByte();
		}
		return instance;
	}

	/**
	 * @see net.sf.JRecord.Common.BasicManager#get(int)
	 */
	@Override
	public ICsvByteLineParser get(int id) {
		if (id < 0 || id >= super.getNumberOfEntries()) {
			return super.get(0);
		}
		return super.get(id);
	}
	
	public ICsvByteLineParser get(int id, boolean binCsv) {
		if (id < 0 || id >= super.getNumberOfEntries()) {
			id = 0;
		}
		if (! binCsv) {
			if (normalParsers[id] == null) {
				Object p = CsvParserManagerChar.getInstance().get(id);
				if (p instanceof ICsvByteLineParser) {
					normalParsers[id] = (ICsvByteLineParser) p;
				}
			}
			
			ICsvByteLineParser r = normalParsers[id];
			if (r != null) {
				return r;
			}
		}
		return super.get(id);
	}

}

