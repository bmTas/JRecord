package net.sf.JRecord.CsvParser;

public interface ICsvParserIds {

	public static final int BASIC_CSV_PARSER_NEW_NUM = 14;
	public static final int BASIC_PARSER_QUOTE_BY_TYPE = 15;

	public static final int BASIC_CSV_PARSER = BASIC_CSV_PARSER_NEW_NUM;
	public static final int EXTENDED_BASIC_CSV_PARSER = 0;
//	public static final int BASIC_CSV_PARSER = 12;
	public static final int STANDARD_CSV_PARSER = 1;
	public static final int DB_CSV_PARSER = 2;
	public static final int BASIC_QUOTED_COL_NAME_CSV_PARSER = 3;
	public static final int STANDARD_QUOTED_COL_NAME_CSV_PARSER = 4;
	public static final int DB_QUOTED_COL_NAME_CSV_PARSER = 5;
	public static final int BASIC_ENSURE_CORRECT_NO_FIELDS = 6;
	public static final int BASIC_ENSURE_CORRECT_NO_FIELDS_P1 = 7;
	public static final int BASIC_EMBEDDED_CR = 8;
	public static final int STANDARD_EMBEDDED_CR = 9;
	public static final int BASIC_EMBEDDED_CR_NAMES_IN_QUOTES = 10;
	public static final int STANDARD_EMBEDDED_CR_NAMES_INQUOTE = 11;
	public static final int STANDARD_EMBEDDED_CR_NAMES_TXT_INQUOTE = 12;
	public static final int BASIC_TXT_INQUOTE = 13;

	public static final int REGULAR_EXPRESSION_PARSER = 40;

	public static final int NUMBER_OF_PARSERS = 50;
}