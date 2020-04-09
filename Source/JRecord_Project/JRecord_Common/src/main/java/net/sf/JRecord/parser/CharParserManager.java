package net.sf.JRecord.parser;

import net.sf.JRecord.CsvParser.CsvParserManagerChar;
import net.sf.JRecord.CsvParser.ICsvCharLineParser;
import net.sf.JRecord.CsvParser.ICsvParserIds;
import net.sf.JRecord.parser.regExp.RegularExpressionLineParser;

public class CharParserManager {

	public static ICsvCharLineParser getParser(int parserType, String param) {
		if (parserType == ICsvParserIds.REGULAR_EXPRESSION_PARSER) {
			return param == null ? null : new RegularExpressionLineParser(param);
		}
		return CsvParserManagerChar.getInstance().get(parserType);
	}
}
