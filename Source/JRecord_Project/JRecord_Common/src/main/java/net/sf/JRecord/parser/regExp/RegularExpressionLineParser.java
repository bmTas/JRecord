package net.sf.JRecord.parser.regExp;

import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

import net.sf.JRecord.CsvParser.ICsvCharLineParser;
import net.sf.JRecord.CsvParser.ICsvDefinition;

public class RegularExpressionLineParser implements ICsvCharLineParser {

	private final Pattern pattern;
	
	
	public RegularExpressionLineParser(String regExpStr) {
		 pattern = Pattern.compile(regExpStr);
	}
	
	@Override
	public boolean isQuoteInColumnNames() {
		return false;
	}

	@Override
	public String getField(int fieldNumber, String line, ICsvDefinition csvDefinition) {
		List<String> fields =  getFieldList(line, csvDefinition);
		return fields == null || fields.size() <= fieldNumber ? null : fields.get(fieldNumber);
	}

	@Override
	public String setField(int fieldNumber, int fieldType, String line, ICsvDefinition csvDefinition, String newValue) {
		return null;
	}

	@Override
	public List<String> getFieldList(String line, ICsvDefinition csvDefinition) {
		String[] fields = pattern.split(line);
		return Arrays.asList(fields);
	}

	@Override
	public List<String> getColumnNames(String line, ICsvDefinition csvDefinition) {
		return getFieldList(line, csvDefinition);
	}

	@Override
	public String getColumnNameLine(List<String> names, ICsvDefinition csvDefinition) {
		return null;
	}

	@Override
	public int getFileStructure(ICsvDefinition csvDefinition, boolean namesOnFirstLine, boolean binary) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public String formatFieldList(List<? extends Object> fields, ICsvDefinition lineDef, int[] fieldTypes) {
		return null;
	}

	@Override
	public boolean isUpdatable() {
		return false;
	}

}
