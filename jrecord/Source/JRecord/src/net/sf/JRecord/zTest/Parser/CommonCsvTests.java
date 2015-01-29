package net.sf.JRecord.zTest.Parser;

import java.util.List;

import junit.framework.TestCase;
import net.sf.JRecord.CsvParser.ICsvDefinition;
import net.sf.JRecord.CsvParser.ICsvLineParser;

public class CommonCsvTests {

	public static void tstGetFieldList(String id, String[] lines, ICsvLineParser parser, ICsvDefinition csvDefinition, int fieldCount) {
		System.out.println(id);
		for (String s : lines) {
			List<String> flds = parser.getFieldList(s, csvDefinition);
			if (flds.size() != fieldCount) {
				flds = parser.getFieldList(s, csvDefinition);
				TestCase.assertEquals(id + " check counts", fieldCount, flds.size());
			}
			for (int i = 0; i < flds.size(); i++) {
				TestCase.assertEquals(id + " Check: " + s + ", fieldNo=" + i, 
						parser.getField(i, s, csvDefinition), flds.get(i) );
			}
		}
	}
	

	public static void tstSetFieldList(String id, String[] lines, ICsvLineParser parser, ICsvDefinition csvDefinition, int fieldCount) {
		tstSetFieldList(id, lines, lines.length, parser, csvDefinition, fieldCount);
	}
	

	public static void tstSetFieldList(String id, String[] lines, int num, ICsvLineParser parser, ICsvDefinition csvDefinition, int fieldCount) {
		System.out.println(id);
		for (int i = 0; i < num; i++) {
			String s = lines[i];
			List<String> flds = parser.getFieldList(s, csvDefinition);
			TestCase.assertEquals(id + " check counts", fieldCount, flds.size());
			TestCase.assertEquals(id + " check line: ", s, parser.formatFieldList(flds, csvDefinition, null));
		}
	}

}
