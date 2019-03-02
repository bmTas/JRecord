package net.sf.JRecord.zTest.csvParser;

import java.util.Arrays;
import java.util.List;

import junit.framework.TestCase;
import net.sf.JRecord.CsvParser.BasicCsvByteLineParserExtended;
import net.sf.JRecord.CsvParser.BasicCsvLineParserExtended;
import net.sf.JRecord.CsvParser.CsvDefinition;
import net.sf.JRecord.CsvParser.ICsvByteLineParser;
import net.sf.JRecord.Types.Type;

public class TstCsvParsers extends TestCase {
	
	String[] vals = { "\"123,456\",22,33", "11,\"123,456\",33", "11,22,\"123,456\"",};
	String[][] expected = {
			{"123,456", "22", "33", },
			{"11", "123,456", "33", },
			{"11", "22", "123,456", },
	};
	
	public void testByteCsvParser1() {
		BasicCsvByteLineParserExtended parser = BasicCsvByteLineParserExtended.getInstance();
		CsvDefinition csvDef = new CsvDefinition(",", "\"");
		
		for (int i = 0; i < vals.length; i++) {
			check(expected[i], parser.split(vals[i].getBytes(), csvDef, 0));
		}
	}

	public void testByteCsvParser2() {
		BasicCsvByteLineParserExtended parser = BasicCsvByteLineParserExtended.getInstance();
		CsvDefinition csvDef = new CsvDefinition(",", "\"");
		
		for (int i = 0; i < expected.length; i++) {
			assertEquals(vals[i], new String(parser.formatFieldListByte(Arrays.asList(expected[i]), csvDef, null)));
		}
	}

	
	public void testByteCsvParser3() {
		BasicCsvByteLineParserExtended parser = BasicCsvByteLineParserExtended.getInstance();
		tstCsvParser3(parser);
		tstByteCsvParser4(parser);
	}
	
	public void testByteCsvParser5() {
		tstCsvParser5(BasicCsvByteLineParserExtended.getInstance());
	}


	private void tstCsvParser3(ICsvByteLineParser parser) {
		CsvDefinition csvDef = new CsvDefinition(",", "\"");
		
		for (int i = 0; i < vals.length; i++) {
			List<String> fieldList = parser.getFieldList(vals[i].getBytes(), csvDef);
			check(expected[i], fieldList.toArray(new String[fieldList.size()]));
		}
	}
	


	private void tstByteCsvParser4(ICsvByteLineParser parser) {
		CsvDefinition csvDef = new CsvDefinition(",", "\"");
		
		for (int i = 0; i < expected.length; i++) {
			byte[] line = vals[i].getBytes();
			for (int colIdx = 0; colIdx < expected[i].length; colIdx++) {
				assertEquals(expected[i][colIdx], parser.getField(colIdx, line, csvDef));
			}
			assertEquals(vals[i], new String(parser.formatFieldListByte(Arrays.asList(expected[i]), csvDef, null)));
		}
	}


	private void tstCsvParser5(ICsvByteLineParser parser) {
		CsvDefinition csvDef = new CsvDefinition(",", "\"");

		for (int i = 0; i < expected.length; i++) {
			byte[] s = {}, t = {};
			for (int colIdx = 0; colIdx < expected[i].length; colIdx++) {
				s = parser.setFieldByteLine(colIdx, Type.ftChar, s, csvDef, expected[i][colIdx]);
				int fieldNumber = expected[i].length - colIdx - 1;
				t = parser.setFieldByteLine(fieldNumber, Type.ftChar, t, csvDef, expected[i][fieldNumber]);
			}
			assertEquals(vals[i], new String(s));
			assertEquals(vals[i], new String(t));
		}
	}

	
	public void testStringCsvParser() {
		BasicCsvLineParserExtended parser = BasicCsvLineParserExtended.getInstance();
		CsvDefinition csvDef = new CsvDefinition(",", "\"");
		
		for (int i = 0; i < vals.length; i++) {
			check(expected[i], parser.split(vals[i], csvDef, 0));
		}
	}
	
	
	public void testStringCsvParser2() {
		BasicCsvLineParserExtended parser = BasicCsvLineParserExtended.getInstance();
		CsvDefinition csvDef = new CsvDefinition(",", "\"");
		
		for (int i = 0; i < vals.length; i++) {
			assertEquals(vals[i], parser.formatFieldList(Arrays.asList(expected[i]), csvDef, null));
		}
	}

	public void testStrinCsvParser3() {
		BasicCsvLineParserExtended parser = BasicCsvLineParserExtended.getInstance();
		tstCsvParser3(parser);
		tstByteCsvParser4(parser);
	}

	public void testStrinCsvParser3a() {
		BasicCsvLineParserExtended parser = BasicCsvLineParserExtended.getInstance();
		CsvDefinition csvDef = new CsvDefinition(",", "\"");
		
		for (int i = 0; i < vals.length; i++) {
			List<String> fieldList = parser.getFieldList(vals[i], csvDef);
			check(expected[i], fieldList.toArray(new String[fieldList.size()]));
		}
		
		for (int i = 0; i < expected.length; i++) {
			String line = vals[i];
			for (int colIdx = 0; colIdx < expected[i].length; colIdx++) {
				assertEquals(expected[i][colIdx], parser.getField(colIdx, line, csvDef));
			}
			assertEquals(vals[i], parser.formatFieldList(Arrays.asList(expected[i]), csvDef, null));
		}
	}
	
	public void testStringCsvParser5() {
		tstCsvParser5(BasicCsvLineParserExtended.getInstance());
	}
	public void testStringCsvParser5a() {
		BasicCsvLineParserExtended parser = BasicCsvLineParserExtended.getInstance();
		CsvDefinition csvDef = new CsvDefinition(",", "\"");

		for (int i = 0; i < expected.length; i++) {
			String s = "", t = "";
			for (int colIdx = 0; colIdx < expected[i].length; colIdx++) {
				s = parser.setField(colIdx, Type.ftChar, s, csvDef, expected[i][colIdx]);
				int fieldNumber = expected[i].length - colIdx - 1;
				t = parser.setField(fieldNumber, Type.ftChar, t, csvDef, expected[i][fieldNumber]);
			}
			assertEquals(vals[i], s);
			assertEquals(vals[i], t);
		}
	}

	private void check(String[] exp, String[] cols) {
		
		if (exp == null) {
			System.out.print("\t{");
			for (int colIdx = 0; colIdx < cols.length; colIdx++) {
				System.out.print("\"" + cols[colIdx] + "\", ");
			}
			System.out.println("},");
		} else {
			for (int colIdx = 0; colIdx < cols.length; colIdx++) {
				assertEquals(exp[colIdx], cols[colIdx]);
			}
		}
	}
	

}
