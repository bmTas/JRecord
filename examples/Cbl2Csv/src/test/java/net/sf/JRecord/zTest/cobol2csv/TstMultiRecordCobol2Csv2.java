package net.sf.JRecord.zTest.cobol2csv;

import static org.junit.Assert.assertEquals;

import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.cbl2csv.imp.CobolToCsvBldr;
import net.sf.JRecord.cbl2csv.imp.ICobolToCsvBldr;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.def.IO.builders.ICsvIOBuilder;
import net.sf.JRecord.zData.Data;

public class TstMultiRecordCobol2Csv2 {

	public static final String[][] EXPECTED = {
			{"2", "123\"\"\"\"\"", "\"124", "12,34", },
			{"1", "345", "12\"34", "45\"66", },
			{"2", "123,456", "\"345", "1,23", },
			{"1", "12", "'123'", "12:34", },
			{"2", "123'''''", "'124", "12:34", },
			{"1", "345", "12'34", "45'66", },
			{"2", "123:456", "'345", "1:23", },
			{"1", "12", "'123'", "12:34", },
			{"2", "123'''''", "'124", "12:34", },
			{"1", "345", "12'34", "45'66", },
			{"2", "123:456", "'345", "1:23", },
	};
	@Test
	public void test1() throws IOException {
		StringWriter sw = new StringWriter(2000);
		
		createCsvBuilder("")
				.setCsvWriter(new BufferedWriter(sw))
				.setCsvHeader(false)
			.run();

		doCheck(sw.toString(), "\t", "\"");
	}
	

	@Test
	public void test3() throws IOException {
		String[] seps = {",", "|", "\t", ":", ""};
		String[] quotes = {"\"", "'", "`"};
		
		for (String sep : seps) {
			for (String q : quotes) {
				checkSeperator(sep, q);
			}
		}
	}
	
	
	private void checkSeperator(String sep, String quote) throws IOException {
		StringWriter sw = new StringWriter(2000);
		
		createCsvBuilder(sep)
				.setCsvWriter(new BufferedWriter(sw))
				.setQuote(quote)
				.setCsvHeader(false)
			.run();
		
		sep = sep.length() == 0 ? "\t" : sep;

		doCheck(sw.toString(), sep, quote);
	}

	
	@Test
	public void testMultiFile1() throws IOException {
		StringWriter str1 = new StringWriter(2000);
		StringWriter str2 = new StringWriter(2000);
		
		createCsvBuilder("!")
				.setCsvHeader(false)
				.addRecordDetails("record-1", new BufferedWriter(str1))
				.addRecordDetails("record-2", new BufferedWriter(str2))
			.run();

		//String[] poData = Data.getPoData();
		List<String[]>  exp1 = new ArrayList<String[]>(),
				 exp2 = new ArrayList<String[]>();
		
		for (int i = 1; i < EXPECTED.length; i++) {
			String[] s = EXPECTED[i];
			if ("1".equals(s[0])) {
				exp1.add(s);
			} else  {
				exp2.add(s);
			}
		}
		doCheck(str1.toString(), "!", "\"", exp1.toArray(new String[exp1.size()][]));
		doCheck(str2.toString(), "!", "\"", exp2.toArray(new String[exp2.size()][]));

	}

	
	private ICobolToCsvBldr createCsvBuilder(String sep) throws IOException {
		String poDownloadCobolFileName = Data.CSVTST1_COPYBOOK_FILE_NAME;
		//URL resource = Data.CSVTST1_RESOURCE;
		
		ICobolIOBuilder ioBldr = CobolToCsvBldr.newCobolIOBuilder(poDownloadCobolFileName)
				.setFont(Conversion.DEFAULT_ASCII_CHARSET)
				.setFileOrganization(IFileStructureConstants.IO_BIN_TEXT)
				.setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
				.setRecordDecider(JRecordInterface1.RECORD_DECIDER_BUILDER.singleFieldDeciderBuilder("Record-Type", false)
						.addRecord("1", "record-1")
						.addRecord("2", "record-2")
						.setCaseSensitive(false)
					.build()
		);
		ICobolToCsvBldr ret = CobolToCsvBldr.newMultiRecordCsvBuilder()
				.setCsvHeader(true)
				.setLineReader(ioBldr.newReader(Data.CSVTST1_RESOURCE.openStream()));

		if(sep.length() > 0) {
			ret.setSeparator(sep);
		}
		
		return ret;
	}
//
//	private void doCheck(StringWriter sw, List<String> lines) {
//		doCheck(sw.toString(), 0, lines.toArray(new String[lines.size()]), 0);
//	}
	
	
	private void doCheck(String csv, String sep, String quote) throws IOException {
		doCheck(csv, sep, quote, EXPECTED);
	}
	
	private void doCheck(String csv, String sep, String quote, String[][] expected) throws IOException {
		ICsvIOBuilder ioBldr = JRecordInterface1.CSV.newIOBuilder(sep, quote)
				.defineFields()
					.addCsvField("field1", Type.ftChar, 0)
					.addCsvField("field2", Type.ftChar, 0)
					.addCsvField("field3", Type.ftChar, 0)
					.addCsvField("field4", Type.ftChar, 0)
				.endOfRecord();
		byte[] b = Conversion.getBytes(csv, "");
		
		AbstractLineReader r = ioBldr.newReader(new ByteArrayInputStream(b));
		AbstractLine line;
		
		if (expected == null) {
			while((line = r.read()) != null) {
				System.out.print("\t{");
				for (AbstractFieldValue fv :line.getFieldIterator(0)) {
					System.out.print("\"" 
								+ Conversion.replace(fv.asString(), "\"", "\\\"") 
								+ "\", ");
				}
				System.out.println("},");
			}
		} else {
			//System.out.println(csv + "\n\n");
			int row =0, col;
			while((line = r.read()) != null) {
				col = 0;
				//System.out.println(row + ">>" + expected[row].toString());
				for (AbstractFieldValue fv :line.getFieldIterator(0)) {
					String val = fv.asString();
					
					if (col >= expected[row].length) {
						if (val.length() > 0) {
							throw new RuntimeException("On row" + row  +" Extra Field: " + val);
						}
						System.out.println("Extra Field at " + row + " " + sep + quote);
						col++;
					} else {
						assertEquals(sep + quote + " " + row + ", " + col + ", ", expected[row][col++], val);
					}
				}
				row += 1;
			}
			assertEquals(expected.length, row);
		}
		r.close();	
		

	}
//	private void doCheck(String csv, int firstCsvLine, String[] expected, int firstExpectedLine) {
//			
//			JRecordInterface1.CSV.newIOBuilder(delimiter, quote)
//			String[] lines = csv.split("\n"); 
//			
//			int length = Math.min(expected.length + firstExpectedLine - firstCsvLine , lines.length);
//			for (int i = firstCsvLine; i < length; i++) {
//				assertEquals(i + ": ", lines[i], expected[i - firstCsvLine + firstExpectedLine]);
//			}
//			assertEquals(expected.length - firstExpectedLine + firstCsvLine, lines.length);
//	}
}
