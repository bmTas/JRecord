package net.sf.JRecord.zTest.cobol2csv;

import static org.junit.Assert.assertEquals;

import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringWriter;

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

public class TstMultiRecordCobol2Csv3 {

	public static final String[][] EXPECTED = {
		{"62684671", "20", "40118", "685", "-1", "-69.99", },
		{"64634429", "20", "40118", "957", "1", "3.99", },
		{"66624458", "20", "0", "0", "0", "0.00", },
		{"63674861", "20", "9999999", "fff", "fffffffff", "fffffffffff", },
		{"65674532", "20", "404040", "4040", "4040404040", "4040404040.40", },
		{"64614401", "59", "40118", "957", "1", "1.99", },
		{"64614401", "59", "40118", "957", "1", "1.99", },
		{"61664713", "59", "40118", "335", "1", "17.99", },
		{"61664713", "59", "40118", "335", "-1", "-17.99", },
		{"68634752", "59", "40118", "410", "1", "8.99", },
		{"60614487", "59", "40118", "878", "1", "5.95", },
		{"63644339", "59", "40118", "878", "1", "12.65", },
		{"60694698", "59", "40118", "620", "1", "3.99", },
		{"60664659", "59", "40118", "620", "1", "3.99", },
		{"62684217", "59", "40118", "957", "1", "9.99", },
		{"67674686", "59", "40118", "929", "1", "3.99", },
		{"61684613", "59", "0", "0", "0", "0.00", },
		{"64624770", "59", "9999999", "fff", "fffffffff", "fffffffffff", },
		{"69694814", "166", "404040", "4040", "4040404040", "4040404040.40", },
		{"69694814", "166", "40118", "360", "1", "2.50", },
		{"69644164", "166", "40118", "193", "1", "21.59", },
		{"62684907", "166", "40118", "375", "1", "13.99", },
		{"62694193", "166", "40118", "375", "1", "13.99", },
		{"62694193", "166", "40118", "375", "-1", "-13.99", },
		{"62694193", "166", "40118", "375", "1", "11.99", },
		{"63654450", "166", "40118", "320", "1", "13.99", },
		{"62664576", "166", "40118", "320", "1", "9.72", },
		{"63634260", "166", "40118", "320", "1", "5.59", },
	};
	
	private static String RESULT
			= "62684671;20;40118;685;1;69.99\n"
			+ "62684671;20;40118;685;-1;-69.99\n"
			+ "64634429;20;40118;957;1;3.99\n"
			+ "66624458;20;0;Low-Values;Low-Values;Low-Values\n"
			+ "63674861;20;9999999;High-Values;High-Values;High-Values\n"
			+ "65674532;20;404040;;;\n"
			+ "64614401;59;40118;957;1;1.99\n"
			+ "64614401;59;40118;957;1;1.99\n"
			+ "61664713;59;40118;335;1;17.99\n"
			+ "61664713;59;40118;335;-1;-17.99\n"
			+ "68634752;59;40118;410;1;8.99\n"
			+ "60614487;59;40118;878;1;5.95\n"
			+ "63644339;59;40118;878;1;12.65\n"
			+ "60694698;59;40118;620;1;3.99\n"
			+ "60664659;59;40118;620;1;3.99\n"
			+ "62684217;59;40118;957;1;9.99\n"
			+ "67674686;59;40118;929;1;3.99\n"
			+ "61684613;59;0;Low-Values;Low-Values;Low-Values\n"
			+ "64624770;59;9999999;High-Values;High-Values;High-Values\n"
			+ "69694814;166;404040;;;\n"
			+ "69694814;166;40118;360;1;2.50\n"
			+ "69644164;166;40118;193;1;21.59\n"
			+ "62684907;166;40118;375;1;13.99\n"
			+ "62694193;166;40118;375;1;13.99\n"
			+ "62694193;166;40118;375;-1;-13.99\n"
			+ "62694193;166;40118;375;1;11.99\n"
			+ "63654450;166;40118;320;1;13.99\n"
			+ "62664576;166;40118;320;1;9.72\n"
			+ "63634260;166;40118;320;1;5.59\n";
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
	public void test2() throws IOException {
		StringWriter sw = new StringWriter(2000);
		
		createCsvBuilder(";")
				.setCsvWriter(new BufferedWriter(sw))
				.setCsvHeader(false)
				.setReportInvalidFields(true)
			.run();

		String[][] expected = EXPECTED.clone();
		updateResult(expected,  "0", "Low-Values");
		updateResult(expected,  "9999999", "High-Values");
		updateResult(expected,  "404040", "");
		
		doCheck(sw.toString(), ";", "\"",  expected, RESULT);
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

	@Test
	public void test5() throws IOException {
		check(null, null, null);
	}

	@Test
	public void test6() throws IOException {
		check("LOW", null, null);
		check(null, "HIGH", null);
		check(null, null, "SPACES");
		check("LOW", "HIGH", "SPACES");
	}
	
	
	private void check(String low, String high, String spaces) throws IOException {
		String[][]expected = EXPECTED.clone();
		StringBuilder b = new StringBuilder(RESULT);
		StringWriter sw = new StringWriter(2000);
		
		ICobolToCsvBldr csvBldr = createCsvBuilder(";")
				.setCsvWriter(new BufferedWriter(sw))
				.setCsvHeader(false)
				.setReportInvalidFields(true);
		
		if (low != null) {
			csvBldr.setLowValueTxt(low);
			
			b = updateResults(expected, b, "0", "Low-Values", low);
		} else {
			updateResult(expected,  "0", "Low-Values");
		}
		if (high != null) {
			csvBldr.setHighValueTxt(high);
			
			b = updateResults(expected, b, "9999999", "High-Values", high);
		} else {
			updateResult(expected,  "9999999", "High-Values");
		}
		if (spaces != null) {
			csvBldr.setNumericSpacesTxt(spaces);
			
			b = updateResults(expected, b, "404040", "", spaces);
		} else {
			updateResult(expected,  "404040", "");
		}
		csvBldr
			.run();

		
		doCheck(sw.toString(), ";", "\"", expected, b.toString());
	}

	private StringBuilder updateResults(String[][] expected, StringBuilder b, String valTst, String val, String newValue) {
		updateResult(expected, valTst, newValue);
		return Conversion.replace(b,
				";" + val + ";" + val + ";" + val, 
				";" + newValue + ";" + newValue + ";" + newValue );
	}

	private void updateResult(String[][] expected, String valTst, String newValue) {
		for ( int i = 0; i < expected.length; i++) {
			if (valTst.equals(expected[i][2])) {
				expected[i] = expected[i].clone();
				for (int idx = 3; idx < expected[i].length; idx++) {
					expected[i][idx] = newValue; 
				}
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


	
	private ICobolToCsvBldr createCsvBuilder(String sep) throws IOException {
		String dtar020CobolFileName = Data.DTAR020_COPYBOOK_FILE_NAME;
		//URL resource = Data.CSVTST1_RESOURCE;
		
		ICobolIOBuilder ioBldr = CobolToCsvBldr.newCobolIOBuilder(dtar020CobolFileName)
				.setFont("cp037")
				.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
				.setSplitCopybook(CopybookLoader.SPLIT_NONE)
				.setOptimizeTypes(false)
		;
		ICobolToCsvBldr ret = CobolToCsvBldr.newMultiRecordCsvBuilder()
				.setCsvHeader(true)
				.setLineReader(ioBldr.newReader(Data.DTAR020_CSVTST_RESOURCE.openStream()));

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
		doCheck(csv, sep, quote, EXPECTED, null);
	}
	
	private void doCheck(String csv, String sep, String quote, String[][] expected, String expectedStr) throws IOException {
		ICsvIOBuilder ioBldr = JRecordInterface1.CSV.newIOBuilder(sep, quote)
				.defineFields()
					.addCsvField("field1", Type.ftChar, 0)
					.addCsvField("field2", Type.ftChar, 0)
					.addCsvField("field3", Type.ftChar, 0)
					.addCsvField("field4", Type.ftChar, 0)
					.addCsvField("field5", Type.ftChar, 0)
					.addCsvField("field6", Type.ftChar, 0)
				.endOfRecord();
		byte[] b = Conversion.getBytes(csv, "");
		
		AbstractLineReader r = ioBldr.newReader(new ByteArrayInputStream(b));
		AbstractLine line;
		
//		System.out.println();
//		System.out.println(csv);
//		System.out.println();
		
		if (expectedStr != null) {
			if (! ";".equals(sep)) {
				expectedStr = Conversion.replace(expectedStr, ";", sep).toString();
			}
			assertEquals(expectedStr, csv);
		}
		
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
