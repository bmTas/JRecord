package net.sf.JRecord.zTest.cobol2csv;

import static org.junit.Assert.assertEquals;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.cbl2csv.imp.CobolToCsvBldr;
import net.sf.JRecord.cbl2csv.imp.ICobolToCsvBldr;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zData.Data;

public class TstMultiRecordCobol2Csv {

	@Test
	public void test1() throws IOException {
		StringWriter sw = new StringWriter(2000);
		
		createCsvBuilder()
				.setCsvWriter(new BufferedWriter(sw))
			.run();

		doCheck(sw.toString(), 0, Data.getPoData(), 0);
	}
	@Test
	public void test2() throws IOException {
		StringWriter sw1 = new StringWriter(2000);
		
		createCsvBuilder()
				.setCsvWriter(new BufferedWriter(sw1))
				.setCsvHeader(false)
			.run();

		doCheck(sw1.toString(), 0, Data.getPoData(), 1);
	}
	

	@Test
	public void test3() throws IOException {
		checkSeperator(",");
		checkSeperator("|");
		checkSeperator("\t");
		checkSeperator("");
	}
	
	
	private void checkSeperator(String sep) throws IOException {
		StringWriter sw = new StringWriter(2000);
		
		createCsvBuilder(sep)
				.setCsvWriter(new BufferedWriter(sw))
			.run();

		String[] poData = Data.getPoData();
		
		sep = sep.length() == 0 ? "\t" : sep;
		for (int i = 0; i < poData.length; i++) {
			poData[i] = Conversion.replace(poData[i], ";", sep).toString();
		}
		doCheck(sw.toString(), 0, poData, 0);
	}

	
	@Test
	public void testMultiFile1() throws IOException {
		StringWriter swH1 = new StringWriter(2000);
		StringWriter swD1 = new StringWriter(2000);
		StringWriter swS1 = new StringWriter(2000);
		
		createCsvBuilder()
				.setCsvHeader(false)
				.addRecordDetails("PO-Record", new BufferedWriter(swH1))
				.addRecordDetails("Product-Record",  new BufferedWriter(swD1))
				.addRecordDetails("Location-Record", new BufferedWriter(swS1))
			.run();

		String[] poData = Data.getPoData();
		List<String>  poLines = new ArrayList<String>(),
				 productLines = new ArrayList<String>(),
				locationLines = new ArrayList<String>();
		
		for (int i = 1; i < poData.length; i++) {
			String s = poData[i];
			if (s.startsWith("H1")) {
				poLines.add(s);
			} else if (s.startsWith("D1")) {
				productLines.add(s);
			}  else if (s.startsWith("S1")) {
				locationLines.add(s);
			}
		}
		
		doCheck(swH1, poLines);
		doCheck(swD1, productLines);
		doCheck(swS1, locationLines);
	}
	
	@Test
	public void testMultiFile2() throws IOException {
		StringWriter sw = new StringWriter(2000);
		StringWriter swS1 = new StringWriter(2000);
		
		createCsvBuilder()
				.setCsvHeader(false)
				.setCsvWriter(new BufferedWriter(sw))
				.addRecordDetails("Location-Record", new BufferedWriter(swS1))
			.run();

		String[] poData = Data.getPoData();
		List<String>  otherLines = new ArrayList<String>(),
				   locationLines = new ArrayList<String>();
		
		for (int i = 1; i < poData.length; i++) {
			String s = poData[i];
			if (s.startsWith("S1")) {
				locationLines.add(s);
			} else {
				otherLines.add(s);	
			}
		}
		

		doCheck(sw, otherLines);
		doCheck(swS1, locationLines);
	}
	
	private ICobolToCsvBldr createCsvBuilder() throws IOException {
		return createCsvBuilder(";");
	}
	
	private ICobolToCsvBldr createCsvBuilder(String sep) throws IOException {
		String poDownloadCobolFileName = Data.AMS_PO_COBOL_COPYBOOK_FILE_NAME;
		URL resource = Data.AMS_PO_DATA_RESOUCE;
		
		ICobolIOBuilder ioBldr = CobolToCsvBldr.newCobolIOBuilder(poDownloadCobolFileName)
				.setFont(Conversion.DEFAULT_ASCII_CHARSET)
				.setFileOrganization(IFileStructureConstants.IO_BIN_TEXT)
				.setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
				.setRecordDecider(JRecordInterface1.RECORD_DECIDER_BUILDER.singleFieldDeciderBuilder("Record-Type", false)
						.addRecord("H1", "PO-Record")
						.addRecord("D1", "Product-Record")
						.addRecord("S1", "Location-Record")
						.setCaseSensitive(false)
					.build()
		);
		ICobolToCsvBldr ret = CobolToCsvBldr.newMultiRecordCsvBuilder()
				.setCsvHeader(true)
				.setLineReader(ioBldr.newReader(resource.openStream()));

		if(sep.length() > 0) {
			ret.setSeparator(sep);
		}
		
		return ret;
	}

	private void doCheck(StringWriter sw, List<String> lines) {
		doCheck(sw.toString(), 0, lines.toArray(new String[lines.size()]), 0);
	}
	private void doCheck(String csv, int firstCsvLine, String[] expected, int firstExpectedLine) {
		String[] lines = csv.split("\n"); 
		
		int length = Math.min(expected.length + firstExpectedLine - firstCsvLine , lines.length);
		for (int i = firstCsvLine; i < length; i++) {
			assertEquals(i + ": ", lines[i], expected[i - firstCsvLine + firstExpectedLine]);
		}
		assertEquals(expected.length - firstExpectedLine + firstCsvLine, lines.length);
	}
}
