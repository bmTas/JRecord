package net.sf.JRecord.zTest.External;


import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

import junit.framework.TestCase;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.CopybookWriter;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.RecordEditorXmlLoader;
import net.sf.JRecord.External.RecordEditorXmlWriter;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.zTest.Common.IO;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 * Testing RecordEditorCSVReader/Writer classes
 *
 * @author Bruce Martin
 *
 */
public class TstRecordEditXmlParser extends TestCase {

//	private final String csvDdirectory = "/home/bm/Work/RecordEditor/CsvCopybooks/";
//	private final String csvDirectory1 = "/home/bm/Work/RecordEditor/CsvCopybooks/Output/";


	private final String       poFileName = TstConstants.TEMP_DIRECTORY + "poDtl.xml";
//	private final String poHeaderFileName = TstConstants.CSV_DIRECTORY + "poDtl_Header.Txt";
	private final String    poSkuFileName = TstConstants.TEMP_DIRECTORY + "poDtl_Sku.xml";

	private final String       poFileNameO= TstConstants.XML_DIRECTORY_OUTPUT + "ams PO Download.Xml";


	private String eol    = "\n";
	private String [] poDetailLines = {
			"<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
			"<RECORD RECORDNAME=\"ams PO Download\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"GroupOfRecords\" LIST=\"Y\" QUOTE=\"\" RecSep=\"default\" TESTFIELD=\"Record Type\" TESTVALUE=\"H1\" LINE_NO_FIELD_NAMES=\"1\">",
			"	<RECORDS>",
			"		<RECORD RECORDNAME=\"ams PO Download: Header\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" DESCRIPTION=\"PO Download: Header\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\" TESTFIELD=\"Record Type\" TESTVALUE=\"H1\" LINE_NO_FIELD_NAMES=\"1\">",
			"			<FIELDS>",
			"				<FIELD NAME=\"Record Type\" POSITION=\"1\" LENGTH=\"2\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"Sequence Number\" POSITION=\"3\" LENGTH=\"5\" DECIMAL=\"3\" TYPE=\"Num Assumed Decimal (Zero padded)\"/>",
			"				<FIELD NAME=\"Vendor\" POSITION=\"8\" LENGTH=\"10\" TYPE=\"Num (Right Justified zero padded)\"/>",
			"				<FIELD NAME=\"PO\" POSITION=\"18\" LENGTH=\"12\" TYPE=\"Num Assumed Decimal (Zero padded)\"/>",
			"				<FIELD NAME=\"Entry Date\" DESCRIPTION=\"Format YYMMDD\" POSITION=\"30\" LENGTH=\"6\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"Filler\" POSITION=\"36\" LENGTH=\"8\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"beg01 code\" POSITION=\"44\" LENGTH=\"2\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"beg02 code\" POSITION=\"46\" LENGTH=\"2\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"Department\" POSITION=\"48\" LENGTH=\"4\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"Expected Reciept Date\" DESCRIPTION=\"Format YYMMDD\" POSITION=\"52\" LENGTH=\"6\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"Cancel by date\" DESCRIPTION=\"Format YYMMDD\" POSITION=\"58\" LENGTH=\"6\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"EDI Type\" POSITION=\"68\" LENGTH=\"1\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"Add Date\" DESCRIPTION=\"Format YYMMDD\" POSITION=\"69\" LENGTH=\"6\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"Filler\" POSITION=\"75\" LENGTH=\"1\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"Department Name\" POSITION=\"76\" LENGTH=\"10\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"Prcoess Type\" DESCRIPTION=\"C/N Conveyable/Non-Conveyable\" POSITION=\"86\" LENGTH=\"1\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"Order Type\" POSITION=\"87\" LENGTH=\"2\" TYPE=\"Char\"/>",
			"			</FIELDS>",
			"		</RECORD>",
			"		<RECORD RECORDNAME=\"ams PO Download: Detail\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" DESCRIPTION=\"PO Download: Detail\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" PARENT=\"ams PO Download: Header\" QUOTE=\"\" RecSep=\"default\" TESTFIELD=\"Record Type\" TESTVALUE=\"D1\" LINE_NO_FIELD_NAMES=\"1\">",
			"			<FIELDS>",
			"				<FIELD NAME=\"Record Type\" POSITION=\"1\" LENGTH=\"2\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"Pack Qty\" POSITION=\"3\" LENGTH=\"9\" DECIMAL=\"4\" TYPE=\"Num Assumed Decimal (Zero padded)\"/>",
			"				<FIELD NAME=\"Pack Cost\" POSITION=\"12\" LENGTH=\"13\" DECIMAL=\"4\" TYPE=\"Num Assumed Decimal (Zero padded)\"/>",
			"				<FIELD NAME=\"APN\" POSITION=\"25\" LENGTH=\"13\" TYPE=\"Num (Right Justified zero padded)\"/>",
			"				<FIELD NAME=\"Filler\" POSITION=\"38\" LENGTH=\"1\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"Product\" POSITION=\"39\" LENGTH=\"8\" TYPE=\"Num (Right Justified zero padded)\"/>",
			"				<FIELD NAME=\"pmg dtl tech key\" POSITION=\"72\" LENGTH=\"15\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"Case Pack id\" POSITION=\"87\" LENGTH=\"15\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"Product Name\" POSITION=\"101\" LENGTH=\"50\" TYPE=\"Char\"/>",
			"			</FIELDS>",
			"		</RECORD>",
			"	</RECORDS>",
			"</RECORD>"
	};

	private String [] poDetailSkuLines = {
			"<?xml version=\"1.0\" ?>",
			"		<RECORD RECORDNAME=\"ams PO Download: Detail\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" DESCRIPTION=\"PO Download: Detail\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" PARENT=\"ams PO Download: Header\" QUOTE=\"\" RecSep=\"default\" TESTFIELD=\"Record Type\" TESTVALUE=\"D1\">",
			"			<FIELDS>",
			"				<FIELD NAME=\"Record Type\" POSITION=\"1\" LENGTH=\"2\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"Pack Qty\" POSITION=\"3\" LENGTH=\"9\" DECIMAL=\"4\" TYPE=\"Num Assumed Decimal (Zero padded)\"/>",
			"				<FIELD NAME=\"Pack Cost\" POSITION=\"12\" LENGTH=\"13\" DECIMAL=\"4\" TYPE=\"Num Assumed Decimal (Zero padded)\"/>",
			"				<FIELD NAME=\"APN\" POSITION=\"25\" LENGTH=\"13\" TYPE=\"Num (Right Justified zero padded)\"/>",
			"				<FIELD NAME=\"Filler\" POSITION=\"38\" LENGTH=\"1\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"Product\" POSITION=\"39\" LENGTH=\"8\" TYPE=\"Num (Right Justified zero padded)\"/>",
			"				<FIELD NAME=\"pmg dtl tech key\" POSITION=\"72\" LENGTH=\"15\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"Case Pack id\" POSITION=\"87\" LENGTH=\"15\" TYPE=\"Char\"/>",
			"				<FIELD NAME=\"Product Name\" POSITION=\"101\" LENGTH=\"50\" TYPE=\"Char\"/>",
			"			</FIELDS>",
			"		</RECORD>",

	};

	public void testLoadCopyBook1() throws Exception {

		System.out.println("Test 1");
		AbsSSLogger log = new TextLog();
		IO.writeAFile(poSkuFileName, poDetailSkuLines, eol);
		CopybookLoader l = new RecordEditorXmlLoader();
		ExternalRecord copybook = l.loadCopyBook(poSkuFileName, 0, 0, "", 0, 0, log);

		checkSkuCopybook(copybook);
	}


	public void testLoadCopyBook2() throws Exception {

		System.out.println("Test 2");
		AbsSSLogger log = new TextLog();
		IO.writeAFile(poFileName, poDetailLines, eol);
		CopybookLoader l = new RecordEditorXmlLoader();
		ExternalRecord copybook = l.loadCopyBook(poFileName, 0, 0, "", 0, 0, log);

		checkCopybook(copybook);
		checkSkuCopybook(copybook.getRecord(1));
	}

	public void testLoadCopyBook3() throws Exception {

		System.out.println("Test 3");
		AbsSSLogger log = new TextLog();
		IO.writeAFile(poFileName, poDetailLines, eol);
		CopybookLoader l = new RecordEditorXmlLoader();
		CopybookLoader l1= new RecordEditorXmlLoader();
		CopybookWriter w = new RecordEditorXmlWriter();
		ExternalRecord copybook = l.loadCopyBook(poFileName, 0, 0, "", 0, 0, log);

		w.writeCopyBook(TstConstants.XML_DIRECTORY_OUTPUT, copybook, log);
		ExternalRecord copybook1 = l1.loadCopyBook(poFileNameO, 0, 0, "", 0, 0, log);

		checkCopybook(copybook1);
		checkSkuCopybook(copybook1.getRecord(1));
	}

	public void testLoadCopyBook4() throws Exception {

		System.out.println("Test 4");
		AbsSSLogger log = new TextLog();
		IO.writeAFile(poFileName, poDetailLines, eol);
		CopybookLoader l = new RecordEditorXmlLoader();

		CopybookWriter w = new RecordEditorXmlWriter();
		ExternalRecord copybook = l.loadCopyBook(poFileName, 0, 0, "", 0, 0, log);

		w.writeCopyBook(TstConstants.XML_DIRECTORY_OUTPUT, copybook, log);

		compareFile2Array(poDetailLines, poFileNameO);
	}



	private void checkSkuCopybook(ExternalRecord copybook ) {
		assertEquals("1.1 Check no Sub records", 0, copybook.getNumberOfRecords());
		assertEquals("1.2 Check Fields = 9; actual=" + copybook.getNumberOfRecordFields(),9, copybook.getNumberOfRecordFields());
		assertEquals("1.3 Check File Structure=0; actual=" + copybook.getFileStructure(), 0, copybook.getFileStructure());
		assertEquals("1.4 Check Record Type=1; actual=" + copybook.getRecordType(), 1, copybook.getRecordType());
		assertEquals("1.5 Check Sep=<Tab>; actual=" + copybook.getDelimiter(), "<Tab>", copybook.getDelimiter());
		assertEquals("1.6 Check Style=0; actual=" + copybook.getRecordStyle(), 0, copybook.getRecordStyle());
		assertEquals("1.7 Check List=N; actual=" + copybook.getListChar(), "N", copybook.getListChar());

		ExternalField f = copybook.getRecordField(1);

		assertEquals("1.8 Check Field Name = 'Pack Qty'; actual=" + f.getName(), "Pack Qty", f.getName());
		assertEquals("1.9 Check Field Pos=3 actual=" + f.getPos(), 3 , f.getPos());
		assertEquals("1.A Check Field Type=8 actual=" + f.getType(), 8, f.getType());
	}

	private void checkCopybook(ExternalRecord copybook) {

		assertEquals("2.1 Check Sub records=2", 2, copybook.getNumberOfRecords());
		assertEquals("2.2 Check Fields=0; actual=" + copybook.getNumberOfRecordFields(), 0, copybook.getNumberOfRecordFields());
		assertEquals("2.3 Check File Structure=0; actual=" + copybook.getFileStructure(), 0, copybook.getFileStructure());
		assertEquals("2.4 Check Record Type=9; actual=" + copybook.getRecordType(), 9, copybook.getRecordType());
		assertEquals("2.5 Check Sep=<Tab>; actual=" + copybook.getDelimiter(), "<Tab>", copybook.getDelimiter());
		assertEquals("2.6 Check Style=0; actual=" + copybook.getRecordStyle(), 0, copybook.getRecordStyle());
		assertEquals("2.7 Check List=Y; actual=" + copybook.getListChar(), "Y", copybook.getListChar());
	}


	/**
	 * Compare the contents of a file and an array of String
	 * @param lines
	 * @param filename
	 * @throws IOException
	 */
	private void compareFile2Array(String[] lines, String filename) throws IOException {
		String newLines = readFile(filename);
		String oldLines;
		StringBuffer s = new StringBuffer();

		System.out.println(" Starting file: " + filename);

		for (int i = 0; i < lines.length; i++) {
			s.append(lines[i].trim());
		}
		oldLines = s.toString();

		if (! oldLines.equals(newLines)) {
				System.out.println();
				System.out.println("Error file: " + filename + "  " + oldLines.compareTo(newLines));
				System.out.println(">>" + oldLines);
				System.out.println(">>" + newLines);
				assertEquals("Error line " , oldLines, newLines);
		}
	}

	/**
	 * Read a file into an array of lines
	 *
	 * @param fileName name of file to be read
	 * @return file contents
	 *
	 * @throws IOException any error that occurred
	 */
	private String readFile(String fileName) throws IOException {
		//ArrayList<String> lines = new ArrayList<String>();
		StringBuffer ret = new StringBuffer();
		String s;

		BufferedReader r = new BufferedReader(new InputStreamReader(new FileInputStream(fileName)));

		while ((s = r.readLine()) != null) {
			ret.append(s.trim());
		}
		r.close();

		return ret.toString();
	//	InputS
	}
}
