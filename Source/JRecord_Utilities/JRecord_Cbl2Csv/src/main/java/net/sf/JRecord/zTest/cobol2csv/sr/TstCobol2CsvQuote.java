package net.sf.JRecord.zTest.cobol2csv.sr;

import static org.junit.Assert.*;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringWriter;

import org.junit.Test;

import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.cbl2csv.Cobol2CsvInterface;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class TstCobol2CsvQuote {

	private static final String COBOL_COPYBOOK = ""
			+ "\n"
			+ "        01  Txt-Record.\n"
			+ "            05  id              pic 9(3).\n"
			+ "            05  txt-1           pic x(12).\n"
			+ "            05  txt-2           pic x(10).\n"
			+ "            05  txt-end         pic x(3).\n";
	
	private static final String COBOL_DATA = ""
			//+ "12 12345678901 123456789 123"
			+ "123Some Txt 1  Txt 1a    321"
			+ "456A , sep     Txt 2a    654"
			+ "234A \" Quote   Sep , E   432"
			+ "345A , Sep     A \" Quote 543"
			+ "456A >\n< Retn  A Fld     654"
			+ "567A >\n< Retn  Rtv>\n< 2  765"
			+ "789Normal 1    Normal 2  987";
	
	private static final String CSV_DATA = ""
			+ "id,txt-1,txt-2,txt-end\n" + 
			"123,Some Txt 1,Txt 1a,321\n" + 
			"456,\"A , sep\",Txt 2a,654\n" + 
			"234,\"A \"\" Quote\",\"Sep , E\",432\n" + 
			"345,\"A , Sep\",\"A \"\" Quote\",543\n" + 
			"456,\"A >\n< Retn\",A Fld,654\n" + 
			"567,\"A >\n< Retn\",\"Rtv>\n< 2\",765\n" + 
			"789,Normal 1,Normal 2,987\n";

	@Test
	public void testBasic() throws IOException {
		ICobolIOBuilder bldr = createIoBuilder();
		StringWriter writer = new StringWriter();
		Cobol2CsvInterface.SINGLE_RECORD_FILES.newCobol2CsvSingleRecordBuilder(bldr)
			.setInputStream(new ByteArrayInputStream(COBOL_DATA.getBytes()))
			.writeCsv(writer);
		//System.out.println(writer.toString());
		assertEquals(CSV_DATA, writer.toString());
		
		writer = new StringWriter();
		Cobol2CsvInterface.SINGLE_RECORD_FILES.newCobol2CsvSingleRecordBuilder(bldr)
			.setInputStream(new ByteArrayInputStream(COBOL_DATA.getBytes()))
			.setFieldSeperator(",")
			.setQuote("\"")
			.writeCsv(writer);
		//System.out.println(writer.toString());
		assertEquals(CSV_DATA, writer.toString());
	}

	@Test
	public void testQuote() throws IOException {
		ICobolIOBuilder bldr = createIoBuilder();
		StringWriter writer = new StringWriter();
		
		Cobol2CsvInterface.SINGLE_RECORD_FILES.newCobol2CsvSingleRecordBuilder(bldr)
			.setInputStream(new ByteArrayInputStream(COBOL_DATA.replace("\"", "'").getBytes()))
			.setFieldSeperator(",")
			.setQuote("'")
			.writeCsv(writer);
		//System.out.println(writer.toString());
		assertEquals(CSV_DATA.replace("\"", "'"), writer.toString());
	}


	@Test
	public void testFieldSeperator() throws IOException {
		ICobolIOBuilder bldr = createIoBuilder();
		StringWriter writer = new StringWriter();
		
		Cobol2CsvInterface.SINGLE_RECORD_FILES.newCobol2CsvSingleRecordBuilder(bldr)
			.setInputStream(new ByteArrayInputStream(
							COBOL_DATA
									.replace(",", "|")
									.getBytes()))
			.setFieldSeperator("|")
			.writeCsv(writer);
		//System.out.println(writer.toString());
		assertEquals(CSV_DATA.replace(",", "|"), writer.toString());
	}
	@Test
	public void testBoth() throws IOException {
		ICobolIOBuilder bldr = createIoBuilder();
		StringWriter writer = new StringWriter();
		
		Cobol2CsvInterface.SINGLE_RECORD_FILES.newCobol2CsvSingleRecordBuilder(bldr)
			.setInputStream(new ByteArrayInputStream(
							COBOL_DATA
									.replace("\"", "'")
									.replace(",", "|")
									.getBytes()))
			.setFieldSeperator("|")
			.setQuote("'")
			.writeCsv(writer);
		//System.out.println(writer.toString());
		assertEquals(CSV_DATA.replace("\"", "'").replace(",", "|"), writer.toString());
	}


	/**
	 * @return
	 * @throws IOException 
	 */
	protected ICobolIOBuilder createIoBuilder() throws IOException {
		return Cobol2CsvInterface.COBOL.newIOBuilder(
					Cobol2CsvInterface.COBOL.newCobolCopybookReader()
						.addFreeFormatCobolText(COBOL_COPYBOOK)
						.setCopybookName("TextRecord")
				)	.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH);
	}

}
