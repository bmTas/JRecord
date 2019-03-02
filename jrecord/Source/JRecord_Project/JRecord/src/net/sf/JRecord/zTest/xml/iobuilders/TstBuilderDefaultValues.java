package net.sf.JRecord.zTest.xml.iobuilders;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringReader;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.ExternalRecord;

/**
 * The class checks the setFont / setDefaultFont + setFileOrganization are
 * applied correctly for Xml-Schema's
 *  
 * @author Bruce Martin
 *
 */
public class TstBuilderDefaultValues extends TestCase {
	private static final String DTAR020_XML_1 = 
		 	"<?xml version=\"1.0\" ?>\n"
		+	"<RECORD RECORDNAME=\"DTAR020\" FONTNAME=\"CP037\" FILESTRUCTURE=\"Fixed_Length\" RECORDTYPE=\"RecordLayout\">\n"
		+	"	<FIELDS>\n"
		+	"		<FIELD NAME=\"KEYCODE-NO\" POSITION=\"1\"  LENGTH=\"8\" TYPE=\"Char\" />\n"
		+	"		<FIELD NAME=\"STORE-NO\"   POSITION=\"9\"  LENGTH=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"DATE\"       POSITION=\"11\" LENGTH=\"4\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"DEPT-NO\"    POSITION=\"15\" LENGTH=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"QTY-SOLD\"   POSITION=\"17\" LENGTH=\"5\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"SALE-PRICE\" POSITION=\"22\" LENGTH=\"6\" DECIMAL=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"	</FIELDS>\n"
		+	"</RECORD>";

	private static final String DTAR020_XML_2 = 
		 	"<?xml version=\"1.0\" ?>\n"
		+	"<RECORD RECORDNAME=\"DTAR020\" FILESTRUCTURE=\"Fixed_Length\" RECORDTYPE=\"RecordLayout\">\n"
		+	"	<FIELDS>\n"
		+	"		<FIELD NAME=\"KEYCODE-NO\" POSITION=\"1\"  LENGTH=\"8\" TYPE=\"Char\" />\n"
		+	"		<FIELD NAME=\"STORE-NO\"   POSITION=\"9\"  LENGTH=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"DATE\"       POSITION=\"11\" LENGTH=\"4\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"DEPT-NO\"    POSITION=\"15\" LENGTH=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"QTY-SOLD\"   POSITION=\"17\" LENGTH=\"5\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"SALE-PRICE\" POSITION=\"22\" LENGTH=\"6\" DECIMAL=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"	</FIELDS>\n"
		+	"</RECORD>";

	
	public void testDefaultsOverriden()throws IOException {
		ExternalRecord xRec = loadXRecordWithDefault(DTAR020_XML_1,  "cp1252");
		
		assertEquals(Constants.IO_FIXED_LENGTH, xRec.getFileStructure());
		assertEquals("CP037", xRec.getFontName());
		
		xRec = loadXRecordStreamWithDefault(DTAR020_XML_1,  "cp1252");
		
		assertEquals(Constants.IO_FIXED_LENGTH, xRec.getFileStructure());
		assertEquals("CP037", xRec.getFontName());
	}
	
	
	public void testDefaultsUsed()throws IOException {
		ExternalRecord xRec = loadXRecordWithDefault(DTAR020_XML_2,  "CP273");
		
		assertEquals(Constants.IO_FIXED_LENGTH, xRec.getFileStructure());
		assertEquals("CP273", xRec.getFontName());
		
		xRec = loadXRecordStreamWithDefault(DTAR020_XML_2,  "CP273");
		
		assertEquals(Constants.IO_FIXED_LENGTH, xRec.getFileStructure());
		assertEquals("CP273", xRec.getFontName());
	}
	
	
	public void testLoadUpdate()throws IOException {
		
		tstUpdate(DTAR020_XML_1);
		tstUpdate(DTAR020_XML_2);
	}


	private void tstUpdate(String copybook) throws IOException {
		ExternalRecord xRec = loadXRecord(copybook,  "CP273");
		
		assertEquals(Constants.IO_VB, xRec.getFileStructure());
		assertEquals("CP273", xRec.getFontName());
		
		xRec = loadXRecordStream(copybook,  "CP273");
		
		assertEquals(Constants.IO_VB, xRec.getFileStructure());
		assertEquals("CP273", xRec.getFontName());
	}
	

	
	private ExternalRecord loadXRecordWithDefault(String xml,  String font) throws IOException {
		return JRecordInterface1.SCHEMA_XML
					.newIOBuilder(new StringReader(xml), "DTAR020")
					.setDefaultFont(font)
				.getExternalRecord();
	}
	
	
	private ExternalRecord loadXRecordStreamWithDefault(String xml,  String font) throws IOException {
		return JRecordInterface1.SCHEMA_XML
					.newIOBuilder(new ByteArrayInputStream(xml.getBytes()), "DTAR020")
					.setDefaultFont(font)
				.getExternalRecord();
	}

	
	private ExternalRecord loadXRecord(String xml,  String font) throws IOException {
		return JRecordInterface1.SCHEMA_XML
					.newIOBuilder(new StringReader(xml), "DTAR020")
					.setFileOrganization(Constants.IO_VB)
					.setFont(font)
				.getExternalRecord();
	}
	
	
	private ExternalRecord loadXRecordStream(String xml,  String font) throws IOException {
		return JRecordInterface1.SCHEMA_XML
					.newIOBuilder(new ByteArrayInputStream(xml.getBytes()), "DTAR020")
					.setFileOrganization(Constants.IO_VB)
					.setFont(font)
				.getExternalRecord();
	}

}
