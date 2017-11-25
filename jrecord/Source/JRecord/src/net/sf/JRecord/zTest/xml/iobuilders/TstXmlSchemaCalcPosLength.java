package net.sf.JRecord.zTest.xml.iobuilders;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.RecordEditorXmlLoader;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.zTest.Common.CommonCodeFields;


/**
 * Checking Position / length correctly for various combinations of
 * supplying / skipping position an lengths
 * 
 * @author Bruce Martin
 *
 */
public class TstXmlSchemaCalcPosLength extends TestCase {
	private static String[] ids = { "Position & Length", "Position","Length", "Mixed"};
	private static final String[] DTAR020_XML_1 = {
			 	"<?xml version=\"1.0\" ?>\n"
			+	"<RECORD RECORDNAME=\"DTAR020\" FONTNAME=\"CP037\" FILESTRUCTURE=\"Default\" RECORDTYPE=\"RecordLayout\">\n"
			+	"	<FIELDS>\n"
			+	"		<FIELD NAME=\"KEYCODE-NO\" POSITION=\"1\"  LENGTH=\"8\" TYPE=\"Char\" />\n"
			+	"		<FIELD NAME=\"STORE-NO\"   POSITION=\"9\"  LENGTH=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
			+	"		<FIELD NAME=\"DATE\"       POSITION=\"11\" LENGTH=\"4\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
			+	"		<FIELD NAME=\"DEPT-NO\"    POSITION=\"15\" LENGTH=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
			+	"		<FIELD NAME=\"QTY-SOLD\"   POSITION=\"17\" LENGTH=\"5\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
			+	"		<FIELD NAME=\"SALE-PRICE\" POSITION=\"22\" LENGTH=\"6\" DECIMAL=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
			+	"	</FIELDS>\n"
			+	"</RECORD>",
			
			 	"<?xml version=\"1.0\" ?>\n"
			+	"<RECORD RECORDNAME=\"DTAR020\" FONTNAME=\"CP037\" FILESTRUCTURE=\"Default\" RECORDTYPE=\"RecordLayout\">\n"
			+	"	<FIELDS>\n"
			+	"		<FIELD NAME=\"KEYCODE-NO\" POSITION=\"1\"  TYPE=\"Char\" />\n"
			+	"		<FIELD NAME=\"STORE-NO\"   POSITION=\"9\"  TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
			+	"		<FIELD NAME=\"DATE\"       POSITION=\"11\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
			+	"		<FIELD NAME=\"DEPT-NO\"    POSITION=\"15\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
			+	"		<FIELD NAME=\"QTY-SOLD\"   POSITION=\"17\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
			+	"		<FIELD NAME=\"SALE-PRICE\" POSITION=\"22\" LENGTH=\"6\" DECIMAL=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
			+	"	</FIELDS>\n"
			+	"</RECORD>",
			
			 	"<?xml version=\"1.0\" ?>\n"
			+	"<RECORD RECORDNAME=\"DTAR020\" FONTNAME=\"CP037\" FILESTRUCTURE=\"Default\" RECORDTYPE=\"RecordLayout\">\n"
			+	"	<FIELDS>\n"
			+	"		<FIELD NAME=\"KEYCODE-NO\"  LENGTH=\"8\" TYPE=\"Char\" />\n"
			+	"		<FIELD NAME=\"STORE-NO\"    LENGTH=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
			+	"		<FIELD NAME=\"DATE\"        LENGTH=\"4\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
			+	"		<FIELD NAME=\"DEPT-NO\"     LENGTH=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
			+	"		<FIELD NAME=\"QTY-SOLD\"    LENGTH=\"5\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
			+	"		<FIELD NAME=\"SALE-PRICE\"  LENGTH=\"6\" DECIMAL=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
			+	"	</FIELDS>\n"
			+	"</RECORD>",
			
		 	"<?xml version=\"1.0\" ?>\n"
		+	"<RECORD RECORDNAME=\"DTAR020\" FONTNAME=\"CP037\" FILESTRUCTURE=\"Default\" RECORDTYPE=\"RecordLayout\">\n"
		+	"	<FIELDS>\n"
		+	"		<FIELD NAME=\"KEYCODE-NO\" POSITION=\"1\"  LENGTH=\"8\" TYPE=\"Char\" />\n"
		+	"		<FIELD NAME=\"STORE-NO\"                                TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"DATE\"       POSITION=\"11\" LENGTH=\"4\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"DEPT-NO\"                                 TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"QTY-SOLD\"   POSITION=\"17\" LENGTH=\"5\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"SALE-PRICE\"                 LENGTH=\"6\" DECIMAL=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"	</FIELDS>\n"
		+	"</RECORD>",

	};
	
	private static final String[] DTAR020_XML_2 = {
		 	"<?xml version=\"1.0\" ?>\n"
		+	"<RECORD RECORDNAME=\"DTAR020\" FONTNAME=\"CP037\" FILESTRUCTURE=\"Default\" RECORDTYPE=\"RecordLayout\">\n"
		+	"	<FIELDS>\n"
		+	"		<FIELD NAME=\"KEYCODE-NO\" POSITION=\"1\"  LENGTH=\"8\" TYPE=\"Char\" />\n"
		+	"		<FIELD NAME=\"STORE-NO\"   POSITION=\"9\"  LENGTH=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"DATE\"       POSITION=\"11\" LENGTH=\"4\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"QTY-SOLD\"   POSITION=\"17\" LENGTH=\"5\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"SALE-PRICE\" POSITION=\"22\" LENGTH=\"6\" DECIMAL=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"	</FIELDS>\n"
		+	"</RECORD>",
		
		 	"<?xml version=\"1.0\" ?>\n"
		+	"<RECORD RECORDNAME=\"DTAR020\" FONTNAME=\"CP037\" FILESTRUCTURE=\"Default\" RECORDTYPE=\"RecordLayout\">\n"
		+	"	<FIELDS>\n"
		+	"		<FIELD NAME=\"KEYCODE-NO\" POSITION=\"1\"               TYPE=\"Char\" />\n"
		+	"		<FIELD NAME=\"STORE-NO\"   POSITION=\"9\"               TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"DATE\"       POSITION=\"11\" LENGTH=\"4\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"QTY-SOLD\"   POSITION=\"17\"              TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"SALE-PRICE\" POSITION=\"22\" LENGTH=\"6\" DECIMAL=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"	</FIELDS>\n"
		+	"</RECORD>",
		
		 	"<?xml version=\"1.0\" ?>\n"
		+	"<RECORD RECORDNAME=\"DTAR020\" FONTNAME=\"CP037\" FILESTRUCTURE=\"Default\" RECORDTYPE=\"RecordLayout\">\n"
		+	"	<FIELDS>\n"
		+	"		<FIELD NAME=\"KEYCODE-NO\"  LENGTH=\"8\"                 TYPE=\"Char\" />\n"
		+	"		<FIELD NAME=\"STORE-NO\"    LENGTH=\"2\"                 TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"DATE\"        LENGTH=\"4\"                 TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"QTY-SOLD\"    POSITION=\"17\" LENGTH=\"5\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"SALE-PRICE\"  LENGTH=\"6\" DECIMAL=\"2\"   TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"	</FIELDS>\n"
		+	"</RECORD>",
		
		 	"<?xml version=\"1.0\" ?>\n"
		+	"<RECORD RECORDNAME=\"DTAR020\" FONTNAME=\"CP037\" FILESTRUCTURE=\"Default\" RECORDTYPE=\"RecordLayout\">\n"
		+	"	<FIELDS>\n"
		+	"		<FIELD NAME=\"KEYCODE-NO\" POSITION=\"1\"  LENGTH=\"8\" TYPE=\"Char\" />\n"
		+	"		<FIELD NAME=\"STORE-NO\"                                TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"DATE\"       POSITION=\"11\" LENGTH=\"4\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"QTY-SOLD\"   POSITION=\"17\" LENGTH=\"5\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"SALE-PRICE\"                 LENGTH=\"6\" DECIMAL=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"	</FIELDS>\n"
		+	"</RECORD>",
	
	};
	
	private static String DTAR020_XML_3
		= "<?xml version=\"1.0\" ?>\n"
		+	"<RECORD RECORDNAME=\"DTAR020\"  FILESTRUCTURE=\"Default\" RECORDTYPE=\"RecordLayout\">\n"
		+	"	<FIELDS>\n"
		+	"		<FIELD NAME=\"KEYCODE-NO\"  LENGTH=\"8\" TYPE=\"Char\" />\n"
		+	"		<FIELD NAME=\"STORE-NO\"    LENGTH=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"DATE\"        LENGTH=\"4\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"DEPT-NO\"     LENGTH=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"QTY-SOLD\"    LENGTH=\"5\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"		<FIELD NAME=\"SALE-PRICE\"  LENGTH=\"6\" DECIMAL=\"2\" TYPE=\"Mainframe Packed Decimal (comp-3)\" />\n"
		+	"	</FIELDS>\n"
		+	"</RECORD>";

	private static final String[] CSV_XML = {
			  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<RECORD RECORDNAME=\"CsvTst\" DELIMITER=\"comma\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"Delimited\" QUOTE=\"\" >\n"
			+ "	<FIELDS>\n"
			+ "		<FIELD NAME=\"f1\" POSITION=\"1\" TYPE=\"Char\"/>\n"
			+ "		<FIELD NAME=\"f2\" POSITION=\"2\" TYPE=\"Char\"/>\n"
			+ "		<FIELD NAME=\"f3\" POSITION=\"3\" TYPE=\"Char\"/>\n"
			+ "		<FIELD NAME=\"f4\" POSITION=\"4\" TYPE=\"Char\"/>\n"
			+ "		<FIELD NAME=\"f5\" POSITION=\"5\" TYPE=\"Char\"/>\n"
			+ "	</FIELDS>\n"
			+ "</RECORD>",
			
			  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<RECORD RECORDNAME=\"CsvTst\" DELIMITER=\"comma\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"Delimited\" QUOTE=\"\" >\n"
			+ "	<FIELDS>\n"
			+ "		<FIELD NAME=\"f1\" TYPE=\"Char\"/>\n"
			+ "		<FIELD NAME=\"f2\" TYPE=\"Char\"/>\n"
			+ "		<FIELD NAME=\"f3\" TYPE=\"Char\"/>\n"
			+ "		<FIELD NAME=\"f4\" TYPE=\"Char\"/>\n"
			+ "		<FIELD NAME=\"f5\" TYPE=\"Char\"/>\n"
			+ "	</FIELDS>\n"
			+ "</RECORD>",
			
			  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<RECORD RECORDNAME=\"CsvTst\" DELIMITER=\"comma\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"Delimited\" QUOTE=\"\" >\n"
			+ "	<FIELDS>\n"
			+ "		<FIELD NAME=\"f1\" />\n"
			+ "		<FIELD NAME=\"f2\" />\n"
			+ "		<FIELD NAME=\"f3\" />\n"
			+ "		<FIELD NAME=\"f4\" />\n"
			+ "		<FIELD NAME=\"f5\" />\n"
			+ "	</FIELDS>\n"
			+ "</RECORD>",
	};

	/**
	 * Checking a record where every position has a field
	 */
	@SuppressWarnings("deprecation")
	private static FieldDetail[] DTAR020_RESULTS_1 = {
		CommonCodeFields.createField("KEYCODE-NO", 1, 8, 0, Type.ftChar),
		CommonCodeFields.createField("STORE-NO", 9, 2, 0, Type.ftPackedDecimalSmall),
		CommonCodeFields.createField("DATE", 11, 4, 0, Type.ftPackedDecimalSmall),
		CommonCodeFields.createField("DEPT-NO", 15, 2, 0, Type.ftPackedDecimalSmall),
		CommonCodeFields.createField("QTY-SOLD", 17, 5, 0, Type.ftPackedDecimalSmall),
		CommonCodeFields.createField("SALE-PRICE", 22, 6, 2, Type.ftPackedDecimalSmall),
	};
	
	/**
	 * Checking fields where ther is a break in the record 
	 */
	@SuppressWarnings("deprecation")
	private static FieldDetail[] DTAR020_RESULTS_2 =  {
			CommonCodeFields.createField("KEYCODE-NO", 1, 8, 0, Type.ftChar),
			CommonCodeFields.createField("STORE-NO", 9, 2, 0, Type.ftPackedDecimalSmall),
			CommonCodeFields.createField("DATE", 11, 4, 0, Type.ftPackedDecimalSmall),
			CommonCodeFields.createField("QTY-SOLD", 17, 5, 0, Type.ftPackedDecimalSmall),
			CommonCodeFields.createField("SALE-PRICE", 22, 6, 2, Type.ftPackedDecimalSmall),
		};
	
	
	private static FieldDetail[] CSV_RESULTS_1 = {
			CommonCodeFields.createCsvField("f1", 1, 0, Type.ftChar),
			CommonCodeFields.createCsvField("f2", 2, 0, Type.ftChar),
			CommonCodeFields.createCsvField("f3", 3, 0, Type.ftChar),
			CommonCodeFields.createCsvField("f4", 4, 0, Type.ftChar),
			CommonCodeFields.createCsvField("f5", 5, 0, Type.ftChar),
		};

/* -----------------------------------------------------------------------------------------------
 *    Tests Code starts here 
 * -----------------------------------------------------------------------------------------------
 */
	public void testPosLengthCalculation01() throws IOException {
		int i =0;
		for (String cpy : DTAR020_XML_1) {
			LayoutDetail l = JRecordInterface1.SCHEMA_XML
					.newIOBuilder(new ByteArrayInputStream(cpy.getBytes()), "DTAR020")
					.getLayout();
			
			CommonCodeFields.checkFields(ids[i++], "CP037", DTAR020_RESULTS_1, l.getRecord(0));
		}
	}
	

	public void testPosLengthCalculation02() throws IOException {
		int i =0;
		for (String cpy : DTAR020_XML_2) {
			LayoutDetail l = JRecordInterface1.SCHEMA_XML
					.newIOBuilder(new ByteArrayInputStream(cpy.getBytes()), "DTAR020")
					.getLayout();
			
			CommonCodeFields.checkFields(ids[i++], "CP037", DTAR020_RESULTS_2, l.getRecord(0));
		}
	}

	public void testPosLengthCalculation02b() throws IOException {
		int i =0;
		for (String cpy : DTAR020_XML_2) {
			LayoutDetail l = JRecordInterface1.SCHEMA_XML
					.newIOBuilder(new ByteArrayInputStream(cpy.getBytes()), "DTAR020")
					.setDefaultFont("cp1252")
					.getLayout();
			
			CommonCodeFields.checkFields(ids[i], "CP037", DTAR020_RESULTS_2, l.getRecord(0));
			
			l = JRecordInterface1.SCHEMA_XML
					.newIOBuilder(new ByteArrayInputStream(cpy.getBytes()), "DTAR020")
					.setFont("cp1252")
					.getLayout();
			
			CommonCodeFields.checkFields(ids[i++], "cp1252", DTAR020_RESULTS_2, l.getRecord(0));

		}
	}

	public void testPosLengthCalculation02c() throws IOException {
		int i =0;
		RecordEditorXmlLoader loader = new RecordEditorXmlLoader();
		for (String cpy : DTAR020_XML_2) {
			LayoutDetail l
				= loader.loadCopyBook(new ByteArrayInputStream(cpy.getBytes()), "DTAR020", 0, 0, "cp1252", 0, 0, 0, null)
						.asLayoutDetail();
			
			CommonCodeFields.checkFields(ids[i++], "CP037", DTAR020_RESULTS_2, l.getRecord(0));
		}
	}
	
	public void testDefaultCharSet1() throws IOException {
		LayoutDetail l = JRecordInterface1.SCHEMA_XML
				.newIOBuilder(new ByteArrayInputStream(DTAR020_XML_3.getBytes()), "DTAR020")
				.setFont("CP037")
				.getLayout();

		CommonCodeFields.checkFields("Length - default Charset", "CP037", DTAR020_RESULTS_1, l.getRecord(0));
		
	}
	
	
	public void testDefaultCharSet2() throws IOException {
		RecordEditorXmlLoader loader = new RecordEditorXmlLoader();
		
		LayoutDetail l
			= loader.loadCopyBook(new ByteArrayInputStream(DTAR020_XML_3.getBytes()), "DTAR020", 0, 0, "CP037", 0, 0, 0, null)
					.asLayoutDetail();

		CommonCodeFields.checkFields("Length - default Charset", "CP037", DTAR020_RESULTS_1, l.getRecord(0));
		
	}


	public void testPosCalculationCsv01() throws IOException {
		int i =0;
		for (String cpy : CSV_XML) {
			LayoutDetail l = JRecordInterface1.SCHEMA_XML
					.newIOBuilder(new ByteArrayInputStream(cpy.getBytes()), "")
					.getLayout();
			
			CommonCodeFields.checkCsvFields(ids[i++], Conversion.getDefaultSingleByteCharacterset(), CSV_RESULTS_1, l.getRecord(0));
		}
	}


}
