package net.sf.JRecord.zTest.exportAsXml;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.Comparator;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.Types.TypeManager.CharsetType;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.def.IO.builders.IFileIOBuilder;
import net.sf.JRecord.zTest.Common.TstConstants;


/**
 * This class tests<uL>
 *   <li>Reading a Multi-Record Cobol Copybook
 *   <li>Saving it as Xml
 *   <li>Reading the Xml
 *   <li>Checking the results are Ok
 * </ul>
 * 
 * @author bruce
 *
 */
public class TstMultiRecordCopybooks extends TestCase {

	String xmlRedef
	= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<RECORD RECORDNAME=\"MultiGrp-Redef\" COPYBOOK=\"MultiGrp-Redef\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"Mainframe_VB\" STYLE=\"0\" RECORDTYPE=\"GroupOfBinaryRecords\" LIST=\"Y\" INITSPACES=\"Y\" QUOTE=\"\" RecSep=\"default\">\n"
			+ "    <RECORDS>\n"
			+ "        <RECORD RECORDNAME=\"Group-1\" COPYBOOK=\"Group-1\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
			+ "            <ITEMS CopybookPref=\"MultiGrp-Redef\" JRecNaming=\"TRUE\">\n"
			+ "                <item level=\"01\" name=\"Parent\" position=\"1\" storage-length=\"30\" display-length=\"30\">\n"
			+ "                    <item level=\"05\" name=\"Record-Type\" picture=\"x\" position=\"1\" storage-length=\"1\" display-length=\"1\"/>\n"
			+ "                    <item level=\"05\" name=\"Group-1\" position=\"2\" storage-length=\"24\" display-length=\"24\" redefined=\"true\">\n"
			+ "                        <item level=\"10\" name=\"Field-11\" picture=\"s99\" position=\"2\" storage-length=\"2\" display-length=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "                        <item level=\"10\" name=\"Field-12\" picture=\"s99\" position=\"4\" storage-length=\"2\" display-length=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "                        <item level=\"10\" name=\"Field-13\" picture=\"x(20)\" position=\"6\" storage-length=\"20\" display-length=\"20\"/>\n"
			+ "                    </item>\n"
			+ "                </item>\n"
			+ "            </ITEMS>\n"
			+ "        </RECORD>\n"
			+ "        <RECORD RECORDNAME=\"Group-2\" COPYBOOK=\"Group-2\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
			+ "            <ITEMS CopybookPref=\"MultiGrp-Redef\" JRecNaming=\"TRUE\">\n"
			+ "                <item level=\"01\" name=\"Parent\" position=\"1\" storage-length=\"30\" display-length=\"30\">\n"
			+ "                    <item level=\"05\" name=\"Record-Type\" picture=\"x\" position=\"1\" storage-length=\"1\" display-length=\"1\"/>\n"
			+ "                    <item level=\"05\" name=\"Group-2\" position=\"2\" storage-length=\"23\" display-length=\"23\" redefines=\"Group-1\">\n"
			+ "                        <item level=\"10\" name=\"Field-21\" picture=\"s999\" position=\"2\" storage-length=\"3\" display-length=\"3\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "                        <item level=\"10\" name=\"Filler\" position=\"5\" storage-length=\"20\" display-length=\"20\">\n"
			+ "                            <item level=\"15\" name=\"Field-22\" picture=\"x(20)\" position=\"5\" storage-length=\"20\" display-length=\"20\"/>\n"
			+ "                        </item>\n"
			+ "                    </item>\n"
			+ "                </item>\n"
			+ "            </ITEMS>\n"
			+ "        </RECORD>\n"
			+ "        <RECORD RECORDNAME=\"Group-3\" COPYBOOK=\"Group-3\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
			+ "            <ITEMS CopybookPref=\"MultiGrp-Redef\" JRecNaming=\"TRUE\">\n"
			+ "                <item level=\"01\" name=\"Parent\" position=\"1\" storage-length=\"30\" display-length=\"30\">\n"
			+ "                    <item level=\"05\" name=\"Record-Type\" picture=\"x\" position=\"1\" storage-length=\"1\" display-length=\"1\"/>\n"
			+ "                    <item level=\"05\" name=\"Group-3\" position=\"2\" storage-length=\"29\" display-length=\"29\" redefines=\"Group-1\">\n"
			+ "                        <item level=\"10\" name=\"Field-31\" picture=\"s9999\" position=\"2\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "                        <item level=\"10\" name=\"Group-31\" position=\"6\" storage-length=\"25\" display-length=\"25\">\n"
			+ "                            <item level=\"15\" name=\"Field-32\" picture=\"x(25)\" position=\"6\" storage-length=\"25\" display-length=\"25\"/>\n"
			+ "                        </item>\n"
			+ "                    </item>\n"
			+ "                </item>\n"
			+ "            </ITEMS>\n"
			+ "        </RECORD>\n"
			+ "    </RECORDS>\n"
			+ "</RECORD>";
	
	FieldDetail[][] bitOfEverythingFields = {{
			createField("NumA", ".CompFields.", 1, 25, 2, 6),
			createField("NumB", ".CompFields.", 26, 3, 2, 22),
			createField("NumC", ".CompFields.", 29, 3, 0, 25),
			createField("text", ".CompFields.", 32, 20, 0, 0),
			createField("NumD", ".CompFields.", 52, 3, 6, 0),
			createField("NumE", ".CompFields.", 55, 3, -3, 0),
			createField("float", ".CompFields.", 58, 4, 0, 17),
			createField("double", ".CompFields.", 62, 8, 0, 18),
			createField("RBI-NUMBER-S96SLS (0)", ".CompFields..RBI-REPETITIVE-AREA.RBI-REPEAT.", 70, 7, 0, 9),
			createField("RBI-NUMBER-S96DISP (0)", ".CompFields..RBI-REPETITIVE-AREA.RBI-REPEAT.", 77, 6, 0, 32),
			createField("SFIELD-SEP", ".CompFields..", 83, 10, 2, 9),
			createField("REN-RETURNED-YEAR", ".CompFields.88-levels.REN-RETURNED-DATE.", 93, 2, 0, 25),
			createField("REN-RETURNED-MONTH", ".CompFields.88-levels.REN-RETURNED-DATE.", 95, 2, 0, 25),
			createField("REN-RETURNED-DAY", ".CompFields.88-levels.REN-RETURNED-DATE.", 97, 2, 0, 25),
			createField("REN-CAR-TYPE", ".CompFields.88-levels.", 99, 1, 0, 0),
			createField("REN-DAYS-RENTED", ".CompFields.88-levels.", 100, 2, 0, 25),
			createField("Store-Num", ".CompFields.occurs-items.", 102, 4, 0, 25),
			createField("Store-Name", ".CompFields.occurs-items.", 106, 30, 0, 0),
			createField("Department-Num (0)", ".CompFields.occurs-items.Department-Dtls.", 136, 4, 0, 25),
			createField("Department-name (0)", ".CompFields.occurs-items.Department-Dtls.", 140, 20, 0, 0),
			createField("keycode (0, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.", 160, 8, 0, 25),
			createField("Qty (0, 0, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 168, 6, 0, 6),
			createField("Price (0, 0, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 174, 9, 2, 6),
			createField("trans-type (0, 0, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 183, 1, 0, 0),
			createField("keycode (0, 1)", ".CompFields.occurs-items.Department-Dtls.Product-details.", 184, 8, 0, 25),
			createField("Qty (0, 1, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 192, 6, 0, 6),
			createField("Price (0, 1, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 198, 9, 2, 6),
			createField("trans-type (0, 1, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 207, 1, 0, 0),
			createField("Qty (0)", ".CompFields.occurs-items.Department-Dtls.Summary.", 208, 6, 0, 6),
			createField("Price (0)", ".CompFields.occurs-items.Department-Dtls.Summary.", 214, 9, 2, 6),
			createField("Sku-Count (0)", ".CompFields.occurs-items.Department-Dtls.Summary.", 223, 6, 0, 6),
			createField("Department-Num (1)", ".CompFields.occurs-items.Department-Dtls.", 229, 4, 0, 25),
			createField("Department-name (1)", ".CompFields.occurs-items.Department-Dtls.", 233, 20, 0, 0),
			createField("keycode (1, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.", 253, 8, 0, 25),
			createField("Qty (1, 0, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 261, 6, 0, 6),
			createField("Price (1, 0, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 267, 9, 2, 6),
			createField("trans-type (1, 0, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 276, 1, 0, 0),
			createField("keycode (1, 1)", ".CompFields.occurs-items.Department-Dtls.Product-details.", 277, 8, 0, 25),
			createField("Qty (1, 1, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 285, 6, 0, 6),
			createField("Price (1, 1, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 291, 9, 2, 6),
			createField("trans-type (1, 1, 0)", ".CompFields.occurs-items.Department-Dtls.Product-details.A-Sale.", 300, 1, 0, 0),
			createField("Qty (1)", ".CompFields.occurs-items.Department-Dtls.Summary.", 301, 6, 0, 6),
			createField("Price (1)", ".CompFields.occurs-items.Department-Dtls.Summary.", 307, 9, 2, 6),
			createField("Sku-Count (1)", ".CompFields.occurs-items.Department-Dtls.Summary.", 316, 6, 0, 6),
			createField("keycode (0)", ".CompFields.occurs-items.Orders.", 322, 8, 0, 25),
			createField("Qty (0)", ".CompFields.occurs-items.Orders.", 330, 6, 0, 6),
			createField("keycode (1)", ".CompFields.occurs-items.Orders.", 336, 8, 0, 25),
			createField("Qty (1)", ".CompFields.occurs-items.Orders.", 344, 6, 0, 6),
			createField("array-fld (0)", ".CompFields.occurs-items.", 350, 2, 0, 0),
			createField("array-fld (1)", ".CompFields.occurs-items.", 352, 2, 0, 0),
			createField("array-fld (2)", ".CompFields.occurs-items.", 354, 2, 0, 0),
			createField("data-field", ".CompFields.occurs-items.", 356, 10, 0, 0),
			createField("rc (0)", ".CompFields.occurs-items.array-redef.", 356, 2, 0, 25),
			createField("rc1 (0)", ".CompFields.occurs-items..", 356, 2, 0, 25),
			createField("rc2 (0)", ".CompFields.occurs-items...", 356, 2, 0, 25),
			createField("rc3 (0)", ".CompFields.occurs-items...", 356, 2, 0, 25),
			createField("rc (1)", ".CompFields.occurs-items.array-redef.", 358, 2, 0, 25),
			createField("rc1 (1)", ".CompFields.occurs-items..", 358, 2, 0, 25),
			createField("rc2 (1)", ".CompFields.occurs-items...", 358, 2, 0, 25),
			createField("sep0", ".CompFields.Signed-Comp.", 366, 1, 0, 0),
			createField("Num0", ".CompFields.Signed-Comp.", 367, 2, 0, 35),
			createField("sep1", ".CompFields.Signed-Comp.", 369, 1, 0, 0),
			createField("Num1", ".CompFields.Signed-Comp.", 370, 2, 2, 35),
			createField("sep2", ".CompFields.Signed-Comp.", 372, 1, 0, 0),
			createField("Num2", ".CompFields.Signed-Comp.", 373, 2, 2, 35),
			createField("sep0", ".CompFields.UnSigned-Comp.aa.bb.cc.dd.", 375, 1, 0, 0),
			createField("Num0", ".CompFields.UnSigned-Comp.aa.bb.cc.dd.", 376, 2, 0, Type.ftPositiveBinaryBigEndian),
			createField("sep1", ".CompFields.UnSigned-Comp.aa.bb.cc.dd.", 378, 1, 0, 0),
			createField("Num1", ".CompFields.UnSigned-Comp.aa.bb.cc.dd.", 379, 2, 2, Type.ftPositiveBinaryBigEndian),
			createField("sep2", ".CompFields.UnSigned-Comp.aa.bb.cc.dd.", 381, 1, 0, 0),
			createField("Num2", ".CompFields.UnSigned-Comp.aa.bb.cc.dd.", 382, 2, 2, Type.ftPositiveBinaryBigEndian),
			createField("sep0", ".CompFields.Signed-Comp-3.aa.", 384, 1, 0, 0),
			createField("Num0", ".CompFields.Signed-Comp-3.aa.", 385, 2, 0, 31),
			createField("sep1", ".CompFields.Signed-Comp-3.aa.", 387, 1, 0, 0),
			createField("Num1", ".CompFields.Signed-Comp-3.aa.", 388, 2, 2, 31),
			createField("sep0", ".CompFields.UnSigned-Comp-3.aa.", 390, 1, 0, 0),
			createField("Num0", ".CompFields.UnSigned-Comp-3.aa.", 391, 2, 0, 33),
			createField("sep1", ".CompFields.UnSigned-Comp-3.aa.", 393, 1, 0, 0),
			createField("Num1", ".CompFields.UnSigned-Comp-3.aa.", 394, 3, 2, 33),
			createField("sep0", ".CompFields.Signed-Comp-sync.", 397, 1, 0, 0),
			createField("Num0", ".CompFields.Signed-Comp-sync.", 399, 2, 0, 35),
			createField("sep1", ".CompFields.Signed-Comp-sync.", 401, 1, 0, 0),
			createField("Num1", ".CompFields.Signed-Comp-sync.", 403, 2, 2, 35),
			createField("sep0", ".CompFields.G-Comp-4.aa.", 405, 1, 0, 0),
			createField("Num0", ".CompFields.G-Comp-4.aa.", 406, 2, 0, 35),
			createField("sep1", ".CompFields.G-Comp-4.aa.", 408, 1, 0, 0),
			createField("Num1", ".CompFields.G-Comp-4.aa.", 409, 2, 2, 35),
			createField("Num2", ".CompFields.G-Comp-4.aa.", 411, 2, 2, Type.ftPositiveBinaryBigEndian),
			createField("sep0", ".CompFields.G-Comp-5.aa.", 413, 1, 0, 0),
			createField("Num0", ".CompFields.G-Comp-5.aa.", 414, 2, 0, 35),
			createField("sep1", ".CompFields.G-Comp-5.aa.", 416, 1, 0, 0),
			createField("Num1", ".CompFields.G-Comp-5.aa.", 417, 2, 2, 35),
			createField("Num2", ".CompFields.G-Comp-5.aa.", 419, 2, 2, Type.ftPositiveBinaryBigEndian),
			createField("PIC-TEST-1", ".CompFields.some-more-items.TOP-LEVEL-ITEM.", 421, 1, 0, 0),
			createField("PIC-TEST-2", ".CompFields.some-more-items.TOP-LEVEL-ITEM.", 422, 2, 0, 0),
			createField("PIC-TEST-3", ".CompFields.some-more-items.TOP-LEVEL-ITEM.", 424, 3, 0, 0),
			createField("PIC-TEST-4", ".CompFields.some-more-items.TOP-LEVEL-ITEM.", 427, 1, 0, 32),
			createField("PIC-TEST-5", ".CompFields.some-more-items.TOP-LEVEL-ITEM.", 428, 3, 1, 6),
			createField("PIC-TEST-6", ".CompFields.some-more-items.TOP-LEVEL-ITEM.", 431, 3, 2, 32),
			createField("PIC-TEST-7", ".CompFields.some-more-items.TOP-LEVEL-ITEM.", 434, 8, 0, 0),
			createField("VALUE-TEST-1", ".CompFields.some-more-items.TOP-LEVEL-ITEM.VALUE-TEST-GROUP.", 442, 3, 0, 0),
			createField("VALUE-TEST-2", ".CompFields.some-more-items.TOP-LEVEL-ITEM.VALUE-TEST-GROUP.", 445, 3, 0, 25),
			createField("VALUE-TEST-3", ".CompFields.some-more-items.TOP-LEVEL-ITEM.VALUE-TEST-GROUP.", 448, 3, 0, 0),
			createField("VALUE-TEST-4", ".CompFields.some-more-items.TOP-LEVEL-ITEM.VALUE-TEST-GROUP.", 451, 3, 0, 25),
			createField("VALUE-TEST-5", ".CompFields.some-more-items.TOP-LEVEL-ITEM.VALUE-TEST-GROUP.", 454, 5, 0, 0),
			createField("DEPENDING-TEST-1", ".CompFields.some-more-items.TOP-LEVEL-ITEM.DEPENDING-ON-TEST-GROUP.", 459, 1, 0, 25),
			createField("DEPENDING-TEST-2 (0)", ".CompFields.some-more-items.TOP-LEVEL-ITEM.DEPENDING-ON-TEST-GROUP.", 460, 1, 0, 0),
			createField("DEPENDING-TEST-2 (1)", ".CompFields.some-more-items.TOP-LEVEL-ITEM.DEPENDING-ON-TEST-GROUP.", 461, 1, 0, 0),
			createField("DEPENDING-TEST-2 (2)", ".CompFields.some-more-items.TOP-LEVEL-ITEM.DEPENDING-ON-TEST-GROUP.", 462, 1, 0, 0),
			createField("GROUP-WITH-88-LEVEL-ITEM-1", ".CompFields.some-more-items.TOP-LEVEL-ITEM.GROUP-WITH-88-LEVEL.", 463, 2, 0, 0),
			createField("GROUP-WITH-88-LEVEL-ITEM-2", ".CompFields.some-more-items.TOP-LEVEL-ITEM.GROUP-WITH-88-LEVEL.", 465, 3, 0, 0),
			createField("LOWERCASE-KEYWORDS", ".CompFields.some-more-items.TOP-LEVEL-ITEM.GROUP-WITH-LOWERCASE.", 468, 1, 0, 25),
			createField("lowercase-identifier", ".CompFields.some-more-items.TOP-LEVEL-ITEM.GROUP-WITH-LOWERCASE.", 469, 1, 0, 0),
			createField("MiXeDcAsE-TEST", ".CompFields.some-more-items.TOP-LEVEL-ITEM.GROUP-WITH-LOWERCASE.", 470, 3, 0, 0),
			createField("LOWERCASE-88-VALUE", ".CompFields.some-more-items.TOP-LEVEL-ITEM.GROUP-WITH-LOWERCASE.", 473, 5, 0, 0),
			createField("UNCOMMENTED-1", ".CompFields.some-more-items.TOP-LEVEL-ITEM.INDICATOR-COLUMN-SLASH-TEST-GROUP.", 478, 1, 0, 0),
			createField("UNCOMMENTED-2", ".CompFields.some-more-items.TOP-LEVEL-ITEM.INDICATOR-COLUMN-SLASH-TEST-GROUP.", 479, 1, 0, 0),
			createField("PIC-TEST-99", ".CompFields.some-more-items.ANOTHER-TOP-LEVEL-ITEM.", 480, 1, 0, 0),
			createField("PIC-TEST-98", ".CompFields.some-more-items.ANOTHER-TOP-LEVEL-ITEM.", 481, 1, 0, 25),
	}, {
			createField("NumA", ".Fields-2.", 1, 25, 2, 6),
			createField("NumB", ".Fields-2.", 26, 3, 2, 22),
			createField("NumC", ".Fields-2.", 29, 3, 0, 25),
			createField("text", ".Fields-2.", 32, 20, 0, 0),
			createField("NumD", ".Fields-2.", 52, 3, 6, 0),
			createField("NumE", ".Fields-2.", 55, 3, -3, 0),
			createField("float", ".Fields-2.", 58, 4, 0, 17),
			createField("double", ".Fields-2.", 62, 8, 0, 18),
			createField("RBI-NUMBER-S96SLS (0)", ".Fields-2..RBI-REPETITIVE-AREA.RBI-REPEAT.", 70, 7, 0, 9),
			createField("RBI-NUMBER-S96DISP (0)", ".Fields-2..RBI-REPETITIVE-AREA.RBI-REPEAT.", 77, 6, 0, 32),
			createField("SFIELD-SEP", ".Fields-2..", 83, 10, 2, 9),
			createField("REN-RETURNED-YEAR", ".Fields-2.88-levels.REN-RETURNED-DATE.", 93, 2, 0, 25),
			createField("REN-RETURNED-MONTH", ".Fields-2.88-levels.REN-RETURNED-DATE.", 95, 2, 0, 25),
			createField("REN-RETURNED-DAY", ".Fields-2.88-levels.REN-RETURNED-DATE.", 97, 2, 0, 25),
			createField("REN-CAR-TYPE", ".Fields-2.88-levels.", 99, 1, 0, 0),
			createField("REN-DAYS-RENTED", ".Fields-2.88-levels.", 100, 2, 0, 25),
	}, {
			createField("NumA", ".CompFields-3.", 1, 25, 2, 6),
			createField("NumB", ".CompFields-3.", 26, 3, 2, 22),
			createField("NumC", ".CompFields-3.", 29, 3, 0, 25),
			createField("text", ".CompFields-3.", 32, 20, 0, 0),
			createField("NumD", ".CompFields-3.", 52, 3, 6, 0),
			createField("NumE", ".CompFields-3.", 55, 3, -3, 0),
			createField("float", ".CompFields-3.", 58, 4, 0, 17),
			createField("double", ".CompFields-3.", 62, 8, 0, 18),
			createField("RBI-NUMBER-S96SLS (0)", ".CompFields-3..RBI-REPETITIVE-AREA.RBI-REPEAT.", 70, 7, 0, 9),
			createField("RBI-NUMBER-S96DISP (0)", ".CompFields-3..RBI-REPETITIVE-AREA.RBI-REPEAT.", 77, 6, 0, 32),
			createField("SFIELD-SEP", ".CompFields-3..", 83, 10, 2, 9),
			createField("REN-RETURNED-YEAR", ".CompFields-3.88-levels.REN-RETURNED-DATE.", 93, 2, 0, 25),
			createField("REN-RETURNED-MONTH", ".CompFields-3.88-levels.REN-RETURNED-DATE.", 95, 2, 0, 25),
			createField("REN-RETURNED-DAY", ".CompFields-3.88-levels.REN-RETURNED-DATE.", 97, 2, 0, 25),
			createField("REN-CAR-TYPE", ".CompFields-3.88-levels.", 99, 1, 0, 0),
			createField("REN-DAYS-RENTED", ".CompFields-3.88-levels.", 100, 2, 0, 25),
	}};
	
	FieldDetail[][] expectedGrp01 = getFieldDtl(null, false);
	FieldDetail[][] empty = {};
		

	CopybookDetails[] cpyBooks = {
			new CopybookDetails(
					"MultiGrp-01-level", CopybookLoader.SPLIT_01_LEVEL, 
					getStdXml("01", "MultiGrp-01-level", null), expectedGrp01),
			new CopybookDetails(
					"MultiGrp-05-level", CopybookLoader.SPLIT_TOP_LEVEL, 
					getStdXml("05", "MultiGrp-05-level", null), expectedGrp01),
			new CopybookDetails(
					"MultiGrp-07-HR", CopybookLoader.SPLIT_HIGHEST_REPEATING, 
					getStdXml("07", "MultiGrp-07-HR", "Top-Level.1st-level"), getFieldDtl("Top-Level.1st-level", false)),
			new CopybookDetails(
					"MultiGrp-Redef", CopybookLoader.SPLIT_REDEFINE, 
					xmlRedef, getFieldDtl("Parent", true)),
			new CopybookDetails("BitOfEverything01", CopybookLoader.SPLIT_01_LEVEL, null, null),
			new CopybookDetails("BitOfEverything02", CopybookLoader.SPLIT_01_LEVEL, null, bitOfEverythingFields) ,
	};
	
	//MultiGrp-07-HR.cbl

	public void testCopybookFields() throws IOException, XMLStreamException, FactoryConfigurationError {
		int fs = Constants.IO_VB;
		for (int i = 0; i < cpyBooks.length; i++) {
			CopybookDetails cpy = cpyBooks[i];
			System.out.println("\n\t" + cpy.name);
			ICobolIOBuilder iob1 = JRecordInterface1.COBOL
					.newIOBuilder(TstConstants.COBOL_DIRECTORY + cpy.name  + ".cbl")
						.setFileOrganization(Constants.IO_VB)
						.setDialect(ICopybookDialects.FMT_MAINFRAME)
						.setSplitCopybook(cpy.splitCode);
			
			checkCobolIoBuilder(cpy.name, cpy.dropCond, iob1, fs, cpy.xml);
			
			checkLayoutFields(cpy.expectedFields, iob1, cpy.name);
		}
	}
	
	/**
	 * @param cpy
	 * @param hasConds
	 * @param iob1
	 * @throws XMLStreamException
	 * @throws UnsupportedEncodingException
	 * @throws FactoryConfigurationError
	 * @throws IOException
	 */
	private void checkCobolIoBuilder(String cpy, boolean hasConds, ICobolIOBuilder iob1, int fs, String xmlstr)
			throws XMLStreamException, UnsupportedEncodingException, FactoryConfigurationError, IOException {
		StringWriter sw1 = new StringWriter(1000);
		StringWriter sw2 = new StringWriter(1000);
		
		JRecordInterface1.SCHEMA_XML
				.setIndentXml(! hasConds)
				.export(sw1, iob1.getExternalRecord());
		
		String xmlString1 = sw1.toString();
		IFileIOBuilder iob2 = JRecordInterface1.SCHEMA_XML.newIOBuilder(new StringReader(xmlString1), cpy);
		
		JRecordInterface1.SCHEMA_XML
				.setIndentXml(! hasConds)
				.export(sw2, iob2.getExternalRecord());
		String xmlString2 = sw2.toString();
		
		if (hasConds) {
			xmlString1 = TstCobolSingeRecord.dropCondition(xmlString1);
		}
		assertEquals(xmlString1, xmlString2);

		//System.out.print(xmlString2);
		compare(cpy, iob1, iob2, fs);
		
		if (xmlstr == null) {
			
		} else if (xmlstr.length() == 0) {
			System.out.println("\n\t" + cpy + ":\n");
			System.out.println(xmlString1);
		} else {
			assertEquals(cpy, xmlstr, xmlString1);
		}
	}

	/**
	 * Compare the field details on 2 iobs
	 * @param iob1
	 * @return
	 * @throws IOException
	 */
	private void compare(String cpy, ICobolIOBuilder iob1, IFileIOBuilder iob2, int fs) throws IOException {
		ExternalRecord rec1 = iob1	.getExternalRecord();
		ExternalRecord rec2 = iob2	.getExternalRecord();

		LayoutDetail schema1 = rec1	.asLayoutDetail();
		LayoutDetail schema2 = rec2	.asLayoutDetail();
		
		assertEquals(fs, rec1.getFileStructure());
		assertEquals(fs, rec2.getFileStructure());
		assertEquals(fs, schema1.getFileStructure());
		assertEquals(fs, schema2.getFileStructure());
		
		int size = rec1.getNumberOfRecords();
//		ExternalField[] xFields1 = rec1.getRecordFields();
//		ExternalField[] xFields2 = rec2.getRecordFields();
//		FieldDetail[] lFields = new FieldDetail[size];
		
		assertEquals(size, rec2.getNumberOfRecords());
		for (int rIdx = 0; rIdx < size; rIdx++) {
			ExternalField[] xFields1 = rec1.getRecord(rIdx).getRecordFields();
			ExternalField[] xFields2 = rec2.getRecord(rIdx).getRecordFields();
			int size1 = schema1.getRecord(rIdx).getFieldCount();
			assertEquals(size1, schema2.getRecord(rIdx).getFieldCount());
			for (int i = 0; i < size1; i++) {
				assertEquals(xFields1[i].getDecimal(), xFields2[i].getDecimal());
				assertEquals(xFields1[i].getGroup(),   xFields2[i].getGroup());
				assertEquals(xFields1[i].getLen(),     xFields2[i].getLen());
				assertEquals(xFields1[i].getName(),    xFields2[i].getName());
				assertEquals(xFields1[i].getPos(),     xFields2[i].getPos());
				assertEquals(xFields1[i].getType(),    xFields2[i].getType());  
				
				if (xFields1[i].getDependOnDtls() != null) {
					assertEquals(xFields1[i].getDependOnDtls().firstIdx, xFields2[i].getDependOnDtls().firstIdx);
					assertEquals(xFields1[i].getDependOnDtls().index,    xFields2[i].getDependOnDtls().index);
	//				assertEquals(xFields1[i].getDependOnDtls().firstIdx, xFields2[i].getDependOnDtls().firstIdx);
				}
			}
			
			for (int i = 0; i < size1 ; i++) {
				FieldDetail field1 = schema1.getField(rIdx, i);
				FieldDetail field2 = schema2.getField(rIdx, i);
				
				assertEquals(field1.getFontName(),   field2.getFontName());
	
				compareLayoutFields(cpy, field1, field2);
			}
			
			Arrays.sort(xFields1, new Comparator<ExternalField>() {
				@Override public int compare(ExternalField o1, ExternalField o2) {
					return Integer.compare(o1.getPos(), o2.getPos());
				}
			});
			assertEquals(size1, rec1.getRecord(rIdx).getNumberOfRecordFields());
			
			TypeManager typeMgr = TypeManager.getInstance();
			CharsetType charsetType = typeMgr.getCharsetType(rec1.getFontName());
			
			for (int i = 0; i < size1; i++) {
				FieldDetail field1 = schema1.getField(rIdx, i);
				
				assertEquals(xFields1[i].getDecimal(), field1.getDecimal());
				assertEquals(xFields1[i].getName(),    field1.getName());
				if (! xFields1[i].getGroup().equals(field1.getGroupName())) {
					assertEquals(xFields1[i].getName() + " " + i, xFields1[i].getGroup(),   field1.getGroupName());
				}
				assertEquals(xFields1[i].getLen(),     field1.getLen());
				assertEquals(xFields1[i].getPos(),     field1.getPos());
				assertEquals(
						typeMgr.getShortType(xFields1[i].getType(), xFields1[i].getLen(), charsetType),    
						field1.getType());  
			}
		}
	}

	/**
	 * @param field1
	 * @param field2
	 */
	private void compareLayoutFields(String cpy, FieldDetail field1, FieldDetail field2) {
		assertEquals(cpy, field1.getLookupName(), field2.getLookupName());
		assertEquals(field1.getLookupName(), field1.getDecimal(),    field2.getDecimal());;
		assertEquals(field1.getEnd(),        field2.getEnd()    );
		assertEquals(cpy + ": " + field1.getName(),       field1.getGroupName(),  field2.getGroupName());
		assertEquals(field1.getLen(),        field2.getLen());
		assertEquals(field1.getName(),       field2.getName());
		assertEquals(field1.getPos(),        field2.getPos());
		assertEquals(
				TypeManager.getInstance().getShortType(field1.getType(), field1.getLen(), field2.getFontName()),       
				field2.getType());
		assertEquals(field1.getName(),       field2.getName());
	}
	
	
	/**
	 * @param expectedFields
	 * @param iob
	 * @throws IOException
	 */
	private void checkLayoutFields(FieldDetail[][] expectedFields, ICobolIOBuilder iob, String cpy) throws IOException {
		LayoutDetail layout = iob.getLayout();
		
		for (int rIdx = 0; rIdx < layout.getRecordCount(); rIdx++) {
			int size = layout.getRecord(rIdx).getFieldCount();
			
			if (expectedFields == null) {
			} else if (rIdx < expectedFields.length  && expectedFields[rIdx] != null) {
				assertEquals(cpy + " " + rIdx, expectedFields[rIdx].length, size);
				for (int i = 0; i < size; i++) {
					compareLayoutFields(cpy, expectedFields[rIdx][i], layout.getField(rIdx, i));
				}
			} else {
				System.out.println("\t}, {");
				for (int i = 0; i < size; i++) {
					FieldDetail f = layout.getField(rIdx, i);
					System.out.println(
							  "\t\t\tcreateField(\"" + f.getName() + "\", " 
										+ "\"" + f.getGroupName() + "\", "
										+ f.getPos() + ", "
										+ f.getLen() + ", "
										+ f.getDecimal() + ", "
										+ f.getType() + "),");
				}
			}
		}
	}

	
	private String getStdXml(String lvlStr, String cpyPrefString, String groupNames) {
		String grpName = "";
		if (groupNames != null) {
			grpName = " GROUPNAMES=\"" + groupNames + "\"";
		}

		return    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
				+ "<RECORD RECORDNAME=\"" + cpyPrefString + "\" COPYBOOK=\""
							+ cpyPrefString + "\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"Mainframe_VB\" STYLE=\"0\" RECORDTYPE=\"GroupOfBinaryRecords\" LIST=\"Y\" INITSPACES=\"Y\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "    <RECORDS>\n"
				+ "        <RECORD RECORDNAME=\"Group-1\" COPYBOOK=\"Group-1\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "            <ITEMS CopybookPref=\"" + cpyPrefString + "\" JRecNaming=\"TRUE\"" + grpName + ">\n"
				+ "                <item level=\"" + lvlStr + "\" name=\"Group-1\" position=\"1\" storage-length=\"25\" display-length=\"25\">\n"
				+ "                    <item level=\"10\" name=\"Record-Type\" picture=\"x\" position=\"1\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                    <item level=\"10\" name=\"Field-11\" picture=\"s99\" position=\"2\" storage-length=\"2\" display-length=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                    <item level=\"10\" name=\"Field-12\" picture=\"s99\" position=\"4\" storage-length=\"2\" display-length=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                    <item level=\"10\" name=\"Field-13\" picture=\"x(20)\" position=\"6\" storage-length=\"20\" display-length=\"20\"/>\n"
				+ "                </item>\n"
				+ "            </ITEMS>\n"
				+ "        </RECORD>\n"
				+ "        <RECORD RECORDNAME=\"Group-2\" COPYBOOK=\"Group-2\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "            <ITEMS CopybookPref=\"" + cpyPrefString + "\" JRecNaming=\"TRUE\"" + grpName + ">\n"
				+ "                <item level=\"" + lvlStr + "\" name=\"Group-2\" position=\"1\" storage-length=\"24\" display-length=\"24\">\n"
				+ "                    <item level=\"10\" name=\"Record-Type\" picture=\"x\" position=\"1\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                    <item level=\"10\" name=\"Field-21\" picture=\"s999\" position=\"2\" storage-length=\"3\" display-length=\"3\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                    <item level=\"10\" name=\"Filler\" position=\"5\" storage-length=\"20\" display-length=\"20\">\n"
				+ "                        <item level=\"15\" name=\"Field-22\" picture=\"x(20)\" position=\"5\" storage-length=\"20\" display-length=\"20\"/>\n"
				+ "                    </item>\n"
				+ "                </item>\n"
				+ "            </ITEMS>\n"
				+ "        </RECORD>\n"
				+ "        <RECORD RECORDNAME=\"Group-3\" COPYBOOK=\"Group-3\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "            <ITEMS CopybookPref=\"" + cpyPrefString + "\" JRecNaming=\"TRUE\"" + grpName + ">\n"
				+ "                <item level=\"" + lvlStr + "\" name=\"Group-3\" position=\"1\" storage-length=\"30\" display-length=\"30\">\n"
				+ "                    <item level=\"10\" name=\"Record-Type\" picture=\"x\" position=\"1\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                    <item level=\"10\" name=\"Field-31\" picture=\"s9999\" position=\"2\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                    <item level=\"10\" name=\"Group-31\" position=\"6\" storage-length=\"25\" display-length=\"25\">\n"
				+ "                        <item level=\"15\" name=\"Field-32\" picture=\"x(25)\" position=\"6\" storage-length=\"25\" display-length=\"25\"/>\n"
				+ "                    </item>\n"
				+ "                </item>\n"
				+ "            </ITEMS>\n"
				+ "        </RECORD>\n"
				+ "    </RECORDS>\n"
				+ "</RECORD>";
	}

	private FieldDetail[][] getFieldDtl(String groupLevels, boolean redef) {
		if (groupLevels == null) {
			groupLevels = "";
		} else {
			groupLevels = '.' + groupLevels;
		}
		
		String recTypeGrp1 = groupLevels + ".Group-1.";
		String recTypeGrp2 = groupLevels + ".Group-2.";
		String recTypeGrp3 = groupLevels + ".Group-3.";
		if (redef) {
			recTypeGrp1 = groupLevels + '.';
			recTypeGrp2 = recTypeGrp1;
			recTypeGrp3 = recTypeGrp1;
		}
		
		FieldDetail[][] xGrp01 = {
				{
					createField("Record-Type", recTypeGrp1, 1, 1, 0, 0),
					createField("Field-11", groupLevels + ".Group-1.", 2, 2, 0, 32),
					createField("Field-12", groupLevels + ".Group-1.", 4, 2, 0, 32),
					createField("Field-13", groupLevels + ".Group-1.", 6, 20, 0, 0),
				}, {
					createField("Record-Type", recTypeGrp2, 1, 1, 0, 0),
					createField("Field-21", groupLevels + ".Group-2.", 2, 3, 0, 32),
					createField("Field-22", groupLevels + ".Group-2..", 5, 20, 0, 0),
				}, {
					createField("Record-Type", recTypeGrp3, 1, 1, 0, 0),
					createField("Field-31", groupLevels + ".Group-3.", 2, 4, 0, 32),
					createField("Field-32", groupLevels + ".Group-3.Group-31.", 6, 25, 0, 0),
		}};
		return xGrp01;
	}
	
	private static FieldDetail createField(String name, String groupName, int pos, int len, int decimal, int type) {
		FieldDetail f = FieldDetail.newFixedWidthField(name, type, pos, len, decimal, "");
		f.setGroupName(groupName);
		
		return f;
	}
	

	private static class CopybookDetails {
		final String name, xml;
		final int splitCode;
		final FieldDetail[][] expectedFields;
		final boolean dropCond;
		
		public CopybookDetails(String name, int splitcode, String xml, FieldDetail[][] expectedFields) {
			super();
			this.name = name;
			this.xml = xml;
			this.splitCode = splitcode;
			this.expectedFields = expectedFields;
			this.dropCond = "BitOfEverything01".equalsIgnoreCase(name) 
						 || "BitOfEverything02".equalsIgnoreCase(name);
		}
	}

}
