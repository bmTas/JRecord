package net.sf.JRecord.zTest.Cobol;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import org.junit.jupiter.api.Test;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Types.Type;

class TestSplitRedefine2 {

	private static final String COPYBOOK_CODE = ""
			+ "    01  Rec-1.\n"
			+ "        03 h-1.\n"
			+ "           04 h1-1.\n"
			+ "              05 record-type             pic x.\n"
			+ "              05 A-1 occurs 3            pic s99. \n"
			+ "              05 h1-1-1.\n"
			+ "                 07 a-2  occurs 4.\n"
			+ "                    10  a-2-f1           pic s94v99.\n"
			+ "                    10  a-2-f2           pic x(5).\n"
			+ "              05 h-1-1-f1                pic x(10).\n"
			+ "           04 h1-2.\n"
			+ "              05 h1-2-f1                 pic xx.\n"
			+ "           04 h1-3.\n"
			+ "              05 h1-3-f1                 pic xx.\n"
			+ "              05 h1-3-a1   occurs 3      pic xx.\n"
			+ "              05 Details                 pic x(50).\n"
			+ "              05 details-type-a redefines Details.\n"
			+ "                 07 details-a-f1         pic x(11).\n"
			+ "                 07 details-a-g1.\n"
			+ "                    09 details-a-f2      pic x(12).\n"
			+ "                    09 details-a-g1 occurs 2.\n"
			+ "                       11 details-a-f3   pic x(13).\n"
			+ "                       11 details-a-f4        pic x(14).\n"
			+ "                    09 details-a-f5      pic x(15).\n"
			+ "                 07 details-a-f5         pic x(16).\n"
			+ "                 \n"
			+ "              05 details-type-b redefines Details.\n"
			+ "                 07 details-b-g1.\n"
			+ "                    09 details-b-f1      pic x(21).\n"
			+ "                    09 details-b-g1 occurs 2.\n"
			+ "                       01 details-b-g1 occurs 2.\n"
			+ "                          15 details-b-f2   pic x(22).\n"
			+ "                          15 details-b-f3   pic x(23).\n"
			+ "             05 details-type-c redefines Details.\n"
			+ "                07 details-c-f1          pic x(31).\n"
			+ "                07 details-c-f2          pic x(32).\n"
			+ "                07 details-c-f3          pic x(33).\n"
			+ "";
	
	private static final String EXPECTED_XML = ""
			+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<RECORD RECORDNAME=\"RECORD-1\" COPYBOOK=\"RECORD-1\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"GroupOfBinaryRecords\" LIST=\"Y\" INITSPACES=\"Y\" QUOTE=\"\" RecSep=\"default\">\n"
			+ "    <RECORDS>\n"
			+ "        <RECORD RECORDNAME=\"Details\" COPYBOOK=\"Details\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
			+ "            <ITEMS CopybookPref=\"RECORD-1\" JRecNaming=\"TRUE\">\n"
			+ "                <item level=\"01\" name=\"Rec-1\" position=\"1\" storage-length=\"167\" display-length=\"167\">\n"
			+ "                    <item level=\"03\" name=\"h-1\" position=\"1\" storage-length=\"167\" display-length=\"167\">\n"
			+ "                        <item level=\"04\" name=\"h1-1\" position=\"1\" storage-length=\"49\" display-length=\"49\">\n"
			+ "                            <item level=\"05\" name=\"record-type\" picture=\"x\" position=\"1\" storage-length=\"1\" display-length=\"1\"/>\n"
			+ "                            <item level=\"05\" name=\"A-1\" occurs=\"3\" picture=\"s99\" position=\"2\" storage-length=\"2\" display-length=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "                            <item level=\"05\" name=\"h1-1-1\" position=\"8\" storage-length=\"32\" display-length=\"32\">\n"
			+ "                                <item level=\"07\" name=\"a-2\" occurs=\"4\" position=\"8\" storage-length=\"8\" display-length=\"8\">\n"
			+ "                                    <item level=\"10\" name=\"a-2-f1\" picture=\"s94v99\" position=\"8\" storage-length=\"3\" display-length=\"3\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "                                    <item level=\"10\" name=\"a-2-f2\" picture=\"x(5)\" position=\"11\" storage-length=\"5\" display-length=\"5\"/>\n"
			+ "                                </item>\n"
			+ "                            </item>\n"
			+ "                            <item level=\"05\" name=\"h-1-1-f1\" picture=\"x(10)\" position=\"40\" storage-length=\"10\" display-length=\"10\"/>\n"
			+ "                        </item>\n"
			+ "                        <item level=\"04\" name=\"h1-2\" position=\"50\" storage-length=\"2\" display-length=\"2\">\n"
			+ "                            <item level=\"05\" name=\"h1-2-f1\" picture=\"xx\" position=\"50\" storage-length=\"2\" display-length=\"2\"/>\n"
			+ "                        </item>\n"
			+ "                        <item level=\"04\" name=\"h1-3\" position=\"52\" storage-length=\"116\" display-length=\"116\">\n"
			+ "                            <item level=\"05\" name=\"h1-3-f1\" picture=\"xx\" position=\"52\" storage-length=\"2\" display-length=\"2\"/>\n"
			+ "                            <item level=\"05\" name=\"h1-3-a1\" occurs=\"3\" picture=\"xx\" position=\"54\" storage-length=\"2\" display-length=\"2\"/>\n"
			+ "                            <item level=\"05\" name=\"Details\" picture=\"x(50)\" position=\"60\" storage-length=\"50\" display-length=\"50\" redefined=\"true\"/>\n"
			+ "                        </item>\n"
			+ "                    </item>\n"
			+ "                </item>\n"
			+ "                <item level=\"01\" name=\"details-b-g1\" occurs=\"2\" position=\"1\" storage-length=\"141\" display-length=\"141\">\n"
			+ "                    <item level=\"15\" name=\"details-b-f2\" picture=\"x(22)\" position=\"1\" storage-length=\"22\" display-length=\"22\"/>\n"
			+ "                    <item level=\"15\" name=\"details-b-f3\" picture=\"x(23)\" position=\"23\" storage-length=\"23\" display-length=\"23\"/>\n"
			+ "                    <item level=\"05\" name=\"details-type-c\" position=\"46\" storage-length=\"96\" display-length=\"96\" redefines=\"Details\">\n"
			+ "                        <item level=\"07\" name=\"details-c-f1\" picture=\"x(31)\" position=\"46\" storage-length=\"31\" display-length=\"31\"/>\n"
			+ "                        <item level=\"07\" name=\"details-c-f2\" picture=\"x(32)\" position=\"77\" storage-length=\"32\" display-length=\"32\"/>\n"
			+ "                        <item level=\"07\" name=\"details-c-f3\" picture=\"x(33)\" position=\"109\" storage-length=\"33\" display-length=\"33\"/>\n"
			+ "                    </item>\n"
			+ "                </item>\n"
			+ "            </ITEMS>\n"
			+ "        </RECORD>\n"
			+ "        <RECORD RECORDNAME=\"details-type-a\" COPYBOOK=\"details-type-a\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
			+ "            <ITEMS CopybookPref=\"RECORD-1\" JRecNaming=\"TRUE\">\n"
			+ "                <item level=\"01\" name=\"Rec-1\" position=\"1\" storage-length=\"167\" display-length=\"167\">\n"
			+ "                    <item level=\"03\" name=\"h-1\" position=\"1\" storage-length=\"167\" display-length=\"167\">\n"
			+ "                        <item level=\"04\" name=\"h1-1\" position=\"1\" storage-length=\"49\" display-length=\"49\">\n"
			+ "                            <item level=\"05\" name=\"record-type\" picture=\"x\" position=\"1\" storage-length=\"1\" display-length=\"1\"/>\n"
			+ "                            <item level=\"05\" name=\"A-1\" occurs=\"3\" picture=\"s99\" position=\"2\" storage-length=\"2\" display-length=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "                            <item level=\"05\" name=\"h1-1-1\" position=\"8\" storage-length=\"32\" display-length=\"32\">\n"
			+ "                                <item level=\"07\" name=\"a-2\" occurs=\"4\" position=\"8\" storage-length=\"8\" display-length=\"8\">\n"
			+ "                                    <item level=\"10\" name=\"a-2-f1\" picture=\"s94v99\" position=\"8\" storage-length=\"3\" display-length=\"3\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "                                    <item level=\"10\" name=\"a-2-f2\" picture=\"x(5)\" position=\"11\" storage-length=\"5\" display-length=\"5\"/>\n"
			+ "                                </item>\n"
			+ "                            </item>\n"
			+ "                            <item level=\"05\" name=\"h-1-1-f1\" picture=\"x(10)\" position=\"40\" storage-length=\"10\" display-length=\"10\"/>\n"
			+ "                        </item>\n"
			+ "                        <item level=\"04\" name=\"h1-2\" position=\"50\" storage-length=\"2\" display-length=\"2\">\n"
			+ "                            <item level=\"05\" name=\"h1-2-f1\" picture=\"xx\" position=\"50\" storage-length=\"2\" display-length=\"2\"/>\n"
			+ "                        </item>\n"
			+ "                        <item level=\"04\" name=\"h1-3\" position=\"52\" storage-length=\"116\" display-length=\"116\">\n"
			+ "                            <item level=\"05\" name=\"h1-3-f1\" picture=\"xx\" position=\"52\" storage-length=\"2\" display-length=\"2\"/>\n"
			+ "                            <item level=\"05\" name=\"h1-3-a1\" occurs=\"3\" picture=\"xx\" position=\"54\" storage-length=\"2\" display-length=\"2\"/>\n"
			+ "                            <item level=\"05\" name=\"details-type-a\" position=\"60\" storage-length=\"108\" display-length=\"108\" redefines=\"Details\">\n"
			+ "                                <item level=\"07\" name=\"details-a-f1\" picture=\"x(11)\" position=\"60\" storage-length=\"11\" display-length=\"11\"/>\n"
			+ "                                <item level=\"07\" name=\"details-a-g1\" position=\"71\" storage-length=\"81\" display-length=\"81\">\n"
			+ "                                    <item level=\"09\" name=\"details-a-f2\" picture=\"x(12)\" position=\"71\" storage-length=\"12\" display-length=\"12\"/>\n"
			+ "                                    <item level=\"09\" name=\"details-a-g1\" occurs=\"2\" position=\"83\" storage-length=\"27\" display-length=\"27\">\n"
			+ "                                        <item level=\"11\" name=\"details-a-f3\" picture=\"x(13)\" position=\"83\" storage-length=\"13\" display-length=\"13\"/>\n"
			+ "                                        <item level=\"11\" name=\"details-a-f4\" picture=\"x(14)\" position=\"96\" storage-length=\"14\" display-length=\"14\"/>\n"
			+ "                                    </item>\n"
			+ "                                    <item level=\"09\" name=\"details-a-f5\" picture=\"x(15)\" position=\"137\" storage-length=\"15\" display-length=\"15\"/>\n"
			+ "                                </item>\n"
			+ "                                <item level=\"07\" name=\"details-a-f5\" picture=\"x(16)\" position=\"152\" storage-length=\"16\" display-length=\"16\"/>\n"
			+ "                            </item>\n"
			+ "                        </item>\n"
			+ "                    </item>\n"
			+ "                </item>\n"
			+ "                <item level=\"01\" name=\"details-b-g1\" occurs=\"2\" position=\"1\" storage-length=\"141\" display-length=\"141\">\n"
			+ "                    <item level=\"15\" name=\"details-b-f2\" picture=\"x(22)\" position=\"1\" storage-length=\"22\" display-length=\"22\"/>\n"
			+ "                    <item level=\"15\" name=\"details-b-f3\" picture=\"x(23)\" position=\"23\" storage-length=\"23\" display-length=\"23\"/>\n"
			+ "                    <item level=\"05\" name=\"details-type-c\" position=\"46\" storage-length=\"96\" display-length=\"96\" redefines=\"Details\">\n"
			+ "                        <item level=\"07\" name=\"details-c-f1\" picture=\"x(31)\" position=\"46\" storage-length=\"31\" display-length=\"31\"/>\n"
			+ "                        <item level=\"07\" name=\"details-c-f2\" picture=\"x(32)\" position=\"77\" storage-length=\"32\" display-length=\"32\"/>\n"
			+ "                        <item level=\"07\" name=\"details-c-f3\" picture=\"x(33)\" position=\"109\" storage-length=\"33\" display-length=\"33\"/>\n"
			+ "                    </item>\n"
			+ "                </item>\n"
			+ "            </ITEMS>\n"
			+ "        </RECORD>\n"
			+ "        <RECORD RECORDNAME=\"details-type-b\" COPYBOOK=\"details-type-b\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
			+ "            <ITEMS CopybookPref=\"RECORD-1\" JRecNaming=\"TRUE\">\n"
			+ "                <item level=\"01\" name=\"Rec-1\" position=\"1\" storage-length=\"167\" display-length=\"167\">\n"
			+ "                    <item level=\"03\" name=\"h-1\" position=\"1\" storage-length=\"167\" display-length=\"167\">\n"
			+ "                        <item level=\"04\" name=\"h1-1\" position=\"1\" storage-length=\"49\" display-length=\"49\">\n"
			+ "                            <item level=\"05\" name=\"record-type\" picture=\"x\" position=\"1\" storage-length=\"1\" display-length=\"1\"/>\n"
			+ "                            <item level=\"05\" name=\"A-1\" occurs=\"3\" picture=\"s99\" position=\"2\" storage-length=\"2\" display-length=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "                            <item level=\"05\" name=\"h1-1-1\" position=\"8\" storage-length=\"32\" display-length=\"32\">\n"
			+ "                                <item level=\"07\" name=\"a-2\" occurs=\"4\" position=\"8\" storage-length=\"8\" display-length=\"8\">\n"
			+ "                                    <item level=\"10\" name=\"a-2-f1\" picture=\"s94v99\" position=\"8\" storage-length=\"3\" display-length=\"3\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
			+ "                                    <item level=\"10\" name=\"a-2-f2\" picture=\"x(5)\" position=\"11\" storage-length=\"5\" display-length=\"5\"/>\n"
			+ "                                </item>\n"
			+ "                            </item>\n"
			+ "                            <item level=\"05\" name=\"h-1-1-f1\" picture=\"x(10)\" position=\"40\" storage-length=\"10\" display-length=\"10\"/>\n"
			+ "                        </item>\n"
			+ "                        <item level=\"04\" name=\"h1-2\" position=\"50\" storage-length=\"2\" display-length=\"2\">\n"
			+ "                            <item level=\"05\" name=\"h1-2-f1\" picture=\"xx\" position=\"50\" storage-length=\"2\" display-length=\"2\"/>\n"
			+ "                        </item>\n"
			+ "                        <item level=\"04\" name=\"h1-3\" position=\"52\" storage-length=\"116\" display-length=\"116\">\n"
			+ "                            <item level=\"05\" name=\"h1-3-f1\" picture=\"xx\" position=\"52\" storage-length=\"2\" display-length=\"2\"/>\n"
			+ "                            <item level=\"05\" name=\"h1-3-a1\" occurs=\"3\" picture=\"xx\" position=\"54\" storage-length=\"2\" display-length=\"2\"/>\n"
			+ "                            <item level=\"05\" name=\"details-type-b\" position=\"60\" storage-length=\"21\" display-length=\"21\" redefines=\"Details\">\n"
			+ "                                <item level=\"07\" name=\"details-b-g1\" position=\"60\" storage-length=\"21\" display-length=\"21\">\n"
			+ "                                    <item level=\"09\" name=\"details-b-f1\" picture=\"x(21)\" position=\"60\" storage-length=\"21\" display-length=\"21\"/>\n"
			+ "                                    <item level=\"09\" name=\"details-b-g1\" occurs=\"2\" position=\"81\" display-length=\"0\"/>\n"
			+ "                                </item>\n"
			+ "                            </item>\n"
			+ "                        </item>\n"
			+ "                    </item>\n"
			+ "                </item>\n"
			+ "                <item level=\"01\" name=\"details-b-g1\" occurs=\"2\" position=\"1\" storage-length=\"141\" display-length=\"141\">\n"
			+ "                    <item level=\"15\" name=\"details-b-f2\" picture=\"x(22)\" position=\"1\" storage-length=\"22\" display-length=\"22\"/>\n"
			+ "                    <item level=\"15\" name=\"details-b-f3\" picture=\"x(23)\" position=\"23\" storage-length=\"23\" display-length=\"23\"/>\n"
			+ "                    <item level=\"05\" name=\"details-type-c\" position=\"46\" storage-length=\"96\" display-length=\"96\" redefines=\"Details\">\n"
			+ "                        <item level=\"07\" name=\"details-c-f1\" picture=\"x(31)\" position=\"46\" storage-length=\"31\" display-length=\"31\"/>\n"
			+ "                        <item level=\"07\" name=\"details-c-f2\" picture=\"x(32)\" position=\"77\" storage-length=\"32\" display-length=\"32\"/>\n"
			+ "                        <item level=\"07\" name=\"details-c-f3\" picture=\"x(33)\" position=\"109\" storage-length=\"33\" display-length=\"33\"/>\n"
			+ "                    </item>\n"
			+ "                </item>\n"
			+ "            </ITEMS>\n"
			+ "        </RECORD>\n"
			+ "    </RECORDS>\n"
			+ "</RECORD>";
	private static final FieldDetail[][] EXPECTED_FIELDS = {
			{
				FieldDetail.newFixedWidthField("record-type", Type.ftChar, 1, 1, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-b-f2 (0)", Type.ftChar, 1, 22, 0, "cp1252"),
				FieldDetail.newFixedWidthField("A-1 (0)", Type.ftZonedAsciiSmall, 2, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("A-1 (1)", Type.ftZonedAsciiSmall, 4, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("A-1 (2)", Type.ftZonedAsciiSmall, 6, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f1 (0)", Type.ftChar, 8, 3, 2, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f2 (0)", Type.ftChar, 11, 5, 0, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f1 (1)", Type.ftChar, 16, 3, 2, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f2 (1)", Type.ftChar, 19, 5, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-b-f3 (0)", Type.ftChar, 23, 23, 0, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f1 (2)", Type.ftChar, 24, 3, 2, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f2 (2)", Type.ftChar, 27, 5, 0, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f1 (3)", Type.ftChar, 32, 3, 2, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f2 (3)", Type.ftChar, 35, 5, 0, "cp1252"),
				FieldDetail.newFixedWidthField("h-1-1-f1", Type.ftChar, 40, 10, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-c-f1 (0)", Type.ftChar, 46, 31, 0, "cp1252"),
				FieldDetail.newFixedWidthField("h1-2-f1", Type.ftChar, 50, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("h1-3-f1", Type.ftChar, 52, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("h1-3-a1 (0)", Type.ftChar, 54, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("h1-3-a1 (1)", Type.ftChar, 56, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("h1-3-a1 (2)", Type.ftChar, 58, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("Details", Type.ftChar, 60, 50, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-c-f2 (0)", Type.ftChar, 77, 32, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-c-f3 (0)", Type.ftChar, 109, 33, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-b-f2 (1)", Type.ftChar, 142, 22, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-b-f3 (1)", Type.ftChar, 164, 23, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-c-f1 (1)", Type.ftChar, 187, 31, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-c-f2 (1)", Type.ftChar, 218, 32, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-c-f3 (1)", Type.ftChar, 250, 33, 0, "cp1252"),
			}, {
				FieldDetail.newFixedWidthField("record-type", Type.ftChar, 1, 1, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-b-f2 (0)", Type.ftChar, 1, 22, 0, "cp1252"),
				FieldDetail.newFixedWidthField("A-1 (0)", Type.ftZonedAsciiSmall, 2, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("A-1 (1)", Type.ftZonedAsciiSmall, 4, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("A-1 (2)", Type.ftZonedAsciiSmall, 6, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f1 (0)", Type.ftChar, 8, 3, 2, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f2 (0)", Type.ftChar, 11, 5, 0, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f1 (1)", Type.ftChar, 16, 3, 2, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f2 (1)", Type.ftChar, 19, 5, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-b-f3 (0)", Type.ftChar, 23, 23, 0, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f1 (2)", Type.ftChar, 24, 3, 2, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f2 (2)", Type.ftChar, 27, 5, 0, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f1 (3)", Type.ftChar, 32, 3, 2, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f2 (3)", Type.ftChar, 35, 5, 0, "cp1252"),
				FieldDetail.newFixedWidthField("h-1-1-f1", Type.ftChar, 40, 10, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-c-f1 (0)", Type.ftChar, 46, 31, 0, "cp1252"),
				FieldDetail.newFixedWidthField("h1-2-f1", Type.ftChar, 50, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("h1-3-f1", Type.ftChar, 52, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("h1-3-a1 (0)", Type.ftChar, 54, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("h1-3-a1 (1)", Type.ftChar, 56, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("h1-3-a1 (2)", Type.ftChar, 58, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-a-f1", Type.ftChar, 60, 11, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-a-f2", Type.ftChar, 71, 12, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-c-f2 (0)", Type.ftChar, 77, 32, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-a-f3 (0)", Type.ftChar, 83, 13, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-a-f4 (0)", Type.ftChar, 96, 14, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-c-f3 (0)", Type.ftChar, 109, 33, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-a-f3 (1)", Type.ftChar, 110, 13, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-a-f4 (1)", Type.ftChar, 123, 14, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-a-f5", Type.ftChar, 137, 15, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-b-f2 (1)", Type.ftChar, 142, 22, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-a-f5", Type.ftChar, 152, 16, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-b-f3 (1)", Type.ftChar, 164, 23, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-c-f1 (1)", Type.ftChar, 187, 31, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-c-f2 (1)", Type.ftChar, 218, 32, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-c-f3 (1)", Type.ftChar, 250, 33, 0, "cp1252"),
			}, {
				FieldDetail.newFixedWidthField("record-type", Type.ftChar, 1, 1, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-b-f2 (0)", Type.ftChar, 1, 22, 0, "cp1252"),
				FieldDetail.newFixedWidthField("A-1 (0)", Type.ftZonedAsciiSmall, 2, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("A-1 (1)", Type.ftZonedAsciiSmall, 4, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("A-1 (2)", Type.ftZonedAsciiSmall, 6, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f1 (0)", Type.ftChar, 8, 3, 2, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f2 (0)", Type.ftChar, 11, 5, 0, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f1 (1)", Type.ftChar, 16, 3, 2, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f2 (1)", Type.ftChar, 19, 5, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-b-f3 (0)", Type.ftChar, 23, 23, 0, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f1 (2)", Type.ftChar, 24, 3, 2, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f2 (2)", Type.ftChar, 27, 5, 0, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f1 (3)", Type.ftChar, 32, 3, 2, "cp1252"),
				FieldDetail.newFixedWidthField("a-2-f2 (3)", Type.ftChar, 35, 5, 0, "cp1252"),
				FieldDetail.newFixedWidthField("h-1-1-f1", Type.ftChar, 40, 10, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-c-f1 (0)", Type.ftChar, 46, 31, 0, "cp1252"),
				FieldDetail.newFixedWidthField("h1-2-f1", Type.ftChar, 50, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("h1-3-f1", Type.ftChar, 52, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("h1-3-a1 (0)", Type.ftChar, 54, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("h1-3-a1 (1)", Type.ftChar, 56, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("h1-3-a1 (2)", Type.ftChar, 58, 2, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-b-f1", Type.ftChar, 60, 21, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-c-f2 (0)", Type.ftChar, 77, 32, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-b-g1 (0)", Type.ftChar, 81, 0, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-b-g1 (1)", Type.ftChar, 81, 0, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-c-f3 (0)", Type.ftChar, 109, 33, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-b-f2 (1)", Type.ftChar, 142, 22, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-b-f3 (1)", Type.ftChar, 164, 23, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-c-f1 (1)", Type.ftChar, 187, 31, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-c-f2 (1)", Type.ftChar, 218, 32, 0, "cp1252"),
				FieldDetail.newFixedWidthField("details-c-f3 (1)", Type.ftChar, 250, 33, 0, "cp1252"),
			}

	};

	@Test
	void test() throws IOException, XMLStreamException, FactoryConfigurationError {
		CheckSchema check = new CheckSchema("RECORD-1", EXPECTED_FIELDS);
		check.checkConversion(
				COPYBOOK_CODE, 
				EXPECTED_XML);

	}

}
