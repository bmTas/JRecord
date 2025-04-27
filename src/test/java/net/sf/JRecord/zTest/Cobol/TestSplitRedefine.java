package net.sf.JRecord.zTest.Cobol;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import org.junit.jupiter.api.Test;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.constantNames.ConstantNameConversion;
import net.sf.JRecord.constantNames.ConstantNames;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

class TestSplitRedefine {

	String header = ""
			+ "01  EMPLOYEE-RECORD.\n";
	
	String copybookDetails = ""
			+ "     07  EMP-TYPE                PIC X.\n"
			+ "     07  EMP-Header.           \n"
			+ "         10 emp-h1               pic x(10).\n"
			+ "         10 emp-h2               pic x(20).\n"
			+ "     07  EMP-f1                  pic 9(4).\n"
			+ "     07  EMP-DETAILS             PIC X(30).\n"
			+ "     07  EMP-REGULAR REDEFINES EMP-DETAILS.\n"
			+ "         10  EMPR-Fld-1          PIC 9(8)V99.  \n"
			+ "         10  Empr-grp-1.\n"
			+ "             15  EMPR-Fld-1-1    pic x.\n"
			+ "                  15  EMPR-Fld-1-2    pic x(20).       \n"
			+ "         10  EMPR-Fld-2          PIC X(10).\n"
			+ "         10  Empr-grp-2.\n"
			+ "             15  EMPR-Fld-1-2    pic x(10).\n"
			+ "         10  EMPR-Fld-3          PIC X(10).\n"
			+ "     07  EMP-CONTRACT REDEFINES EMP-DETAILS.\n"
			+ "         10  CONTRACT-Fld-1      PIC 9(6)V99.\n"
			+ "         10  CONTRACT-Fld-2      PIC 9(3).\n"
			+ "         10  CONTRACT-Fld-3      PIC X(19).";
	
	private static final FieldDetail[][] EXPECTED_FIELDS = {
			{
				FieldDetail.newFixedWidthField("EMP-TYPE", Type.ftChar, 1, 1, 0, "cp1252"),
				FieldDetail.newFixedWidthField("emp-h1", Type.ftChar, 2, 10, 0, "cp1252"),
				FieldDetail.newFixedWidthField("emp-h2", Type.ftChar, 12, 20, 0, "cp1252"),
				FieldDetail.newFixedWidthField("EMP-f1", Type.ftNumZeroPaddedPositive, 32, 4, 0, "cp1252"),
				FieldDetail.newFixedWidthField("EMP-DETAILS", Type.ftChar, 36, 30, 0, "cp1252"),
			}, {
				FieldDetail.newFixedWidthField("EMP-TYPE", Type.ftChar, 1, 1, 0, "cp1252"),
				FieldDetail.newFixedWidthField("emp-h1", Type.ftChar, 2, 10, 0, "cp1252"),
				FieldDetail.newFixedWidthField("emp-h2", Type.ftChar, 12, 20, 0, "cp1252"),
				FieldDetail.newFixedWidthField("EMP-f1", Type.ftNumZeroPaddedPositive, 32, 4, 0, "cp1252"),
				FieldDetail.newFixedWidthField("EMPR-Fld-1", Type.ftAssumedDecimalPositive, 36, 10, 2, "cp1252"),
				FieldDetail.newFixedWidthField("EMPR-Fld-1-1", Type.ftChar, 46, 1, 0, "cp1252"),
				FieldDetail.newFixedWidthField("EMPR-Fld-1-2", Type.ftChar, 47, 20, 0, "cp1252"),
				FieldDetail.newFixedWidthField("EMPR-Fld-2", Type.ftChar, 67, 10, 0, "cp1252"),
				FieldDetail.newFixedWidthField("EMPR-Fld-1-2", Type.ftChar, 77, 10, 0, "cp1252"),
				FieldDetail.newFixedWidthField("EMPR-Fld-3", Type.ftChar, 87, 10, 0, "cp1252"),
			}, {
				FieldDetail.newFixedWidthField("EMP-TYPE", Type.ftChar, 1, 1, 0, "cp1252"),
				FieldDetail.newFixedWidthField("emp-h1", Type.ftChar, 2, 10, 0, "cp1252"),
				FieldDetail.newFixedWidthField("emp-h2", Type.ftChar, 12, 20, 0, "cp1252"),
				FieldDetail.newFixedWidthField("EMP-f1", Type.ftNumZeroPaddedPositive, 32, 4, 0, "cp1252"),
				FieldDetail.newFixedWidthField("CONTRACT-Fld-1", Type.ftAssumedDecimalPositive, 36, 8, 2, "cp1252"),
				FieldDetail.newFixedWidthField("CONTRACT-Fld-2", Type.ftNumZeroPaddedPositive, 44, 3, 0, "cp1252"),
				FieldDetail.newFixedWidthField("CONTRACT-Fld-3", Type.ftChar, 47, 19, 0, "cp1252"),
			}
	};

	@Test
	void testBasic() throws IOException, XMLStreamException, FactoryConfigurationError {
		String expected = ""
				+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
				+ "<RECORD RECORDNAME=\"EMPLOYEE-RECORD\" COPYBOOK=\"EMPLOYEE-RECORD\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"GroupOfBinaryRecords\" LIST=\"Y\" INITSPACES=\"Y\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "    <RECORDS>\n"
				+ "        <RECORD RECORDNAME=\"EMP-DETAILS\" COPYBOOK=\"EMP-DETAILS\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "            <ITEMS CopybookPref=\"EMPLOYEE-RECORD\" JRecNaming=\"TRUE\">\n"
				+ "                <item level=\"01\" name=\"EMPLOYEE-RECORD\" position=\"1\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                    <item level=\"07\" name=\"EMP-TYPE\" picture=\"X\" position=\"1\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                    <item level=\"07\" name=\"EMP-Header\" position=\"2\" storage-length=\"30\" display-length=\"30\">\n"
				+ "                        <item level=\"10\" name=\"emp-h1\" picture=\"x(10)\" position=\"2\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                        <item level=\"10\" name=\"emp-h2\" picture=\"x(20)\" position=\"12\" storage-length=\"20\" display-length=\"20\"/>\n"
				+ "                    </item>\n"
				+ "                    <item level=\"07\" name=\"EMP-f1\" picture=\"9(4)\" position=\"32\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                    <item level=\"07\" name=\"EMP-DETAILS\" picture=\"X(30)\" position=\"36\" storage-length=\"30\" display-length=\"30\" redefined=\"true\"/>\n"
				+ "                </item>\n"
				+ "            </ITEMS>\n"
				+ "        </RECORD>\n"
				+ "        <RECORD RECORDNAME=\"EMP-REGULAR\" COPYBOOK=\"EMP-REGULAR\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "            <ITEMS CopybookPref=\"EMPLOYEE-RECORD\" JRecNaming=\"TRUE\">\n"
				+ "                <item level=\"01\" name=\"EMPLOYEE-RECORD\" position=\"1\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                    <item level=\"07\" name=\"EMP-TYPE\" picture=\"X\" position=\"1\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                    <item level=\"07\" name=\"EMP-Header\" position=\"2\" storage-length=\"30\" display-length=\"30\">\n"
				+ "                        <item level=\"10\" name=\"emp-h1\" picture=\"x(10)\" position=\"2\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                        <item level=\"10\" name=\"emp-h2\" picture=\"x(20)\" position=\"12\" storage-length=\"20\" display-length=\"20\"/>\n"
				+ "                    </item>\n"
				+ "                    <item level=\"07\" name=\"EMP-f1\" picture=\"9(4)\" position=\"32\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                    <item level=\"07\" name=\"EMP-REGULAR\" position=\"36\" storage-length=\"61\" display-length=\"61\" redefines=\"EMP-DETAILS\">\n"
				+ "                        <item level=\"10\" name=\"EMPR-Fld-1\" picture=\"9(8)V99\" position=\"36\" storage-length=\"10\" display-length=\"10\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                        <item level=\"10\" name=\"Empr-grp-1\" position=\"46\" storage-length=\"21\" display-length=\"21\">\n"
				+ "                            <item level=\"15\" name=\"EMPR-Fld-1-1\" picture=\"x\" position=\"46\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                            <item level=\"15\" name=\"EMPR-Fld-1-2\" picture=\"x(20)\" position=\"47\" storage-length=\"20\" display-length=\"20\"/>\n"
				+ "                        </item>\n"
				+ "                        <item level=\"10\" name=\"EMPR-Fld-2\" picture=\"X(10)\" position=\"67\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                        <item level=\"10\" name=\"Empr-grp-2\" position=\"77\" storage-length=\"10\" display-length=\"10\">\n"
				+ "                            <item level=\"15\" name=\"EMPR-Fld-1-2\" picture=\"x(10)\" position=\"77\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                        </item>\n"
				+ "                        <item level=\"10\" name=\"EMPR-Fld-3\" picture=\"X(10)\" position=\"87\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                    </item>\n"
				+ "                </item>\n"
				+ "            </ITEMS>\n"
				+ "        </RECORD>\n"
				+ "        <RECORD RECORDNAME=\"EMP-CONTRACT\" COPYBOOK=\"EMP-CONTRACT\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "            <ITEMS CopybookPref=\"EMPLOYEE-RECORD\" JRecNaming=\"TRUE\">\n"
				+ "                <item level=\"01\" name=\"EMPLOYEE-RECORD\" position=\"1\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                    <item level=\"07\" name=\"EMP-TYPE\" picture=\"X\" position=\"1\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                    <item level=\"07\" name=\"EMP-Header\" position=\"2\" storage-length=\"30\" display-length=\"30\">\n"
				+ "                        <item level=\"10\" name=\"emp-h1\" picture=\"x(10)\" position=\"2\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                        <item level=\"10\" name=\"emp-h2\" picture=\"x(20)\" position=\"12\" storage-length=\"20\" display-length=\"20\"/>\n"
				+ "                    </item>\n"
				+ "                    <item level=\"07\" name=\"EMP-f1\" picture=\"9(4)\" position=\"32\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                    <item level=\"07\" name=\"EMP-CONTRACT\" position=\"36\" storage-length=\"30\" display-length=\"30\" redefines=\"EMP-DETAILS\">\n"
				+ "                        <item level=\"10\" name=\"CONTRACT-Fld-1\" picture=\"9(6)V99\" position=\"36\" storage-length=\"8\" display-length=\"8\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                        <item level=\"10\" name=\"CONTRACT-Fld-2\" picture=\"9(3)\" position=\"44\" storage-length=\"3\" display-length=\"3\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                        <item level=\"10\" name=\"CONTRACT-Fld-3\" picture=\"X(19)\" position=\"47\" storage-length=\"19\" display-length=\"19\"/>\n"
				+ "                    </item>\n"
				+ "                </item>\n"
				+ "            </ITEMS>\n"
				+ "        </RECORD>\n"
				+ "    </RECORDS>\n"
				+ "</RECORD>";
		
		CheckSchema check = new CheckSchema("EMPLOYEE-RECORD", EXPECTED_FIELDS);
		check.checkConversion(
				header
				+ copybookDetails, 
				expected);
	}

	@Test
	void testOneGroup() throws IOException, XMLStreamException, FactoryConfigurationError {
		String expected = ""
				+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
				+ "<RECORD RECORDNAME=\"EMPLOYEE-RECORD\" COPYBOOK=\"EMPLOYEE-RECORD\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"GroupOfBinaryRecords\" LIST=\"Y\" INITSPACES=\"Y\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "    <RECORDS>\n"
				+ "        <RECORD RECORDNAME=\"EMP-DETAILS\" COPYBOOK=\"EMP-DETAILS\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "            <ITEMS CopybookPref=\"EMPLOYEE-RECORD\" JRecNaming=\"TRUE\">\n"
				+ "                <item level=\"01\" name=\"EMPLOYEE-RECORD\" position=\"1\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                    <item level=\"03\" name=\"Emp-Header-1\" position=\"1\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                        <item level=\"07\" name=\"EMP-TYPE\" picture=\"X\" position=\"1\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                        <item level=\"07\" name=\"EMP-Header\" position=\"2\" storage-length=\"30\" display-length=\"30\">\n"
				+ "                            <item level=\"10\" name=\"emp-h1\" picture=\"x(10)\" position=\"2\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                            <item level=\"10\" name=\"emp-h2\" picture=\"x(20)\" position=\"12\" storage-length=\"20\" display-length=\"20\"/>\n"
				+ "                        </item>\n"
				+ "                        <item level=\"07\" name=\"EMP-f1\" picture=\"9(4)\" position=\"32\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                        <item level=\"07\" name=\"EMP-DETAILS\" picture=\"X(30)\" position=\"36\" storage-length=\"30\" display-length=\"30\" redefined=\"true\"/>\n"
				+ "                    </item>\n"
				+ "                </item>\n"
				+ "            </ITEMS>\n"
				+ "        </RECORD>\n"
				+ "        <RECORD RECORDNAME=\"EMP-REGULAR\" COPYBOOK=\"EMP-REGULAR\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "            <ITEMS CopybookPref=\"EMPLOYEE-RECORD\" JRecNaming=\"TRUE\">\n"
				+ "                <item level=\"01\" name=\"EMPLOYEE-RECORD\" position=\"1\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                    <item level=\"03\" name=\"Emp-Header-1\" position=\"1\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                        <item level=\"07\" name=\"EMP-TYPE\" picture=\"X\" position=\"1\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                        <item level=\"07\" name=\"EMP-Header\" position=\"2\" storage-length=\"30\" display-length=\"30\">\n"
				+ "                            <item level=\"10\" name=\"emp-h1\" picture=\"x(10)\" position=\"2\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                            <item level=\"10\" name=\"emp-h2\" picture=\"x(20)\" position=\"12\" storage-length=\"20\" display-length=\"20\"/>\n"
				+ "                        </item>\n"
				+ "                        <item level=\"07\" name=\"EMP-f1\" picture=\"9(4)\" position=\"32\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                        <item level=\"07\" name=\"EMP-REGULAR\" position=\"36\" storage-length=\"61\" display-length=\"61\" redefines=\"EMP-DETAILS\">\n"
				+ "                            <item level=\"10\" name=\"EMPR-Fld-1\" picture=\"9(8)V99\" position=\"36\" storage-length=\"10\" display-length=\"10\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                            <item level=\"10\" name=\"Empr-grp-1\" position=\"46\" storage-length=\"21\" display-length=\"21\">\n"
				+ "                                <item level=\"15\" name=\"EMPR-Fld-1-1\" picture=\"x\" position=\"46\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                                <item level=\"15\" name=\"EMPR-Fld-1-2\" picture=\"x(20)\" position=\"47\" storage-length=\"20\" display-length=\"20\"/>\n"
				+ "                            </item>\n"
				+ "                            <item level=\"10\" name=\"EMPR-Fld-2\" picture=\"X(10)\" position=\"67\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                            <item level=\"10\" name=\"Empr-grp-2\" position=\"77\" storage-length=\"10\" display-length=\"10\">\n"
				+ "                                <item level=\"15\" name=\"EMPR-Fld-1-2\" picture=\"x(10)\" position=\"77\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                            </item>\n"
				+ "                            <item level=\"10\" name=\"EMPR-Fld-3\" picture=\"X(10)\" position=\"87\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                        </item>\n"
				+ "                    </item>\n"
				+ "                </item>\n"
				+ "            </ITEMS>\n"
				+ "        </RECORD>\n"
				+ "        <RECORD RECORDNAME=\"EMP-CONTRACT\" COPYBOOK=\"EMP-CONTRACT\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "            <ITEMS CopybookPref=\"EMPLOYEE-RECORD\" JRecNaming=\"TRUE\">\n"
				+ "                <item level=\"01\" name=\"EMPLOYEE-RECORD\" position=\"1\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                    <item level=\"03\" name=\"Emp-Header-1\" position=\"1\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                        <item level=\"07\" name=\"EMP-TYPE\" picture=\"X\" position=\"1\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                        <item level=\"07\" name=\"EMP-Header\" position=\"2\" storage-length=\"30\" display-length=\"30\">\n"
				+ "                            <item level=\"10\" name=\"emp-h1\" picture=\"x(10)\" position=\"2\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                            <item level=\"10\" name=\"emp-h2\" picture=\"x(20)\" position=\"12\" storage-length=\"20\" display-length=\"20\"/>\n"
				+ "                        </item>\n"
				+ "                        <item level=\"07\" name=\"EMP-f1\" picture=\"9(4)\" position=\"32\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                        <item level=\"07\" name=\"EMP-CONTRACT\" position=\"36\" storage-length=\"30\" display-length=\"30\" redefines=\"EMP-DETAILS\">\n"
				+ "                            <item level=\"10\" name=\"CONTRACT-Fld-1\" picture=\"9(6)V99\" position=\"36\" storage-length=\"8\" display-length=\"8\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                            <item level=\"10\" name=\"CONTRACT-Fld-2\" picture=\"9(3)\" position=\"44\" storage-length=\"3\" display-length=\"3\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                            <item level=\"10\" name=\"CONTRACT-Fld-3\" picture=\"X(19)\" position=\"47\" storage-length=\"19\" display-length=\"19\"/>\n"
				+ "                        </item>\n"
				+ "                    </item>\n"
				+ "                </item>\n"
				+ "            </ITEMS>\n"
				+ "        </RECORD>\n"
				+ "    </RECORDS>\n"
				+ "</RECORD>";
		
		CheckSchema check = new CheckSchema("EMPLOYEE-RECORD", EXPECTED_FIELDS);
		check.checkConversion(
				header
				+ "  03  Emp-Header-1.\n"
				+ copybookDetails, 
				expected);
	}

	@Test
	void testTwoGroup() throws IOException, XMLStreamException, FactoryConfigurationError {
		String expected = ""
				+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
				+ "<RECORD RECORDNAME=\"EMPLOYEE-RECORD\" COPYBOOK=\"EMPLOYEE-RECORD\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"GroupOfBinaryRecords\" LIST=\"Y\" INITSPACES=\"Y\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "    <RECORDS>\n"
				+ "        <RECORD RECORDNAME=\"EMP-DETAILS\" COPYBOOK=\"EMP-DETAILS\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "            <ITEMS CopybookPref=\"EMPLOYEE-RECORD\" JRecNaming=\"TRUE\">\n"
				+ "                <item level=\"01\" name=\"EMPLOYEE-RECORD\" position=\"1\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                    <item level=\"03\" name=\"Emp-Header-1\" position=\"1\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                        <item level=\"05\" name=\"Emp-Header-11\" position=\"1\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                            <item level=\"07\" name=\"EMP-TYPE\" picture=\"X\" position=\"1\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                            <item level=\"07\" name=\"EMP-Header\" position=\"2\" storage-length=\"30\" display-length=\"30\">\n"
				+ "                                <item level=\"10\" name=\"emp-h1\" picture=\"x(10)\" position=\"2\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                                <item level=\"10\" name=\"emp-h2\" picture=\"x(20)\" position=\"12\" storage-length=\"20\" display-length=\"20\"/>\n"
				+ "                            </item>\n"
				+ "                            <item level=\"07\" name=\"EMP-f1\" picture=\"9(4)\" position=\"32\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                            <item level=\"07\" name=\"EMP-DETAILS\" picture=\"X(30)\" position=\"36\" storage-length=\"30\" display-length=\"30\" redefined=\"true\"/>\n"
				+ "                        </item>\n"
				+ "                    </item>\n"
				+ "                </item>\n"
				+ "            </ITEMS>\n"
				+ "        </RECORD>\n"
				+ "        <RECORD RECORDNAME=\"EMP-REGULAR\" COPYBOOK=\"EMP-REGULAR\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "            <ITEMS CopybookPref=\"EMPLOYEE-RECORD\" JRecNaming=\"TRUE\">\n"
				+ "                <item level=\"01\" name=\"EMPLOYEE-RECORD\" position=\"1\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                    <item level=\"03\" name=\"Emp-Header-1\" position=\"1\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                        <item level=\"05\" name=\"Emp-Header-11\" position=\"1\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                            <item level=\"07\" name=\"EMP-TYPE\" picture=\"X\" position=\"1\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                            <item level=\"07\" name=\"EMP-Header\" position=\"2\" storage-length=\"30\" display-length=\"30\">\n"
				+ "                                <item level=\"10\" name=\"emp-h1\" picture=\"x(10)\" position=\"2\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                                <item level=\"10\" name=\"emp-h2\" picture=\"x(20)\" position=\"12\" storage-length=\"20\" display-length=\"20\"/>\n"
				+ "                            </item>\n"
				+ "                            <item level=\"07\" name=\"EMP-f1\" picture=\"9(4)\" position=\"32\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                            <item level=\"07\" name=\"EMP-REGULAR\" position=\"36\" storage-length=\"61\" display-length=\"61\" redefines=\"EMP-DETAILS\">\n"
				+ "                                <item level=\"10\" name=\"EMPR-Fld-1\" picture=\"9(8)V99\" position=\"36\" storage-length=\"10\" display-length=\"10\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                                <item level=\"10\" name=\"Empr-grp-1\" position=\"46\" storage-length=\"21\" display-length=\"21\">\n"
				+ "                                    <item level=\"15\" name=\"EMPR-Fld-1-1\" picture=\"x\" position=\"46\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                                    <item level=\"15\" name=\"EMPR-Fld-1-2\" picture=\"x(20)\" position=\"47\" storage-length=\"20\" display-length=\"20\"/>\n"
				+ "                                </item>\n"
				+ "                                <item level=\"10\" name=\"EMPR-Fld-2\" picture=\"X(10)\" position=\"67\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                                <item level=\"10\" name=\"Empr-grp-2\" position=\"77\" storage-length=\"10\" display-length=\"10\">\n"
				+ "                                    <item level=\"15\" name=\"EMPR-Fld-1-2\" picture=\"x(10)\" position=\"77\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                                </item>\n"
				+ "                                <item level=\"10\" name=\"EMPR-Fld-3\" picture=\"X(10)\" position=\"87\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                            </item>\n"
				+ "                        </item>\n"
				+ "                    </item>\n"
				+ "                </item>\n"
				+ "            </ITEMS>\n"
				+ "        </RECORD>\n"
				+ "        <RECORD RECORDNAME=\"EMP-CONTRACT\" COPYBOOK=\"EMP-CONTRACT\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "            <ITEMS CopybookPref=\"EMPLOYEE-RECORD\" JRecNaming=\"TRUE\">\n"
				+ "                <item level=\"01\" name=\"EMPLOYEE-RECORD\" position=\"1\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                    <item level=\"03\" name=\"Emp-Header-1\" position=\"1\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                        <item level=\"05\" name=\"Emp-Header-11\" position=\"1\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                            <item level=\"07\" name=\"EMP-TYPE\" picture=\"X\" position=\"1\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                            <item level=\"07\" name=\"EMP-Header\" position=\"2\" storage-length=\"30\" display-length=\"30\">\n"
				+ "                                <item level=\"10\" name=\"emp-h1\" picture=\"x(10)\" position=\"2\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                                <item level=\"10\" name=\"emp-h2\" picture=\"x(20)\" position=\"12\" storage-length=\"20\" display-length=\"20\"/>\n"
				+ "                            </item>\n"
				+ "                            <item level=\"07\" name=\"EMP-f1\" picture=\"9(4)\" position=\"32\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                            <item level=\"07\" name=\"EMP-CONTRACT\" position=\"36\" storage-length=\"30\" display-length=\"30\" redefines=\"EMP-DETAILS\">\n"
				+ "                                <item level=\"10\" name=\"CONTRACT-Fld-1\" picture=\"9(6)V99\" position=\"36\" storage-length=\"8\" display-length=\"8\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                                <item level=\"10\" name=\"CONTRACT-Fld-2\" picture=\"9(3)\" position=\"44\" storage-length=\"3\" display-length=\"3\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                                <item level=\"10\" name=\"CONTRACT-Fld-3\" picture=\"X(19)\" position=\"47\" storage-length=\"19\" display-length=\"19\"/>\n"
				+ "                            </item>\n"
				+ "                        </item>\n"
				+ "                    </item>\n"
				+ "                </item>\n"
				+ "            </ITEMS>\n"
				+ "        </RECORD>\n"
				+ "    </RECORDS>\n"
				+ "</RECORD>";
		
		CheckSchema check = new CheckSchema("EMPLOYEE-RECORD", EXPECTED_FIELDS);
		check.checkConversion(
				header
				+ "  03  Emp-Header-1.\n"
				+ "    05  Emp-Header-11.\n"
				+ copybookDetails, 
				expected);
	}

	@Test
	void testTwoGroup2() throws IOException, XMLStreamException, FactoryConfigurationError {
		String expected = ""
				+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
				+ "<RECORD RECORDNAME=\"EMPLOYEE-RECORD\" COPYBOOK=\"EMPLOYEE-RECORD\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"GroupOfBinaryRecords\" LIST=\"Y\" INITSPACES=\"Y\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "    <RECORDS>\n"
				+ "        <RECORD RECORDNAME=\"EMP-DETAILS\" COPYBOOK=\"EMP-DETAILS\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "            <ITEMS CopybookPref=\"EMPLOYEE-RECORD\" JRecNaming=\"TRUE\">\n"
				+ "                <item level=\"01\" name=\"EMPLOYEE-RECORD\" position=\"1\" storage-length=\"100\" display-length=\"100\">\n"
				+ "                    <item level=\"03\" name=\"Emp-Header-1\" position=\"1\" storage-length=\"100\" display-length=\"100\">\n"
				+ "                        <item level=\"05\" name=\"Emp-Header-11\" position=\"1\" storage-length=\"4\" display-length=\"4\">\n"
				+ "                            <item level=\"07\" name=\"EmpH-Field-1\" picture=\"9\" position=\"1\" storage-length=\"1\" display-length=\"1\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                            <item level=\"07\" name=\"EmpH-Field-2\" picture=\"xxx\" position=\"2\" storage-length=\"3\" display-length=\"3\"/>\n"
				+ "                        </item>\n"
				+ "                        <item level=\"05\" name=\"Emp-Header-21\" position=\"5\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                            <item level=\"07\" name=\"EMP-TYPE\" picture=\"X\" position=\"5\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                            <item level=\"07\" name=\"EMP-Header\" position=\"6\" storage-length=\"30\" display-length=\"30\">\n"
				+ "                                <item level=\"10\" name=\"emp-h1\" picture=\"x(10)\" position=\"6\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                                <item level=\"10\" name=\"emp-h2\" picture=\"x(20)\" position=\"16\" storage-length=\"20\" display-length=\"20\"/>\n"
				+ "                            </item>\n"
				+ "                            <item level=\"07\" name=\"EMP-f1\" picture=\"9(4)\" position=\"36\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                            <item level=\"07\" name=\"EMP-DETAILS\" picture=\"X(30)\" position=\"40\" storage-length=\"30\" display-length=\"30\" redefined=\"true\"/>\n"
				+ "                        </item>\n"
				+ "                    </item>\n"
				+ "                </item>\n"
				+ "            </ITEMS>\n"
				+ "        </RECORD>\n"
				+ "        <RECORD RECORDNAME=\"EMP-REGULAR\" COPYBOOK=\"EMP-REGULAR\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "            <ITEMS CopybookPref=\"EMPLOYEE-RECORD\" JRecNaming=\"TRUE\">\n"
				+ "                <item level=\"01\" name=\"EMPLOYEE-RECORD\" position=\"1\" storage-length=\"100\" display-length=\"100\">\n"
				+ "                    <item level=\"03\" name=\"Emp-Header-1\" position=\"1\" storage-length=\"100\" display-length=\"100\">\n"
				+ "                        <item level=\"05\" name=\"Emp-Header-11\" position=\"1\" storage-length=\"4\" display-length=\"4\">\n"
				+ "                            <item level=\"07\" name=\"EmpH-Field-1\" picture=\"9\" position=\"1\" storage-length=\"1\" display-length=\"1\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                            <item level=\"07\" name=\"EmpH-Field-2\" picture=\"xxx\" position=\"2\" storage-length=\"3\" display-length=\"3\"/>\n"
				+ "                        </item>\n"
				+ "                        <item level=\"05\" name=\"Emp-Header-21\" position=\"5\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                            <item level=\"07\" name=\"EMP-TYPE\" picture=\"X\" position=\"5\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                            <item level=\"07\" name=\"EMP-Header\" position=\"6\" storage-length=\"30\" display-length=\"30\">\n"
				+ "                                <item level=\"10\" name=\"emp-h1\" picture=\"x(10)\" position=\"6\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                                <item level=\"10\" name=\"emp-h2\" picture=\"x(20)\" position=\"16\" storage-length=\"20\" display-length=\"20\"/>\n"
				+ "                            </item>\n"
				+ "                            <item level=\"07\" name=\"EMP-f1\" picture=\"9(4)\" position=\"36\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                            <item level=\"07\" name=\"EMP-REGULAR\" position=\"40\" storage-length=\"61\" display-length=\"61\" redefines=\"EMP-DETAILS\">\n"
				+ "                                <item level=\"10\" name=\"EMPR-Fld-1\" picture=\"9(8)V99\" position=\"40\" storage-length=\"10\" display-length=\"10\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                                <item level=\"10\" name=\"Empr-grp-1\" position=\"50\" storage-length=\"21\" display-length=\"21\">\n"
				+ "                                    <item level=\"15\" name=\"EMPR-Fld-1-1\" picture=\"x\" position=\"50\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                                    <item level=\"15\" name=\"EMPR-Fld-1-2\" picture=\"x(20)\" position=\"51\" storage-length=\"20\" display-length=\"20\"/>\n"
				+ "                                </item>\n"
				+ "                                <item level=\"10\" name=\"EMPR-Fld-2\" picture=\"X(10)\" position=\"71\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                                <item level=\"10\" name=\"Empr-grp-2\" position=\"81\" storage-length=\"10\" display-length=\"10\">\n"
				+ "                                    <item level=\"15\" name=\"EMPR-Fld-1-2\" picture=\"x(10)\" position=\"81\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                                </item>\n"
				+ "                                <item level=\"10\" name=\"EMPR-Fld-3\" picture=\"X(10)\" position=\"91\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                            </item>\n"
				+ "                        </item>\n"
				+ "                    </item>\n"
				+ "                </item>\n"
				+ "            </ITEMS>\n"
				+ "        </RECORD>\n"
				+ "        <RECORD RECORDNAME=\"EMP-CONTRACT\" COPYBOOK=\"EMP-CONTRACT\" DELIMITER=\"&lt;Tab&gt;\" FONTNAME=\"cp1252\" FILESTRUCTURE=\"\" STYLE=\"0\" RECORDTYPE=\"BinaryRecord\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\">\n"
				+ "            <ITEMS CopybookPref=\"EMPLOYEE-RECORD\" JRecNaming=\"TRUE\">\n"
				+ "                <item level=\"01\" name=\"EMPLOYEE-RECORD\" position=\"1\" storage-length=\"100\" display-length=\"100\">\n"
				+ "                    <item level=\"03\" name=\"Emp-Header-1\" position=\"1\" storage-length=\"100\" display-length=\"100\">\n"
				+ "                        <item level=\"05\" name=\"Emp-Header-11\" position=\"1\" storage-length=\"4\" display-length=\"4\">\n"
				+ "                            <item level=\"07\" name=\"EmpH-Field-1\" picture=\"9\" position=\"1\" storage-length=\"1\" display-length=\"1\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                            <item level=\"07\" name=\"EmpH-Field-2\" picture=\"xxx\" position=\"2\" storage-length=\"3\" display-length=\"3\"/>\n"
				+ "                        </item>\n"
				+ "                        <item level=\"05\" name=\"Emp-Header-21\" position=\"5\" storage-length=\"96\" display-length=\"96\">\n"
				+ "                            <item level=\"07\" name=\"EMP-TYPE\" picture=\"X\" position=\"5\" storage-length=\"1\" display-length=\"1\"/>\n"
				+ "                            <item level=\"07\" name=\"EMP-Header\" position=\"6\" storage-length=\"30\" display-length=\"30\">\n"
				+ "                                <item level=\"10\" name=\"emp-h1\" picture=\"x(10)\" position=\"6\" storage-length=\"10\" display-length=\"10\"/>\n"
				+ "                                <item level=\"10\" name=\"emp-h2\" picture=\"x(20)\" position=\"16\" storage-length=\"20\" display-length=\"20\"/>\n"
				+ "                            </item>\n"
				+ "                            <item level=\"07\" name=\"EMP-f1\" picture=\"9(4)\" position=\"36\" storage-length=\"4\" display-length=\"4\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                            <item level=\"07\" name=\"EMP-CONTRACT\" position=\"40\" storage-length=\"30\" display-length=\"30\" redefines=\"EMP-DETAILS\">\n"
				+ "                                <item level=\"10\" name=\"CONTRACT-Fld-1\" picture=\"9(6)V99\" position=\"40\" storage-length=\"8\" display-length=\"8\" scale=\"2\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                                <item level=\"10\" name=\"CONTRACT-Fld-2\" picture=\"9(3)\" position=\"48\" storage-length=\"3\" display-length=\"3\" numeric=\"COBOL_NUMERIC\"/>\n"
				+ "                                <item level=\"10\" name=\"CONTRACT-Fld-3\" picture=\"X(19)\" position=\"51\" storage-length=\"19\" display-length=\"19\"/>\n"
				+ "                            </item>\n"
				+ "                        </item>\n"
				+ "                    </item>\n"
				+ "                </item>\n"
				+ "            </ITEMS>\n"
				+ "        </RECORD>\n"
				+ "    </RECORDS>\n"
				+ "</RECORD>";
		
		CheckSchema check = new CheckSchema("EMPLOYEE-RECORD", EXPECTED_FIELDS);
		ICobolIOBuilder iob = check.checkXml(
				header
				+ "  03  Emp-Header-1.\n"
				+ "    05  Emp-Header-11.\n"
				+ "        07  EmpH-Field-1   pic 9.\n"
				+ "        07  EmpH-Field-2   pic xxx.\n"
				+ "    05  Emp-Header-21.\n"
				+ copybookDetails, 
				expected);
		
		LayoutDetail layout = iob.getLayout();

		
		for (int recNum = 0; recNum < layout.getRecordCount(); recNum++) {
			RecordDetail record = layout.getRecord(recNum);

			for (int fieldNum = 2; fieldNum < record.getFieldCount(); fieldNum++) {
				check.compareFields(EXPECTED_FIELDS[recNum][fieldNum-2], record.getField(fieldNum), 4);
			}
		}

	}
//	
//	private void checkConversion(String copybook, String expectedXml) throws IOException, XMLStreamException, FactoryConfigurationError {
//		ICobolIOBuilder iob = checkXml(copybook, expectedXml);
//		LayoutDetail layout = iob.getLayout();
//		
//		
//		//printExpectedFields(layout);
//		
//		for (int recNum = 0; recNum < layout.getRecordCount(); recNum++) {
//			RecordDetail record = layout.getRecord(recNum);
//
//			for (int fieldNum = 0; fieldNum < record.getFieldCount(); fieldNum++) {
//				compareFields(EXPECTED_FIELDS[recNum][fieldNum], record.getField(fieldNum), 0);
//			}
//		}
//
//	}
//
//	private void compareFields(FieldDetail expectedField, FieldDetail field, int diff) {
//		assertEquals(expectedField.getName(), field.getName());
//		assertEquals(expectedField.getType(), field.getType());
//		assertEquals(expectedField.getPos() + diff, field.getPos());
//		assertEquals(expectedField.getLen(), field.getLen());
//		assertEquals(expectedField.getDecimal(), field.getDecimal());
//		assertEquals(expectedField.getFontName(), field.getFontName());
//	}
//
//	protected ICobolIOBuilder checkXml(String copybook, String expectedXml)
//			throws IOException, XMLStreamException, UnsupportedEncodingException, FactoryConfigurationError {
//		ICobolIOBuilder iob = JRecordInterface1.COBOL.newIOBuilder(
//				 JRecordInterface1.COBOL.newCobolCopybookReader()
//				 		.setCopybookName("EMPLOYEE-RECORD")
//				 		.addFreeFormatCobolText(copybook))
//						.setSplitCopybook(CopybookLoader.SPLIT_REDEFINE);
//		StringWriter w = new StringWriter();
//		JRecordInterface1.SCHEMA_XML
//				.setIndentXml(true)
//				.export(w, iob.getExternalRecord());
//		assertEquals(expectedXml, w.toString());
//		
//		return iob;
//	}
//
//	private void printExpectedFields(LayoutDetail layout) {
//		ConstantNameConversion typeNames = ConstantNames.getTypeNames();
//		for (int recNum = 0; recNum < layout.getRecordCount(); recNum++) {
//			RecordDetail record = layout.getRecord(recNum);
//
//			System.out.println("\t}, {");
//			for (int fieldNum = 0; fieldNum < record.getFieldCount(); fieldNum++) {
//				FieldDetail field = record.getField(fieldNum);
//				System.out.println(
//						"\t\tFieldDetail.newFixedWidthField("
//							+ "\"" + field.getName() + "\", "
//							+ typeNames.getConstantDetails(field.getType()).getJRecordInterfaceConstant() + ", "
//							//+ field.getType() + ", "
//							+ field.getPos() + ", "
//							+ field.getLen() + ", "
//							+ field.getDecimal() + ", "
//							+ "\"" + field.getFontName() + "\"),"
//						);
//			}
//		}
//		System.out.println("\t}");
//	}

}
