package net.sf.JRecord.zTest.External;

import junit.framework.TestCase;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.RecordEditorXmlLoader;

public class TstMultiFieldCheck3  extends TestCase {

	private final static int[] LAYOUT_IDXS = {
		0, 1, 8, 8, 2, 2, 7, 7, 3, 4, 5, 5, 5, 5, 5, 5, 6,
	};
	private final static String[] LINES = {
		"H  111111 2222 333",
		"K H 111111 2222 333",
		"K 1 aaaa    ss ww  111111 2222 333 ",
		"K 1 aaaa    ss ww  122222 2222 333 ",
		"K 2 aaaa    ss ww  222222 2222 333 ",
		"K 2 aaaa    ss ww  222222 2222 333 ",
		"K 3 aaaa    ss ww  300000 2222 333 ",
		"K 3 aaaa    ss ww  333333 2222 333 ",
		"K FFFFFF 111111 2222 333",
		"P H 111111 2222 333",
		"P 1 aaaa  PP ss ww  111111 2222 333 ",
		"P 1 aaaa  PP ss ww  111111 2222 333 ",
		"P 2 aaaa  PP ss ww  222222 2222 333 ",
		"P 2 aaaa  PP ss ww  222222 2222 333 ",
		"P 3 aaaa  PP ss ww  300000 2222 333 ",
		"P 3 aaaa  PP ss ww  333333 2222 333 ",
		"FFFFFF 111111 2222 333"

	};
	private final static String[] XML_LAYOUT = {
		"<RECORD RECORDNAME=\"Wizard_mf\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"GroupOfRecords\" LIST=\"Y\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">",
		"	<RECORDS>",
		"		<RECORD RECORDNAME=\"H\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\" TESTFIELD=\"Record_Type\" TESTVALUE=\"H\" LINE_NO_FIELD_NAMES=\"1\">",
		"			<FIELDS>",
		"				<FIELD NAME=\"Record_Type\" POSITION=\"1\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"H1\" POSITION=\"2\" LENGTH=\"2\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"H2\" POSITION=\"4\" LENGTH=\"6\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"H3\" POSITION=\"10\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"H4\" POSITION=\"11\" LENGTH=\"4\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"H5\" POSITION=\"15\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"H6\" POSITION=\"16\" LENGTH=\"3\" TYPE=\"Num (Right Justified space padded)\"/>",
		"			</FIELDS>",
		"		</RECORD>",
		"		<RECORD RECORDNAME=\"KH\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">",
		"			<TSTFIELDS>",
		"				<TSTFIELD NAME=\"Record_Type\" VALUE=\"K\"/>",
		"				<TSTFIELD NAME=\"Record_Type_2\" VALUE=\"H\"/>",
		"			</TSTFIELDS>",
		"			<FIELDS>",
		"				<FIELD NAME=\"Record_Type\" POSITION=\"1\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"KH1\" POSITION=\"2\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"Record_Type_2\" POSITION=\"3\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"KH3\" POSITION=\"4\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"KH4\" POSITION=\"5\" LENGTH=\"6\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"KH5\" POSITION=\"11\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"KH6\" POSITION=\"12\" LENGTH=\"4\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"KH7\" POSITION=\"16\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"KH8\" POSITION=\"17\" LENGTH=\"3\" TYPE=\"Num (Right Justified space padded)\"/>",
		"			</FIELDS>",
		"		</RECORD>",
		"		<RECORD RECORDNAME=\"K\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">",
		"			<TSTFIELDS DEFAULTRECORD=\"Y\">",
		"				<TSTFIELD NAME=\"Record_Type\" VALUE=\"K\"/>",
		"			</TSTFIELDS>",
		"			<FIELDS>",
		"				<FIELD NAME=\"Record_Type\" POSITION=\"1\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K1\" POSITION=\"2\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"Record_Type_2\" POSITION=\"3\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K3\" POSITION=\"4\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K4\" POSITION=\"5\" LENGTH=\"8\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K5\" POSITION=\"13\" LENGTH=\"3\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K6\" POSITION=\"16\" LENGTH=\"4\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K7\" POSITION=\"20\" LENGTH=\"6\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"K8\" POSITION=\"26\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K9\" POSITION=\"27\" LENGTH=\"4\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"K10\" POSITION=\"31\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K11\" POSITION=\"32\" LENGTH=\"3\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"K12\" POSITION=\"35\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"			</FIELDS>",
		"		</RECORD>",
		"		<RECORD RECORDNAME=\"KF\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">",
		"			<TSTFIELDS>",
		"				<TSTFIELD NAME=\"Record_Type\" VALUE=\"K\"/>",
		"				<TSTFIELD NAME=\"Record_Type_2\" VALUE=\"F\"/>",
		"			</TSTFIELDS>",
		"			<FIELDS>",
		"				<FIELD NAME=\"Record_Type\" POSITION=\"1\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"KF1\" POSITION=\"2\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"Record_Type_2\" POSITION=\"3\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"KF3\" POSITION=\"4\" LENGTH=\"6\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"KF4\" POSITION=\"10\" LENGTH=\"6\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"KF5\" POSITION=\"16\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"KF6\" POSITION=\"17\" LENGTH=\"4\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"KF7\" POSITION=\"21\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"KF8\" POSITION=\"22\" LENGTH=\"3\" TYPE=\"Num (Right Justified space padded)\"/>",
		"			</FIELDS>",
		"		</RECORD>",
		"		<RECORD RECORDNAME=\"PH\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">",
		"			<TSTFIELDS>",
		"				<TSTFIELD NAME=\"Record_Type\" VALUE=\"P\"/>",
		"				<TSTFIELD NAME=\"Record_Type_2\" VALUE=\"H\"/>",
		"			</TSTFIELDS>",
		"			<FIELDS>",
		"				<FIELD NAME=\"Record_Type\" POSITION=\"1\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"PH1\" POSITION=\"2\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"Record_Type_2\" POSITION=\"3\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"PH3\" POSITION=\"4\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"PH4\" POSITION=\"5\" LENGTH=\"6\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"PH5\" POSITION=\"11\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"PH6\" POSITION=\"12\" LENGTH=\"4\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"PH7\" POSITION=\"16\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"PH8\" POSITION=\"17\" LENGTH=\"3\" TYPE=\"Num (Right Justified space padded)\"/>",
		"			</FIELDS>",
		"		</RECORD>",
		"		<RECORD RECORDNAME=\"P\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">",
		"			<TSTFIELDS DEFAULTRECORD=\"Y\">",
		"				<TSTFIELD NAME=\"Record_Type\" VALUE=\"P\"/>",
		"			</TSTFIELDS>",
		"			<FIELDS>",
		"				<FIELD NAME=\"Record_Type\" POSITION=\"1\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"P1\" POSITION=\"2\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"Record_Type_2\" POSITION=\"3\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"P3\" POSITION=\"4\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"P4\" POSITION=\"5\" LENGTH=\"6\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"P5\" POSITION=\"11\" LENGTH=\"3\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"P6\" POSITION=\"14\" LENGTH=\"3\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"P7\" POSITION=\"17\" LENGTH=\"4\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"P8\" POSITION=\"21\" LENGTH=\"6\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"P9\" POSITION=\"27\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"P10\" POSITION=\"28\" LENGTH=\"4\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"P11\" POSITION=\"32\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"P12\" POSITION=\"33\" LENGTH=\"3\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"P13\" POSITION=\"36\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"			</FIELDS>",
		"		</RECORD>",
		"		<RECORD RECORDNAME=\"F\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\" TESTFIELD=\"Record_Type\" TESTVALUE=\"F\" LINE_NO_FIELD_NAMES=\"1\">",
		"			<FIELDS>",
		"				<FIELD NAME=\"Record_Type\" POSITION=\"1\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"F1\" POSITION=\"2\" LENGTH=\"6\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"F2\" POSITION=\"8\" LENGTH=\"6\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"F3\" POSITION=\"14\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"F4\" POSITION=\"15\" LENGTH=\"4\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"F5\" POSITION=\"19\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"F6\" POSITION=\"20\" LENGTH=\"3\" TYPE=\"Num (Right Justified space padded)\"/>",
		"			</FIELDS>",
		"		</RECORD>",
		"		<RECORD RECORDNAME=\"K3000000\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">",
		"			<TSTFIELDS>",
		"				<TSTFIELD NAME=\"Record_Type\" VALUE=\"K\"/>",
		"               <TSTFIELD NAME=\"K7\" Operator=\"ge\" VALUE=\"300000\"/>",
		"			</TSTFIELDS>",
		"			<FIELDS>",
		"				<FIELD NAME=\"Record_Type\" POSITION=\"1\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K1\" POSITION=\"2\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"Record_Type_2\" POSITION=\"3\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K3\" POSITION=\"4\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K4\" POSITION=\"5\" LENGTH=\"8\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K5\" POSITION=\"13\" LENGTH=\"3\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K6\" POSITION=\"16\" LENGTH=\"4\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K7\" POSITION=\"20\" LENGTH=\"6\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"K8\" POSITION=\"26\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K9\" POSITION=\"27\" LENGTH=\"4\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"K10\" POSITION=\"31\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K11\" POSITION=\"32\" LENGTH=\"3\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"K12\" POSITION=\"35\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"			</FIELDS>",
		"		</RECORD>",
		"		<RECORD RECORDNAME=\"P3000000\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">",
		"			<TSTFIELDS>",
		"				<TSTFIELD NAME=\"Record_Type\" VALUE=\"K\"/>",
		"               <TSTFIELD NAME=\"K7\" Operator=\"le\" VALUE=\"122222\"/>",
		"			</TSTFIELDS>",
		"			<FIELDS>",
		"				<FIELD NAME=\"Record_Type\" POSITION=\"1\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K1\" POSITION=\"2\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"Record_Type_2\" POSITION=\"3\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K3\" POSITION=\"4\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K4\" POSITION=\"5\" LENGTH=\"8\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K5\" POSITION=\"13\" LENGTH=\"3\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K6\" POSITION=\"16\" LENGTH=\"4\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K7\" POSITION=\"20\" LENGTH=\"6\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"K8\" POSITION=\"26\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K9\" POSITION=\"27\" LENGTH=\"4\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"K10\" POSITION=\"31\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"K11\" POSITION=\"32\" LENGTH=\"3\" TYPE=\"Num (Right Justified space padded)\"/>",
		"				<FIELD NAME=\"K12\" POSITION=\"35\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"			</FIELDS>",
		"		</RECORD>",
		"	</RECORDS>",
		"</RECORD>",
	};
	
	public void test1() throws RecordException, Exception {
		int[] selectionFields = {1, 2, 1, 2, 2, 1, 1, 2, 2};
		boolean[] isDefault = {false, false, true, false, false, true, false, false, false};
		LayoutDetail l = getLayout();
		
		assertEquals("Record Count", selectionFields.length, l.getRecordCount());
		
		for (int i = 0; i < selectionFields.length; i++) {
			assertEquals(
					"Selection Field Record " +i, 
					selectionFields[i], 
					l.getRecord(i).getRecordSelection().size());
			assertEquals(
					"Default Check Record " + i, 
					isDefault[i], 
					l.getRecord(i).getRecordSelection().isDefaultRecord());
		}
	};
	
	public void test2() throws RecordException, Exception {
		LayoutDetail l = getLayout();
		Line line;
		
		for (int i = 0; i < LINES.length; i++) {
			line = new Line(l, LINES[i].getBytes());
			assertEquals(
					"Error in line " + i,
					LAYOUT_IDXS[i], line.getPreferredLayoutIdxAlt());
			System.out.print(line.getPreferredLayoutIdxAlt() + ", ");
		}
	}
	
	private LayoutDetail getLayout() throws RecordException, Exception {
		StringBuilder b = new StringBuilder();
			
		for (int i = 0; i < XML_LAYOUT.length; i++) {
			b.append(XML_LAYOUT[i]);
		}
		
		return RecordEditorXmlLoader.getExternalRecord(b.toString(), "Csv Layout").asLayoutDetail();
	}
}
