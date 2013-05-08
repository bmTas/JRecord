package net.sf.JRecord.zTest.External;

import junit.framework.TestCase;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.RecordEditorXmlLoader;

public class TstMultiFieldCheck4  extends TestCase {

	private final static int[] LAYOUT_IDXS = {
		5, 4, 0, 1, 2, 1, 0, 0,
		4, 0, 1, 2, 3,
	};
	private final static String[] LINES = {
		"H HD2010010100000001",
		"P HD121 00012121",
		"P 0121  0000002223 ",
		"P 0221  0000002223 ",
		"P 0521  0000002223 ",
		"P 0221  0000002223 ",
		"P 0121  0000002223 ",
		"P 0121  0000002223 ",
		
		"P HD121 00012121",
		"P 0121  0000002223 ",
		"P 0221  0000002223 ",
		"P 0521  0000002223 ",
		"T TR11  0000000000000123",
	};
	
	
	private final static String[] XML_LAYOUT = {
		"<?xml version=\"1.0\" ?>",
		"<RECORD RECORDNAME=\"ww1File\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" DESCRIPTION=\"ww File Def\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"GroupOfRecords\" LIST=\"Y\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">",
		"	<RECORDS>",
		"		<RECORD RECORDNAME=\"wwProd01\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" DESCRIPTION=\"Prod 01 Record\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" PARENT=\"wwProdHead\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">",
		"			<TSTFIELDS>",
		"				<AND>",
		"					<TSTFIELD NAME=\"RecordType1\" VALUE=\"P\"/>",
		"					<TSTFIELD NAME=\"RecordType2\" VALUE=\"01\"/>",
		"				</AND>",
		"			</TSTFIELDS>",
		"			<FIELDS>",
		"				<FIELD NAME=\"RecordType1\"  POSITION=\"1\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"RecordType2\"  POSITION=\"3\" LENGTH=\"2\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"Field11\"  POSITION=\"5\" LENGTH=\"4\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"Field12\" POSITION=\"9\" LENGTH=\"8\" TYPE=\"Num (Right Justified zero padded)\"/>",
		"				<FIELD NAME=\"Field13\"  POSITION=\"17\" LENGTH=\"3\" TYPE=\"Char\"/>",
		"			</FIELDS>",
		"		</RECORD>",
		"		<RECORD RECORDNAME=\"wwProd02\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" PARENT=\"wwProdHead\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">",
		"			<TSTFIELDS>",
		"				<AND>",
		"					<TSTFIELD NAME=\"RecordType1\" VALUE=\"P\"/>",
		"					<TSTFIELD NAME=\"RecordType2\" VALUE=\"02\"/>",
		"				</AND>",
		"			</TSTFIELDS>",
		"			<FIELDS>",
		"				<FIELD NAME=\"RecordType1\"  POSITION=\"1\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"RecordType2\"  POSITION=\"3\" LENGTH=\"2\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"Field21\"  POSITION=\"5\" LENGTH=\"4\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"Field22\"  POSITION=\"9\" LENGTH=\"8\" TYPE=\"Num (Right Justified zero padded)\"/>",
		"				<FIELD NAME=\"Field23\"  POSITION=\"17\" LENGTH=\"3\" TYPE=\"Char\"/>",
		"			</FIELDS>",
		"		</RECORD>",
		"		<RECORD RECORDNAME=\"wwProd05\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" PARENT=\"wwProdHead\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">",
		"			<TSTFIELDS>",
		"				<AND>",
		"					<TSTFIELD NAME=\"RecordType1\" VALUE=\"P\"/>",
		"					<TSTFIELD NAME=\"RecordType2\" VALUE=\"05\"/>",
		"				</AND>",
		"			</TSTFIELDS>",
		"			<FIELDS>",
		"				<FIELD NAME=\"RecordType1\"  POSITION=\"1\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"RecordType2\"  POSITION=\"3\" LENGTH=\"2\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"Field51\"  POSITION=\"5\" LENGTH=\"6\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"Field52\"  POSITION=\"11\" LENGTH=\"9\" TYPE=\"Num (Right Justified zero padded)\"/>",
		"				<FIELD NAME=\"Field53\"  POSITION=\"26\" LENGTH=\"3\" TYPE=\"Char\"/>",
		"			</FIELDS>",
		"		</RECORD>",
		"		<RECORD RECORDNAME=\"wwTrailer\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" PARENT=\"wwHeader\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">",
		"			<TSTFIELDS>",
		"				<AND>",
		"					<TSTFIELD NAME=\"RecordType1\" VALUE=\"T\"/>",
		"					<TSTFIELD NAME=\"RecordType2\" VALUE=\"TR\"/>",
		"				</AND>",
		"			</TSTFIELDS>",
		"			<FIELDS>",
		"				<FIELD NAME=\"RecordType1\"  POSITION=\"1\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"RecordType2\"  POSITION=\"3\" LENGTH=\"2\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"Count\" POSITION=\"17\" LENGTH=\"8\" TYPE=\"Num (Right Justified zero padded)\"/>",
		"			</FIELDS>",
		"		</RECORD>",
		"		<RECORD RECORDNAME=\"wwProdHead\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">",
		"			<TSTFIELDS>",
		"				<AND>",
		"					<TSTFIELD NAME=\"RecordType1\" VALUE=\"P\"/>",
		"					<TSTFIELD NAME=\"RecordType2\" VALUE=\"HD\"/>",
		"				</AND>",
		"			</TSTFIELDS>",
		"			<FIELDS>",
		"				<FIELD NAME=\"RecordType1\"  POSITION=\"1\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"RecordType2\"  POSITION=\"3\" LENGTH=\"2\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"Dept\"  POSITION=\"5\" LENGTH=\"4\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"Product\"  POSITION=\"9\" LENGTH=\"8\" TYPE=\"Num (Right Justified zero padded)\"/>",
		"			</FIELDS>",
		"		</RECORD>",
		"		<RECORD RECORDNAME=\"wwHeader\" COPYBOOK=\"\" DELIMITER=\"&lt;Tab&gt;\" DESCRIPTION=\"File Header\" FILESTRUCTURE=\"Default\" STYLE=\"0\" RECORDTYPE=\"RecordLayout\" LIST=\"Y\" PARENT=\"wwHeader\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">",
		"			<TSTFIELDS>",
		"				<AND>",
		"					<TSTFIELD NAME=\"RecordType1\" VALUE=\"H\"/>",
		"					<TSTFIELD NAME=\"RecordType2\" VALUE=\"HD\"/>",
		"				</AND>",
		"			</TSTFIELDS>",
		"			<FIELDS>",
		"				<FIELD NAME=\"RecordType1\"  POSITION=\"1\" LENGTH=\"1\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"RecordType2\"  POSITION=\"3\" LENGTH=\"2\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"RunDate\" POSITION=\"5\" LENGTH=\"8\" TYPE=\"Char\"/>",
		"				<FIELD NAME=\"RunNumber\"  POSITION=\"13\" LENGTH=\"8\" TYPE=\"Num (Right Justified zero padded)\"/>",
		"			</FIELDS>",
		"		</RECORD>",
		"	</RECORDS>",
		"</RECORD>",
		

	};
	
	public void test1() throws RecordException, Exception {
		//int[] selectionFields = {1, 2, 1, 2, 2, 1, 1, 2, 2};
		LayoutDetail l = getLayout();
				
		for (int i = 0; i <  l.getRecordCount(); i++) {
			assertEquals(
					"Selection Field Record " + i, 
					2, 
					l.getRecord(i).getRecordSelection().size());
			assertEquals(
					"Default Check Record " + i, 
					false, 
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
	
	public void testLayoutParent() throws Exception {
		int[] parentIdxs = {4, 4, 4, 5, -1, 5,};
		String[] parentNames = {"wwProdHead", "wwProdHead", "wwProdHead", "wwHeader", "", "wwHeader"};
		ExternalRecord xRec = getExternalLayout();
		int parent;
		String s;
				
		System.out.println();
		for (int i = 0; i < xRec.getNumberOfRecords(); i++) {
			s = "";
			parent = xRec.getRecord(i).getParentRecord();
			if (parent >= 0) {
				s = xRec.getRecord(parent).getRecordName();
			}
			System.out.println("Rec: " + xRec.getRecord(i).getRecordName() 
					+ "\tParent Idx: " + parent
					+ "\tName: " + s);
		}
		
		assertEquals("Record Count ", parentIdxs.length,  xRec.getNumberOfRecords());
		for (int i = 0; i < xRec.getNumberOfRecords(); i++) {
			s = "";
			parent = xRec.getRecord(i).getParentRecord();
			if (parent >= 0) {
				s = xRec.getRecord(parent).getRecordName();
			}
			assertEquals("Parent Name" , parentNames[i], s);
			assertEquals("Parent Index", parentIdxs[i], parent);
		}
	}
	
	public void testLayoutFieldNames() throws Exception {
		String[][] fieldNames = {
				{"RecordType1", "RecordType2", "Field11", "Field12", "Field13", },
				{"RecordType1", "RecordType2", "Field21", "Field22", "Field23", },
				{"RecordType1", "RecordType2", "Field51", "Field52", "Field53", },
				{"RecordType1", "RecordType2", "Count", },
				{"RecordType1", "RecordType2", "Dept", "Product", },
				{"RecordType1", "RecordType2", "RunDate", "RunNumber", },
		};
		ExternalRecord cRec;
		ExternalRecord xRec = getExternalLayout();
		for (int i = 0; i < xRec.getNumberOfRecords(); i++) {
			cRec = xRec.getRecord(i);
			System.out.print("\t{");
			for (int j = 0; j < cRec.getNumberOfRecordFields(); j++) {
				System.out.print("\"" + cRec.getRecordField(j).getName() + "\", ");
			}
			System.out.println("},");
		}
		
		assertEquals("Record Count ", fieldNames.length, xRec.getNumberOfRecords());
		for (int i = 0; i < xRec.getNumberOfRecords(); i++) {
			cRec = xRec.getRecord(i);
			
			assertEquals("Field Count ", fieldNames[i].length, cRec.getNumberOfRecordFields());
			for (int j = 0; j < cRec.getNumberOfRecordFields(); j++) {
				assertEquals("Field " + i + ", " + j + " ", cRec.getRecordField(j).getName(), fieldNames[i][j]);
			}
		}
	}

	public void testLayoutFieldPos() throws Exception {
		int[][] fieldPos = {
				{1, 3, 5, 9, 17, },
				{1, 3, 5, 9, 17, },
				{1, 3, 5, 11, 26, },
				{1, 3, 17, },
				{1, 3, 5, 9, },
				{1, 3, 5, 13, },
		};
		ExternalRecord cRec;
		ExternalRecord xRec = getExternalLayout();
		for (int i = 0; i < xRec.getNumberOfRecords(); i++) {
			cRec = xRec.getRecord(i);
			System.out.print("\t{");
			for (int j = 0; j < cRec.getNumberOfRecordFields(); j++) {
				System.out.print( cRec.getRecordField(j).getPos() + ", ");
			}
			System.out.println("},");
		}
	
		assertEquals("Record Count ", fieldPos.length, xRec.getNumberOfRecords());
		for (int i = 0; i < xRec.getNumberOfRecords(); i++) {
			cRec = xRec.getRecord(i);
			
			assertEquals("Field Count ", fieldPos[i].length, cRec.getNumberOfRecordFields());
			for (int j = 0; j < cRec.getNumberOfRecordFields(); j++) {
				assertEquals("Field " + i + ", " + j + " ", cRec.getRecordField(j).getPos(), fieldPos[i][j]);
			}
		}
	}


	public void testLayoutFieldLength() throws Exception {
		int[][] fieldLen = {
				{1, 2, 4, 8, 3, },
				{1, 2, 4, 8, 3, },
				{1, 2, 6, 9, 3, },
				{1, 2, 8, },
				{1, 2, 4, 8, },
				{1, 2, 8, 8, },
		};
		ExternalRecord cRec;
		ExternalRecord xRec = getExternalLayout();
		for (int i = 0; i < xRec.getNumberOfRecords(); i++) {
			cRec = xRec.getRecord(i);
			System.out.print("\t{");
			for (int j = 0; j < cRec.getNumberOfRecordFields(); j++) {
				System.out.print( cRec.getRecordField(j).getLen() + ", ");
			}
			System.out.println("},");
		}
	
		assertEquals("Record Count ", fieldLen.length, xRec.getNumberOfRecords());
		for (int i = 0; i < xRec.getNumberOfRecords(); i++) {
			cRec = xRec.getRecord(i);
			
			assertEquals("Field Count ", fieldLen[i].length, cRec.getNumberOfRecordFields());
			for (int j = 0; j < cRec.getNumberOfRecordFields(); j++) {
				assertEquals("Field Length " + i + ", " + j + " ", cRec.getRecordField(j).getLen(), fieldLen[i][j]);
			}
		}
	}
	

	public void testLayoutFieldType() throws Exception {
		int[][] fieldType = {
				{0, 0, 0, 7, 0, },
				{0, 0, 0, 7, 0, },
				{0, 0, 0, 7, 0, },
				{0, 0, 7, },
				{0, 0, 0, 7, },
				{0, 0, 0, 7, },
		};
		ExternalRecord cRec;
		ExternalRecord xRec = getExternalLayout();
		for (int i = 0; i < xRec.getNumberOfRecords(); i++) {
			cRec = xRec.getRecord(i);
			System.out.print("\t{");
			for (int j = 0; j < cRec.getNumberOfRecordFields(); j++) {
				System.out.print( cRec.getRecordField(j).getType() + ", ");
			}
			System.out.println("},");
		}
	
		assertEquals("Record Count ", fieldType.length, xRec.getNumberOfRecords());
		for (int i = 0; i < xRec.getNumberOfRecords(); i++) {
			cRec = xRec.getRecord(i);
			
			assertEquals("Field Count ", fieldType[i].length, cRec.getNumberOfRecordFields());
			for (int j = 0; j < cRec.getNumberOfRecordFields(); j++) {
				assertEquals("Field Type " + i + ", " + j + " ", cRec.getRecordField(j).getType(), fieldType[i][j]);
			}
		}
	}


	private LayoutDetail getLayout() throws RecordException, Exception {		
		return getExternalLayout().asLayoutDetail();
	}
	
	private ExternalRecord getExternalLayout() throws Exception {
		StringBuilder b = new StringBuilder();
		
		for (int i = 0; i < XML_LAYOUT.length; i++) {
			b.append(XML_LAYOUT[i]);
		}
		
		return RecordEditorXmlLoader.getExternalRecord(b.toString(), "Csv Layout");
	}
}
