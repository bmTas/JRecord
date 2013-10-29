package net.sf.JRecord.zTest.Details;

import java.util.Set;
import java.util.TreeMap;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Numeric.Convert;
import net.sf.JRecord.zTest.Common.TestCommonCode;
import junit.framework.TestCase;

public class TstFieldNameDuplicates extends TestCase {
	private String cobolCopybook
			= "      01 COMPANY-RECORD.\n"
			+ "         05 COMPANY-NAME     PIC X(30).\n"
			+ "         05 EMPLOYEE-LIST.\n"
			+ "            10 PRESIDENT.\n"
			+ "               15 LAST-NAME  PIC X(15).\n"
			+ "               15 FIRST-NAME PIC X(8).\n"
			+ "            10 VICE-PRESIDENT.\n"
			+ "               15 LAST-NAME  PIC X(15).\n"
			+ "               15 FIRST-NAME PIC X(8).\n"
			+ "            10 OTHERS.\n"
			+ "               15 TITLE      PIC X(10).\n"
			+ "               15 LAST-NAME  PIC X(15).\n"
			+ "               15 FIRST-NAME PIC X(8).\n"
			+ "         05 ADDRESS          PIC X(15).\n"
			+ "         05 CITY             PIC X(15).\n"
			+ "         05 STATE            PIC XX.\n"
			+ "         05 ZIP              PIC 9(5).\n";

	private String[] fieldNames = {
			"ADDRESS",
			"CITY",
			"COMPANY-NAME",
			"FIRST-NAME",
			"FIRST-NAME~1",
			"FIRST-NAME~2",
			"LAST-NAME",
			"LAST-NAME~1",
			"LAST-NAME~2",
			"STATE",
			"TITLE",
			"ZIP",
	};

	int[][] fieldDetails = {
			{110, 15, 0},
			{125, 15, 0},
			{1, 30, 0},
			{46, 8, 0},
			{69, 8, 0},
			{102, 8, 0},
			{31, 15, 0},
			{54, 15, 0},
			{87, 15, 0},
			{140, 2, 0},
			{77, 10, 0},
			{142, 5, 25},
	};

	public void testFieldNames() throws RecordException {
		LayoutDetail l = TestCommonCode.getLayoutFromCobolStr(cobolCopybook, "COMPANY-RECORD",
				CopybookLoader.SPLIT_NONE, "", Convert.FMT_INTEL);

		TreeMap<String, IFieldDetail> fm = new TreeMap<String, IFieldDetail>(l.getFieldNameMap());
		Set<String> keySet = fm.keySet();

//		for (String s : keySet) {
//			System.out.println("\t" + s + ",");
//		}
//
//		for (String s : keySet) {
//			IFieldDetail fieldDetail = fm.get(s);
//			System.out.println("\t{" + fieldDetail.getPos() + ", " + fieldDetail.getLen()+ ", " + fieldDetail.getType() + "}, ");
//		}

		int i = 0;
		for (String s : keySet) {
			IFieldDetail fieldDetail = fm.get(s);

			assertEquals("check Field Name", fieldNames[i], s);
			assertEquals("check Field pos", fieldDetails[i][0], fieldDetail.getPos());
			assertEquals("check Field Length", fieldDetails[i][1], fieldDetail.getLen());
			assertEquals("check Field Type", fieldDetails[i][2], fieldDetail.getType());
			i += 1;
		}

	}
}
