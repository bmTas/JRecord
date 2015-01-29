package net.sf.JRecord.zTest.External;

import junit.framework.TestCase;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.Convert;

public class TstLoadCobolCopybook  extends TestCase {

	private String copybookFileName = "CopybookWithHex88.cbl";
	
	private String[] fldNames = {
			"CB-SORT-CODE",
			"STATISTICAL-IND",
			"HDR-PORTION-LENGTH",
			"POL-BODY-LENGTH",
			"XBOLG-HEX",
			"OVERFLOW-LENGTH",
			"1st-byte",
			"2nd-byte",
			"ARRANGEMENT-TYPE",
			"1-field",
			"ARRANGEMENT-NUMB",
			"PAY-POINT-NUMB",
			"XRCCT",
			"RECORD-COUNT",
			"CSO",
			"STATUS-CD",	
	};
	
	private int[][] fldAttrs = {
			{0, 1, 1},
			{0, 2, 1},
			{0, 3, 2},
			{0, 5, 2},
			{39, 5, 2},
			{0, 7, 2},
			{25, 7, 1},
			{25, 8, 1},
			{0, 9, 1},
			{25, 9, 1},
			{33, 10, 3},
			{33, 13, 2},
			{33, 15, 1},
			{0, 16, 1},
			{0, 17, 1},
			{0, 18, 1},
	};
	
	public void testLoadCopybook() throws RecordException {
		
    	String copyName = this.getClass().getResource(copybookFileName).getFile();
    	CobolCopybookLoader loaderCBL = new CobolCopybookLoader();
    	ExternalRecord extlayoutCBL = loaderCBL.loadCopyBook(
    			copyName, CopybookLoader.SPLIT_NONE, 0,
				/* Font name */"", Convert.FMT_MAINFRAME, 0, new TextLog());
    	
    	
    	assertEquals(fldNames.length, extlayoutCBL.getNumberOfRecordFields());
    	for (int i = 0; i < extlayoutCBL.getNumberOfRecordFields(); i++) {
    		ExternalField fld = extlayoutCBL.getRecordField(i);
			String id = "FldNo=" + i + ", name=" + fldNames[i];
			assertEquals(id, fldNames[i], fld.getName());
			assertEquals(id, fldAttrs[i][0], fld.getType());
			assertEquals(id, fldAttrs[i][1], fld.getPos());
			assertEquals(id, fldAttrs[i][2], fld.getLen());
    	}

    	
//    	System.out.println(" ===> " + extlayoutCBL.getNumberOfRecordFields() + ", " + extlayoutCBL.getNumberOfRecords() );
//    	for (int i = 0; i < extlayoutCBL.getNumberOfRecordFields(); i++) {
//    		System.out.println("\t\"" + extlayoutCBL.getRecordField(i).getName() + "\",");
//    	}
//    	for (int i = 0; i < extlayoutCBL.getNumberOfRecordFields(); i++) {
//    		ExternalField fld = extlayoutCBL.getRecordField(i);
//			System.out.println("\t{" + fld.getType() + ", " + fld.getPos() + ", " + fld.getLen() + "},");
//    	}
	}
}
