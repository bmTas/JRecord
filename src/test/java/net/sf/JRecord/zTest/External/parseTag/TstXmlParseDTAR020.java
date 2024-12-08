package net.sf.JRecord.zTest.External.parseTag;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.zTest.External.CopybookAccess;
import net.sf.JRecord.zTest.External.test.data.ExternalRecordCheck;
import net.sf.JRecord.zTest.External.test.data.FieldTestDetails;

public class TstXmlParseDTAR020 {

	private final static FieldTestDetails[] DTAR020_FIELDS = {
			new FieldTestDetails("KEYCODE-NO", 1, 8, 0, Type.ftChar),
			new FieldTestDetails("STORE-NO", 9, 2, 0, Type.ftPackedDecimal),
			new FieldTestDetails("DATE", 11, 4, 0, Type.ftPackedDecimal),
			new FieldTestDetails("DEPT-NO", 15, 2, 0, Type.ftPackedDecimal),
			new FieldTestDetails("QTY-SOLD", 17, 5, 0, Type.ftPackedDecimal),
			new FieldTestDetails("SALE-PRICE", 22, 6, 2, Type.ftPackedDecimal),
		};
	

	
	@Test
	public void testLoadDTAR020() throws Exception {
		FieldTestDetails[][] expectedFields = {DTAR020_FIELDS};
		String[] expectedSelection= {""};
		ExternalRecordCheck check = new ExternalRecordCheck("CP037", IFileStructureConstants.IO_FIXED_LENGTH, 
				expectedFields, expectedSelection);
		
		ExternalRecord dtar020 = CopybookAccess.getDTAR020();
//		FieldTestDetails.printRecord(dtar020);
		
		assertEquals(ICopybookDialects.FMT_MAINFRAME, dtar020.getDialectCode());
		check.checkExternalRecord(dtar020);
		check.checkLayout(dtar020.asLayoutDetail());
	}
	
	
	@Test
	public void testLoadDTAR020a() throws Exception {
		FieldTestDetails[][] expectedFields = {DTAR020_FIELDS};
		String[] expectedSelection= {""};
		ExternalRecordCheck check = new ExternalRecordCheck("CP037", IFileStructureConstants.IO_FIXED_LENGTH, 
				expectedFields, expectedSelection);
		
		ExternalRecord dtar020 = CopybookAccess.getDTAR020a();
//		FieldTestDetails.printRecord(dtar020);
		
		assertEquals(ICopybookDialects.FMT_MAINFRAME, dtar020.getDialectCode());
		//check.checkExternalRecord(dtar020);
		
		int length = Math.min(DTAR020_FIELDS.length, dtar020.getNumberOfRecordFields());
		for (int fieldNumber = 0; fieldNumber < length; fieldNumber++) {
			DTAR020_FIELDS[fieldNumber].checkExternalField(dtar020.getRecordName(), dtar020.getRecordField(fieldNumber));
		}
		assertEquals(DTAR020_FIELDS.length, dtar020.getNumberOfRecordFields());
		
		
		check.checkLayout(dtar020.asLayoutDetail());
	}
	
}
