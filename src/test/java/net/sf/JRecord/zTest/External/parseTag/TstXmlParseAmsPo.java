package net.sf.JRecord.zTest.External.parseTag;

import org.junit.jupiter.api.Test;

import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.zTest.External.CopybookAccess;
import net.sf.JRecord.zTest.External.test.data.ExternalRecordCheck;
import net.sf.JRecord.zTest.External.test.data.FieldTestDetails;

public class TstXmlParseAmsPo {

	/**
				<FIELD NAME="Record-Type"     POSITION="1" LENGTH="2" TYPE="Char"/>
				<FIELD NAME="Sequence-Number" POSITION="3" LENGTH="5" DECIMAL="3" TYPE="Num Assumed Decimal (Zero padded)"/>
				<FIELD NAME="Vendor"          POSITION="8" LENGTH="10" TYPE="Num (Right Justified zero padded)"/>
				<FIELD NAME="PO"              POSITION="18" LENGTH="12" TYPE="Num Assumed Decimal (Zero padded)"/>
				<FIELD NAME="Entry-Date"      DESCRIPTION="Format YYMMDD" POSITION="30" LENGTH="6" TYPE="Char"/>
				<FIELD NAME="Filler"          POSITION="36" LENGTH="8" TYPE="Char"/>
				<FIELD NAME="beg01-code"      POSITION="44" LENGTH="2" TYPE="Char"/>
				<FIELD NAME="beg02-code"      POSITION="46" LENGTH="2" TYPE="Char"/>
				<FIELD NAME="Department"      POSITION="48" LENGTH="4" TYPE="Char"/>
				<FIELD NAME="Expected-Reciept-Date" DESCRIPTION="Format YYMMDD" POSITION="52" LENGTH="6" TYPE="Char"/>
				<FIELD NAME="Cancel-by-date"  DESCRIPTION="Format YYMMDD" POSITION="58" LENGTH="6" TYPE="Char"/>
				<FIELD NAME="EDI-Type"        POSITION="68" LENGTH="1" TYPE="Char"/>
				<FIELD NAME="Add-Date"        DESCRIPTION="Format YYMMDD" POSITION="69" LENGTH="6" TYPE="Char"/>
				<FIELD NAME="Filler"          POSITION="75" LENGTH="1" TYPE="Char"/>
				<FIELD NAME="Department-Name" POSITION="76" LENGTH="10" TYPE="Char"/>
				<FIELD NAME="Prcoess-Type"    DESCRIPTION="C/N Conveyable/Non-Conveyable" POSITION="86" LENGTH="1" TYPE="Char"/>
				<FIELD NAME="Order-Type"      POSITION="87" LENGTH="2" TYPE="Char"/>

	 */
	private static FieldTestDetails[][] expectedFields = {
		{
				new FieldTestDetails("Record-Type", 1, 2, 0, Type.ftChar),
				new FieldTestDetails("Sequence-Number", 3, 5, 3, Type.ftAssumedDecimalPositive),
				new FieldTestDetails("Vendor", 8, 10, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("PO", 18, 12, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("Entry-Date", 30, 6, 0, Type.ftChar),
				new FieldTestDetails("beg01-code", 44, 2, 0, Type.ftChar),
				new FieldTestDetails("beg02-code", 46, 2, 0, Type.ftChar),
				new FieldTestDetails("Department", 48, 4, 0, Type.ftChar),
				new FieldTestDetails("Expected-Reciept-Date", 52, 6, 0, Type.ftChar),
				new FieldTestDetails("Cancel-by-date", 58, 6, 0, Type.ftChar),
				new FieldTestDetails("EDI-Type", 68, 1, 0, Type.ftChar),
				new FieldTestDetails("Add-Date", 69, 6, 0, Type.ftChar),
				new FieldTestDetails("Department-Name", 76, 10, 0, Type.ftChar),
				new FieldTestDetails("Prcoess-Type", 86, 1, 0, Type.ftChar),
				new FieldTestDetails("Order-Type", 87, 2, 0, Type.ftChar),
			}, {
				new FieldTestDetails("Record-Type", 1, 2, 0, Type.ftChar),
				new FieldTestDetails("Pack-Qty", 3, 9, 4, Type.ftAssumedDecimalPositive),
				new FieldTestDetails("Pack-Cost", 12, 13, 4, Type.ftAssumedDecimalPositive),
				new FieldTestDetails("APN", 25, 13, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("Product", 39, 8, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("pmg-dtl-tech-key", 72, 15, 0, Type.ftChar),
				new FieldTestDetails("Case-Pack-id", 87, 15, 0, Type.ftChar),
				new FieldTestDetails("Product-Name", 102, 50, 0, Type.ftChar),
			}, {
				new FieldTestDetails("Record-Type", 1, 2, 0, Type.ftChar),
				new FieldTestDetails("DC-Number (0)", 3, 4, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("Pack-Quantity (0)", 7, 8, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("DC-Number (1)", 15, 4, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("Pack-Quantity (1)", 19, 8, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("DC-Number (2)", 27, 4, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("Pack-Quantity (2)", 31, 8, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("DC-Number (3)", 39, 4, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("Pack-Quantity (3)", 43, 8, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("DC-Number (4)", 51, 4, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("Pack-Quantity (4)", 55, 8, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("DC-Number (5)", 63, 4, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("Pack-Quantity (5)", 67, 8, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("DC-Number (6)", 75, 4, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("Pack-Quantity (6)", 79, 8, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("DC-Number (7)", 87, 4, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("Pack-Quantity (7)", 91, 8, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("DC-Number (8)", 99, 4, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("Pack-Quantity (8)", 103, 8, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("DC-Number (9)", 111, 4, 0, Type.ftNumZeroPaddedPositive),
				new FieldTestDetails("Pack-Quantity (9)", 115, 8, 0, Type.ftNumZeroPaddedPositive),
			}};
	
	private static String[] expectedRecordSelections = {
			"Record-Type = H1\n",
			"Record-Type = D1\n",
			"Record-Type = S1\n",
	};
	
	@Test
	public void testLoadAmsPo() throws Exception {
		ExternalRecordCheck check = createPoCheck();
		ExternalRecord amsPo = CopybookAccess.getAmsPo();
		
		
		check.checkExternalRecord(amsPo);
		
		check.checkLayout(amsPo.asLayoutDetail());
	}

	@Test
	public void testLoadAmsPo5() throws Exception {
		ExternalRecordCheck check = createPoCheck();
		ExternalRecord amsPo = CopybookAccess.getAmsPo(5);
		
		
		check.checkExternalRecord(amsPo);
		
		check.checkLayout(amsPo.asLayoutDetail());
	}

	
	@Test
	public void testLoadAmsPoRedefine() throws Exception {
		ExternalRecordCheck check = createPoCheck();
		ExternalRecord amsPo = CopybookAccess.getAmsPo(2);
		
		
		check.checkExternalRecord(amsPo);
		
		check.checkLayout(amsPo.asLayoutDetail());
	}
	
	@Test
	public void testLoadAmsPoRedefine6() throws Exception {
		ExternalRecordCheck check = createPoCheck();
		ExternalRecord amsPo = CopybookAccess.getAmsPo(6);
		
		
		check.checkExternalRecord(amsPo);
		
		check.checkLayout(amsPo.asLayoutDetail());
	}
	
	public void testLoadAmsPoMultiParse() throws Exception {
		ExternalRecordCheck check = createPoCheck();
		ExternalRecord amsPo = CopybookAccess.getAmsPo(3);
		
		
		check.checkExternalRecord(amsPo);
		
		check.checkLayout(amsPo.asLayoutDetail());
	}

	
	public void testLoadXmlAmsPo() throws Exception {
		ExternalRecordCheck check = createPoCheck();
		ExternalRecord amsPo = CopybookAccess.getXmlAmsPo();
		
		
		check.checkExternalRecord(amsPo);
		
		check.checkLayout(amsPo.asLayoutDetail());
	}

	
	public void testLoadXmlAmsPo2() throws Exception {
		ExternalRecordCheck check = createPoCheck();
		ExternalRecord amsPo = CopybookAccess.getXmlAmsPo2();
		
		
		check.checkExternalRecord(amsPo);
		
		check.checkLayout(amsPo.asLayoutDetail());
	}

	private ExternalRecordCheck createPoCheck() {
		return new ExternalRecordCheck("", IFileStructureConstants.IO_STANDARD_TEXT_FILE, 
				expectedFields, expectedRecordSelections);
	}

}
