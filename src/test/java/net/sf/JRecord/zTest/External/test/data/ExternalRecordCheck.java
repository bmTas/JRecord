package net.sf.JRecord.zTest.External.test.data;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.base.IChildRecord;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.ExternalRecordSelection.IExternalSelectionField;
import net.sf.JRecord.ExternalRecordSelection.IExternalSelectionGroup;

public class ExternalRecordCheck {
	
	private final String encoding;
	private final int fileStructure;

	private final FieldTestDetails[][] expectedFields;
	private final String[] expectedRecordSelections;

	public ExternalRecordCheck(String encoding, int fileStructure, 
			FieldTestDetails[][] expectedFields,  String[] expectedRecordSelections) {
		super();
		this.encoding = encoding;
		this.fileStructure = fileStructure;
		this.expectedFields = expectedFields;
		this.expectedRecordSelections = expectedRecordSelections;
	}
	
	public void checkExternalRecord (ExternalRecord record) {
//		TestCase.assertEquals(fileStructure, record.getFileStructure());
		assertEquals(encoding, record.getFontName());

		assertEquals(expectedFields.length, record.getNumberOfRecords());
		for (int recNumber = 0; recNumber < expectedFields.length; recNumber++) {
			IChildRecord<ExternalRecord> childRecordDtls = record.getChildRecord(recNumber);
			ExternalRecord childRecord = childRecordDtls.getExternalRecord();
			String id = recNumber + " " + childRecord.getRecordName();
			assertEquals(expectedFields[recNumber].length, childRecord.getNumberOfRecordFields(), id);
			for (int fieldNumber = 0; fieldNumber < expectedFields[recNumber].length; fieldNumber++) {
				expectedFields[recNumber][fieldNumber].checkExternalField(childRecord.getRecordName(), childRecord.getRecordField(fieldNumber));
			}
			
			StringBuilder sb = new StringBuilder();
			printSelection(sb, 0, childRecordDtls.getRecordSelection());
			//System.out.println("\"" + sb + "\",");
			String expected = expectedRecordSelections[recNumber];
			String actual = sb.toString();

			assertEquals(expected, actual);
		}
	}
	
	public void checkLayout (LayoutDetail schema) {
		assertEquals(expectedFields.length, schema.getRecordCount());
		assertEquals(encoding, schema.getFontName());

		int layoutFileStructure = schema.getFileStructure();
		if (fileStructure == layoutFileStructure
		|| (  fileStructure == IFileStructureConstants.IO_STANDARD_TEXT_FILE 
		   && layoutFileStructure == IFileStructureConstants.IO_STANDARD_UNICODE_TEXT_FILE)) {
			
		} else {
//			TestCase.assertEquals(fileStructure, layoutFileStructure);
		}

		for (int recNumber = 0; recNumber < expectedFields.length; recNumber++) {
			RecordDetail record = schema.getRecord(recNumber);
			for (int i = 0; i < expectedFields[recNumber].length; i++) {
				FieldDetail field = record.getField(i);
				expectedFields[recNumber][i].checkField(field);
				assertEquals(encoding, field.getFontName());
				assertTrue(field.isFixedFormat());
			}
			StringBuilder sb = new StringBuilder();
			printSelection(sb, 0, record.getRecordSelection().getRecSel());
		}

	}
	
	static void printSelection(StringBuilder sb, int level,  ExternalSelection recSelection) {
		for (int i = 0 ; i < level; i++) {
			sb.append("      ");
		}
		if (recSelection instanceof IExternalSelectionField) {
			IExternalSelectionField sf = (IExternalSelectionField) recSelection;
			sb.append(sf.getFieldName() + " " + sf.getOperator() + " " + sf.getFieldValue() + "\n");
		} else if (recSelection instanceof IExternalSelectionGroup) {
			IExternalSelectionGroup gf = (IExternalSelectionGroup) recSelection;
			sb.append((gf.getType() == IExternalSelectionGroup.TYPE_AND) ? "and\n" : "or\n");
			for (int i = 0; i < gf.getSize(); i++) {
				printSelection(sb, level+1, gf.get(i));
			}
		}
	}
}
