package net.sf.JRecord.zTest.Cobol.cobolItemTree;

import java.io.IOException;
import java.util.List;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.cgen.def.IArrayAnyDimension;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.detailsBasic.IItemDetails;
import net.sf.JRecord.zTest.Common.TstConstants;

public class TstFieldIsCorrect extends TestCase {

	public static final String[] SINGLE_RECORD_COPBOOKS = {
			"DTAR020", "DTAR107", "FCUSDAT", "MultiGrp-07-HR", "Numeric", "OccursDepending",
			"RBIFCopy", "BitOfEverything01", "RBIVCopy", "MultiGrp-Redef", "Cbl_Line_Test_Record",
			"MultiGrp-01-level"
	};
	
	public static final String[] MULTI_RECORD_COPBOOKS = {
			"MultiGrp-01-level", "BitOfEverything01", "RBIVCopy"
	};
	
	public void testSingleRecordCopybooks() throws IOException {
		for (String c : MULTI_RECORD_COPBOOKS) {
			tstCopybook(c, CopybookLoader.SPLIT_NONE);
		}
	}
	
	
	public void testMultiRecordCopybooks() throws IOException {
		for (String c : SINGLE_RECORD_COPBOOKS) {
			tstCopybook(c, CopybookLoader.SPLIT_01_LEVEL);
		}
	}
	private void  tstCopybook(String c, int split) throws IOException {
		ICobolIOBuilder iob = JRecordInterface1.COBOL
				.newIOBuilder(TstConstants.COBOL_DIRECTORY + c + ".cbl")
					.setSplitCopybook(split);
		
		LayoutDetail layout = iob.getLayout();
		
		for (int i = 0; i < layout.getRecordCount(); i++) {
			RecordDetail record = layout.getRecord(i);
			
			checkCobolTree(c, layout, record, record.getCobolItems());
		}
	}
	
	private void checkCobolTree(String c, LayoutDetail layout, RecordDetail record, List<? extends IItemDetails> cobolItems) {
		
		for (IItemDetails cblItm : cobolItems) {
			List<? extends IItemDetails> childItems = cblItm.getChildItems();
			String fieldName = cblItm.getFieldName();
			if (childItems != null && childItems.size() > 0) {
				checkCobolTree(c, layout, record, childItems);
			} else if ("".equals(fieldName) || "filler".equalsIgnoreCase(fieldName)) {
			} else {
				IFieldDetail field = cblItm.getFieldDefinition();
				if (field != null) {
					assertEquals(fieldName, field.getName());
					assertTrue(field.getLookupName().startsWith(fieldName));
					assertTrue(c + " " + fieldName, field == layout.getFieldFromName(field.getLookupName()));
					
					if (field.getLookupName().equals(fieldName)) {
						assertTrue(c + " " + fieldName, field == record.getField(fieldName));
					}
				} else {
					IArrayAnyDimension arrayDef = cblItm.getArrayDefinition();
					
					switch (arrayDef.getIndexCount()) {
					case 1:
						for (int i = 0; i < arrayDef.getArrayLength(0); i++) {
							chkArrayItem(c, layout, record, arrayDef, arrayDef.getField(i), fieldName + " (" + i + ")");
						}
						break;
					case 2:
						for (int i = 0; i < arrayDef.getArrayLength(0); i++) {
							for (int j = 0; j < arrayDef.getArrayLength(1); j++) {
								chkArrayItem(
										c, layout, record, arrayDef, 
										arrayDef.getField(i, j), fieldName + " (" + i + ", " + j + ")");
							}
						}
						break;
					case 3:
						for (int i = 0; i < arrayDef.getArrayLength(0); i++) {
							for (int j = 0; j < arrayDef.getArrayLength(1); j++) {
								for (int k = 0; k < arrayDef.getArrayLength(2); k++) {
									chkArrayItem(
											c, layout, record, arrayDef, 
											arrayDef.getField(i, j, k), fieldName + " (" + i + ", " + j + ", " + k + ")");
								}
							}
						}
						break;
					}
				}
			}
		}
	}

	/**
	 * @param c
	 * @param layout
	 * @param record
	 * @param arrayDef
	 * @param i
	 * @param n
	 */
	protected void chkArrayItem(String c, LayoutDetail layout, RecordDetail record, IArrayAnyDimension arrayDef, IFieldDetail f,
			String n) {

		assertEquals(n, f.getName());
		IFieldDetail fieldFromName = layout.getFieldFromName(n);
		
		if (fieldFromName == f) {
			assertTrue(n, f == record.getField(n));
		} else {
			assertTrue(f.getLookupName().startsWith(n));
			assertNotSame(n, f.getLookupName());
			//System.out.println(f.getLookupName());
			assertTrue(c + " " + n, f == layout.getFieldFromName(n + f.getLookupName().substring(n.length())));								
		}
	}
}
