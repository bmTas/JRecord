package net.sf.JRecord.zTest.Cobol.cobolItemTree;

import java.io.IOException;
import java.io.StringReader;
import java.util.List;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.cgen.def.IArrayAnyDimension;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.detailsBasic.IItemDetails;
import net.sf.JRecord.zTest.Common.TstConstants;

public class TstFieldIsCorrect extends TestCase {

	private static final String D107_CPY
			= "001300     03  DTAR107-STORE-NO               PIC S9(03)    COMP-3.\n"
			+ "001400     03  DTAR107-TRANS-DATE             PIC S9(06)    COMP-3.\n"
			+ "001500     03  DTAR107-CUST-NO                PIC 9(16).\n"
			+ "001600     03  DTAR107-AMOUNT                 PIC S9(07)V99 COMP-3.\n"
			+ "001700     03  DTAR107-OPERATOR-NO            PIC S9(08)    COMP-3.\n"
			+ "001800     03  DTAR107-TERMINAL-NO            PIC S9(03)    COMP-3.\n"
			+ "001900     03  DTAR107-TIME                   PIC S9(04)    COMP-3.\n"
			+ "002000     03  DTAR107-TRANS-NO               PIC S9(04)    COMP-3.\n"
			+ "002100     03  DTAR107-TRANS-TYPE             PIC 9(02).\n"
			+ "002200         88 DTAR107-SALE                   VALUE 1.\n"
			+ "002300         88 DTAR107-REFUND                 VALUE 2.\n"
			+ "002400         88 DTAR107-LAYBY                  VALUE 3.\n"
			+ "002500         88 DTAR107-VOID                   VALUE 4.\n"
			+ "002600     03  DTAR107-TRANS-CODE             PIC 9(02).\n"
			+ "002700         88 DTAR107-SALE-DR                VALUE 10.\n"
			+ "002800         88 DTAR107-REFUND-CR              VALUE 20.\n"
			+ "002900         88 DTAR107-DR-REVERSAL            VALUE 12.\n"
			+ "003000         88 DTAR107-CR-REVERSAL            VALUE 22.\n"
			+ "003100     03  DTAR107-STD-POINTS             PIC S9(06)    COMP-3.\n"
			+ "003200     03  DTAR107-BONUS-POINTS           PIC S9(06)    COMP-3.\n"
			+ "003300     03  DTAR107-NO-OF-TXNS             PIC 9(02).\n";
		
	public static final String[] SINGLE_RECORD_COPBOOKS = {
			"DTAR020", "DTAR107", "FCUSDAT", "MultiGrp-07-HR", "Numeric", "OccursDepending",
			"RBIFCopy", "BitOfEverything01", "RBIVCopy", "MultiGrp-Redef", "Cbl_Line_Test_Record",
			"MultiGrp-01-level", "MultiGrp-05-level", "MultiGrp-07-HR"
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
	
	public void testNumeric() throws IOException {
		LayoutDetail layout = JRecordInterface1.COBOL
			.newIOBuilder(TstConstants.COBOL_DIRECTORY + "Numeric.cbl")
			.getLayout();
		
		checkCobolAgainstFields(layout, layout.getRecord(0).getCobolItems().get(0).getChildItems(), 1);
	}

	
	public void testComp() throws IOException {
		LayoutDetail layout = JRecordInterface1.COBOL
			.newIOBuilder(TstConstants.COBOL_DIRECTORY2 + "cpyComp5positive.cbl")
			.getLayout();
		
		checkCobolAgainstFields(layout, layout.getRecord(0).getCobolItems().get(0).getChildItems(), 1);
	}
	
	public void testCblLine() throws IOException {
		LayoutDetail layout = JRecordInterface1.COBOL
			.newIOBuilder(TstConstants.COBOL_DIRECTORY + "Cbl_Line_Test_Record.cbl")
			.getLayout();
		
		checkCobolAgainstFields(layout, layout.getRecord(0).getCobolItems().get(0).getChildItems(), 2);
	}
	
	public void testDTAR107() throws IOException {
		LayoutDetail layout = JRecordInterface1.COBOL
			.newIOBuilder(new StringReader(D107_CPY), "DTAR107")
			.getLayout();
		
		checkCobolAgainstFields(layout, layout.getRecord(0).getCobolItems(), 1);
	}

	private void checkCobolAgainstFields(LayoutDetail layout, List<? extends IItemDetails> cobolItems, int max) {
		for (int i = 0; i < cobolItems.size(); i++) {
			IItemDetails cblItem = cobolItems.get(i);
			FieldDetail field = layout.getField(0, i);
			String id = i + " " + cblItem.getFieldName() + " " + field.getName();
			assertTrue(id, cblItem.getFieldDefinition() == field);
			assertTrue(id, cblItem == field.getCobolItem());
			
			List<IItemDetails> lookupCobolItems = layout.getCobolItems(field.getName());
			
			assertTrue(id, lookupCobolItems != null && lookupCobolItems.size() > 0 );
			assertTrue(id, lookupCobolItems.size() <= max);

			assertTrue(id, contains(lookupCobolItems, cblItem));
		}
	}
	
	private boolean contains(List<IItemDetails> lookupCobolItems, IItemDetails cblItem) {
		if (lookupCobolItems != null) {
			for (IItemDetails c : lookupCobolItems) {
				if (c == cblItem) {
					return true;
				}
			}
		}
		return false;
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
			
			assertTrue(fieldName, contains(layout.getCobolItems(fieldName), cblItm));

			if (childItems != null && childItems.size() > 0) {
				checkCobolTree(c, layout, record, childItems);
				assertTrue(fieldName, contains(layout.getCobolGroupItems(fieldName), cblItm));
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
					assertTrue(field.getCobolItem() == cblItm);
				} else {
					IArrayAnyDimension arrayDef = cblItm.getArrayDefinition();
					
					switch (arrayDef.getIndexCount()) {
					case 1:
						for (int i = 0; i < arrayDef.getArrayLength(0); i++) {
							IFieldDetail arrayField = arrayDef.getField(i);
							chkArrayItem(c, layout, record, arrayDef, arrayField, fieldName + " (" + i + ")");
							assertTrue(arrayField.getCobolItem() == cblItm);
						}
						break;
					case 2:
						for (int i = 0; i < arrayDef.getArrayLength(0); i++) {
							for (int j = 0; j < arrayDef.getArrayLength(1); j++) {
								IFieldDetail arrayField = arrayDef.getField(i, j);
								chkArrayItem(
										c, layout, record, arrayDef, 
										arrayField, fieldName + " (" + i + ", " + j + ")");
							}
						}
						break;
					case 3:
						for (int i = 0; i < arrayDef.getArrayLength(0); i++) {
							for (int j = 0; j < arrayDef.getArrayLength(1); j++) {
								for (int k = 0; k < arrayDef.getArrayLength(2); k++) {
									IFieldDetail arrayField = arrayDef.getField(i, j, k);
									chkArrayItem(
											c, layout, record, arrayDef, 
											arrayField, fieldName + " (" + i + ", " + j + ", " + k + ")");
									assertTrue(arrayField.getCobolItem() == cblItm);
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
