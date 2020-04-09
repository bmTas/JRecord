package net.sf.JRecord.zTest.External;

import java.io.IOException;
import java.io.StringReader;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.ExternalRecord.FieldAdjustmentOptions;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Types.Type;

public class Tst_ExternalField_AddFixedWidthField extends TestCase {
	
	private static final int FIELD_COUNT = 6;
	private static final String COPYBOOK
		= "              03  DTAR020-KCODE-STORE-KEY.                        \n"
		+ "                  05 DTAR020-KEYCODE-NO      PIC X(08).           \n"
		+ "                  05 DTAR020-STORE-NO        PIC S9(03)   COMP-3. \n"
		+ "              03  DTAR020-DATE               PIC S9(07)   COMP-3. \n"
		+ "              03  DTAR020-DEPT-NO            PIC S9(03)   COMP-3. \n"
		+ "              03  DTAR020-QTY-SOLD           PIC S9(9)    COMP-3. \n"
		+ "              03  DTAR020-SALE-PRICE         PIC S9(9)V99 COMP-3. \n";
	private static int[] POSITIONS = { 1, 9, 11, 15, 17, 22, 28 };
	
	private static ExternalField[][] expectedNoAdj1 = {{
			CommonTestCode.newFldDtl("name", Type.ftChar, 1, 5, 0),
			CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
			CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
			CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
			CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
			CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
			CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
		}, {
			CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
			CommonTestCode.newFldDtl("name", Type.ftChar, 9, 5, 0),
			CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
			CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
			CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
			CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
			CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
		}, {
			CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
			CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
			CommonTestCode.newFldDtl("name", Type.ftChar, 11, 5, 0),
			CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
			CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
			CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
			CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
		}, {
			CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
			CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
			CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
			CommonTestCode.newFldDtl("name", Type.ftChar, 15, 5, 0),
			CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
			CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
			CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
		}, {
			CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
			CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
			CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
			CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
			CommonTestCode.newFldDtl("name", Type.ftChar, 17, 5, 0),
			CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
			CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
		}, {
			CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
			CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
			CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
			CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
			CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
			CommonTestCode.newFldDtl("name", Type.ftChar, 22, 5, 0),
			CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
		}, {
			CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
			CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
			CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
			CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
			CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
			CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
			CommonTestCode.newFldDtl("name", Type.ftChar, 28, 5, 0),
		}};
	
	private static ExternalField[][] expectedNoAdj2 = {{
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 2, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 10, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 12, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 16, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 18, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
		CommonTestCode.newFldDtl("name", Type.ftChar, 23, 5, 0),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
		CommonTestCode.newFldDtl("name", Type.ftChar, 29, 5, 0),

	}};
	
	private static ExternalField[][] expectedLength1 = {{
		CommonTestCode.newFldDtl("name", Type.ftChar, 1, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 6, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 14, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 16, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 20, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 22, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 27, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 9, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 14, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 16, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 20, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 22, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 27, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 11, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 16, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 20, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 22, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 27, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 15, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 20, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 22, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 27, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 17, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 22, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 27, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 22, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 27, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
		CommonTestCode.newFldDtl("name", Type.ftChar, 28, 5, 0),

	}};
	private static ExternalField[][] expectedLength2 = {{
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 2, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 14, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 16, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 20, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 22, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 27, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 10, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 16, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 20, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 22, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 27, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 12, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 20, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 22, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 27, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 16, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 22, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 27, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 18, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 27, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
		CommonTestCode.newFldDtl("name", Type.ftChar, 23, 5, 0),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
		CommonTestCode.newFldDtl("name", Type.ftChar, 29, 5, 0),

	}};
	
	private static ExternalField[][] expectedCalcAdj2 = {{
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 2, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 10, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 15, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 19, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 21, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 26, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 12, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 17, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 19, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 24, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 16, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 21, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 26, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
		CommonTestCode.newFldDtl("name", Type.ftChar, 18, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 23, 6, 2),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
		CommonTestCode.newFldDtl("name", Type.ftChar, 23, 5, 0),
	}, {
		CommonTestCode.newFldDtl("DTAR020-KEYCODE-NO", Type.ftChar, 1, 8, 0),
		CommonTestCode.newFldDtl("DTAR020-STORE-NO", Type.ftPackedDecimal, 9, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-DATE", Type.ftPackedDecimal, 11, 4, 0),
		CommonTestCode.newFldDtl("DTAR020-DEPT-NO", Type.ftPackedDecimal, 15, 2, 0),
		CommonTestCode.newFldDtl("DTAR020-QTY-SOLD", Type.ftPackedDecimal, 17, 5, 0),
		CommonTestCode.newFldDtl("DTAR020-SALE-PRICE", Type.ftPackedDecimal, 22, 6, 2),
		CommonTestCode.newFldDtl("name", Type.ftChar, 29, 5, 0),

	}};
	
	public void testNoAdjust() throws IOException {
		
		FieldAdjustmentOptions adj = ExternalRecord.FieldAdjustmentOptions.NO_ADJUSTMENT;

		doTest(adj, expectedNoAdj1, 0);
		doTest(adj, expectedNoAdj2, 1);
	}
	public void testLengthAdj() throws IOException {
		
		FieldAdjustmentOptions adj = ExternalRecord.FieldAdjustmentOptions.ADJUST_BY_LENGTH;

		doTest(adj, expectedLength1, 0);
		doTest(adj, expectedLength2, 1);
	}
	public void testCalcAdj() throws IOException {
		
		FieldAdjustmentOptions adj = ExternalRecord.FieldAdjustmentOptions.CALCULATE_ADJUSTMENT;

		doTest(adj, expectedLength1, 0);
		doTest(adj, expectedCalcAdj2, 1);
	}

	private void doTest(FieldAdjustmentOptions adj, ExternalField[][] expected, int adjAmount) throws IOException {

		for (int i = 0; i <= FIELD_COUNT; i++) {
			ExternalRecord r = getXRecord();
			ExternalField fld = new ExternalField(POSITIONS[i] + adjAmount, 5, "name", "", Type.ftChar, 0, 0, "", "", "", 0);
			int idx = r.addFixedWidthField(fld, adj);
			String id = Integer.toString(i);
			
			CommonTestCode.compare(id, expected==null?null:expected[i], r.getRecordFields());
			
			if (adjAmount == 0) {
				assertEquals(id, i, idx);
			}
		}
	}
	
	private static ExternalRecord getXRecord() throws IOException {
		return JRecordInterface1.COBOL
					.newIOBuilder(new StringReader(COPYBOOK), "DTAR020")
						.setDialect(ICopybookDialects.FMT_MAINFRAME)
						.setFileOrganization(Constants.IO_FIXED_LENGTH)
					.getExternalRecord();
	}
}
