package net.sf.JRecord.zTest.Cobol;

import java.io.ByteArrayInputStream;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.Convert;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.Types.TypeNum;

import junit.framework.TestCase;

/**
 * This class checks the type and positive attribute for
 * various Cobol Numeric Types
 *
 * @author Bruce Martin
 *
 */
public class TstCobol1 extends TestCase {

	private static final String POSITIVE_NUMERIC_COPYBOOK
					=  "           01  ZZ-TTT4.\n"
					 + "             03 f01         pic 999V99.\n"
					 + "             03 f02         pic 999V99 comp-3.\n"
					 + "             03 f03         pic 999V99 comp.\n"
					 + "             03 f04         pic 999V99 comp-5.\n"
					 + "\n"
					 + "             03 f05         pic 9999.99.\n";

	private static final String SIGNED_NUMERIC_COPYBOOK
					=  "           01  ZZ-TTT4.\n"
					 + "             03 f01         pic S999V99.\n"
					 + "             03 f02         pic S999V99 comp-3.\n"
					 + "             03 f03         pic S999V99 comp.\n"
					 + "             03 f04         pic S999V99 comp-5.\n"
					 + "\n"
					 + "             03 f05         pic -9999.99.\n"
	                 + "             03 f06         pic +9999.99.\n"
					 + "\n"
					 + "             03 f07         pic ----9.99.\n"
	                 + "             03 f08         pic ++++9.99.\n"
					 + "\n"
					 + "             03 f07         pic -----.99.\n"
	                 + "             03 f08         pic +++++.99.\n"

	                 ;

	public void testPositiveConversion() throws RecordException {
		int[] intelTypes = {22, 33, 23, 23, 25, };
		int[] mainframeTypes = {22, 33, 39, 39, 25, };

		System.out.println();
		System.out.println("Intel: ");
		testPositiveConversion(Convert.FMT_INTEL, intelTypes);

		System.out.println();
		System.out.println("Mainframe: ");
		testPositiveConversion(Convert.FMT_MAINFRAME, mainframeTypes);
	}

	public void testPositiveConversion(int cobolDialect, int[] types) throws RecordException {
		LayoutDetail l = getCobolLayout(cobolDialect, POSITIVE_NUMERIC_COPYBOOK);
		TypeManager m = TypeManager.getInstance();

		System.out.println();
		System.out.print("{");
		for (int i = 0; i < l.getRecord(0).getFieldCount(); i++) {
			FieldDetail field = l.getRecord(0).getField(i);
			//Type t = m.getType(field.getType());

			System.out.print(field.getType() + ", ");
		}
		System.out.println("};");
		System.out.println();


		for (int i = 0; i < l.getRecord(0).getFieldCount(); i++) {
			FieldDetail field = l.getRecord(0).getField(i);
			Type t = m.getType(field.getType());

			assertTrue("Field: " + field.getName(), t instanceof TypeNum && ((TypeNum) t).isPositive());
			assertEquals(types[i], field.getType());
		}

	}


	public void testSignedConversion() throws RecordException {
		int[] intelTypes = {41, 31, 15, 15, 7, 24, 6, 29, 6, 29, };
		int[] mainframeTypes = {32, 31, 35, 35, 7, 24, 6, 29, 6, 29, };

		System.out.println();
		System.out.println("Signed Intel: ");
		testSignedConversion(Convert.FMT_INTEL, intelTypes);

		System.out.println();
		System.out.println("Signed Mainframe: ");
		testSignedConversion(Convert.FMT_MAINFRAME, mainframeTypes);
	}

	public void testSignedConversion(int cobolDialect, int[] types) throws RecordException {
		LayoutDetail l = getCobolLayout(cobolDialect, SIGNED_NUMERIC_COPYBOOK);
		TypeManager m = TypeManager.getInstance();

		System.out.println();
		System.out.print("{");
		for (int i = 0; i < l.getRecord(0).getFieldCount(); i++) {
			FieldDetail field = l.getRecord(0).getField(i);
			//Type t = m.getType(field.getType());

			System.out.print(field.getType() + ", ");
		}
		System.out.println("};");
		System.out.println();


		for (int i = 0; i < l.getRecord(0).getFieldCount(); i++) {
			FieldDetail field = l.getRecord(0).getField(i);
			Type t = m.getType(field.getType());

			assertTrue("Field: " + field.getName(), (t instanceof TypeNum && (! ((TypeNum) t).isPositive())));
			assertEquals("Field Type: " + field.getName(), types[i], field.getType());
		}

	}


	private static LayoutDetail getCobolLayout(int cobolDialect, String cobolCopybook) throws RecordException {
		CobolCopybookLoader loader = new CobolCopybookLoader();
		ByteArrayInputStream bs = new ByteArrayInputStream(cobolCopybook.getBytes());


		return loader.loadCopyBook(bs, "ZZ-TTT4", CopybookLoader.SPLIT_01_LEVEL, 0, "", cobolDialect, 0, new TextLog())
				.asLayoutDetail();
	}
}
