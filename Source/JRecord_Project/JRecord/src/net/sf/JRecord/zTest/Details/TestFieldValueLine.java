package net.sf.JRecord.zTest.Details;

import java.io.IOException;
import java.io.StringReader;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.fieldValue.IFieldValue ;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class TestFieldValueLine extends TestCase {
	
	private static final String COMP5 = "COMP-5";
	private static final String COMP = "COMP";
	public static final byte[] LOW_VALUES = {0, 0, 0, 0, 0, 0, 0, 0, 0}; 
	public static final byte[] HIGH_VALUES = {-1, -1, -1, -1, -1, -1, -1, -1, -1}; 
	public static final byte[] SPACES = {64, 64, 64, 64, 64, 64, 64, 64, 64}; 
	public static final byte[] ASCII_SPACES = {32, 32, 32, 32, 32, 32, 32, 32, 32}; 
	
	public static final int LOW_VALUES_TST = 1;
	public static final int HIGH_VALUES_TST = 2;
	public static final int SPACES_TST = 3;
	
	public static String CBL
	     = "      01  CBL-CPY.\n"
	     + "          03  field1   pic s9(3)";
	
	public static String[] DECIMAL_OPTS = {
		"", "V9", "V99" 	
	};
	public static String[] USAGE_OPTS = {
			"", "COMP-3", COMP, COMP5
	};
	
	
	/**
	 * Check the isPresent, isSpaces, isLowValues, isHighValues 
	 * for a Cobol Line (Byte line)
	 * 
	 * @throws IOException
	 */
	public void testChecks() throws IOException {
		for (String decimal : DECIMAL_OPTS) {
			for (String usage : USAGE_OPTS) {
				doTests(CBL + decimal+  " " + usage + ".\n", usage);
			}
		}
	}
	
	
	private void doTests(String cbl, String usage) throws IOException {
		//System.out.println(cbl);
		AbstractLine line;

		ICobolIOBuilder iob = JRecordInterface1.COBOL
									.newIOBuilder(new StringReader(cbl), "CBL-CPY")
									.setFont("cp037");
		
		
		line = iob.newLine();	
		assertTrue(! line.getFieldValue("field1").isFieldPresent());
		
		chkFieldValue(iob.newLine(LOW_VALUES.clone()), LOW_VALUES_TST, COMP.equals(usage) || COMP5.equals(usage));
		chkFieldValue(iob.newLine(HIGH_VALUES.clone()), HIGH_VALUES_TST, true);
		chkFieldValue(iob.newLine(SPACES.clone()), SPACES_TST, true);

		iob = JRecordInterface1.COBOL
									.newIOBuilder(new StringReader(cbl), "CBL-CPY");
		
		
		line = iob.newLine();	
		assertTrue(! line.getFieldValue("field1").isFieldPresent());
		
		chkFieldValue(iob.newLine(LOW_VALUES.clone()), LOW_VALUES_TST, COMP.equals(usage) || COMP5.equals(usage));
		chkFieldValue(iob.newLine(HIGH_VALUES.clone()), HIGH_VALUES_TST, true);
		chkFieldValue(iob.newLine(ASCII_SPACES.clone()), SPACES_TST, true);
		
		System.out.println();
	}


	private void chkFieldValue(AbstractLine line, int tstId, boolean isPresent) {
		IFieldValue fieldValue = line.getFieldValue("field1");
		if ((isPresent) != fieldValue.isFieldPresent()) {
			assertTrue((isPresent) == fieldValue.isFieldPresent());
		}
		assertTrue((tstId == LOW_VALUES_TST) == fieldValue.isLowValues());
		assertTrue((tstId == HIGH_VALUES_TST) ==  fieldValue.isHighValues());
		assertTrue((tstId == SPACES_TST) ==  fieldValue.isSpaces());
	}
	
	
	
}
