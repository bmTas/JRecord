package net.sf.JRecord.zTest.Cobol;

import java.io.IOException;
import java.io.StringReader;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.fieldValue.IFieldValue;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class TstCobol3 extends TestCase {

	private static final String UNSIGNED_COPYBOOK
		= "           01  TST.\n"
		+ "               03 f01         pic 999.\n";

	private static final String SIGNED_COPYBOOK
		= "           01  TST.\n"
		+ "               03 f01         pic s999.\n";
	
	private static final String SIGNED_COMP_COPYBOOK
	= "           01  TST.\n"
	+ "               03 f01         pic s9999 comp.\n";

	public void testCblUnSigned() throws IOException {
		tstSetUnsigned("");
		tstSetUnsigned("CP037");
	}


	public void testCblSigned() throws IOException {
		tstSetSigned("");
		tstSetSigned("CP037");
	}
	/**
	 * @param font
	 * @throws IOException
	 */
	@SuppressWarnings("deprecation")
	public void tstSetUnsigned(String font) throws IOException {
		ICobolIOBuilder iob = JRecordInterface1.COBOL
				.newIOBuilder(new StringReader(UNSIGNED_COPYBOOK), "TST")
				.setFont(font)
				;
		
		AbstractLine line = iob.newLine();
		line.setData("   ");
		
		for (int i = 0; i < 1000; i++) {
			IFieldValue fieldValue = line.getFieldValue("f01");
			
			fieldValue.set(i);
			tstFieldValueUnsigned(line, i, fieldValue);
			
			fieldValue.set("" + (999 - i));
			tstFieldValueUnsigned(line, 999 - i, fieldValue);
			
			line.setField("f01", "" + i);
			tstFieldValueUnsigned(line, i, fieldValue);
			
			line.setField("f01", (999 - i));
			tstFieldValueUnsigned(line, 999 - i, fieldValue);
		}
	}
	public void tstSetSigned(String font) throws IOException {
		ICobolIOBuilder iob = JRecordInterface1.COBOL
				.newIOBuilder(new StringReader(SIGNED_COPYBOOK), "TST")
				.setFont(font)
				;
		
		AbstractLine line = iob.newLine();
		line.setData("   ");
		
		IFieldValue fieldValue = line.getFieldValue("f01");
		
		tstSigned(line, fieldValue, 0);
		for (int i = 1; i < 1000; i++) {
			tstSigned(line, fieldValue, i);
			tstSigned(line, fieldValue, -i);
		}
	}

	/**
	 * @param line
	 * @param fieldValue
	 * @param i
	 */
	@SuppressWarnings("deprecation")
	public void tstSigned(AbstractLine line, IFieldValue fieldValue, int i) {
		int reset = 999-Math.abs(i);
		fieldValue.set(i);
		tstFieldValueSigned(line, i, fieldValue);
		fieldValue.set(reset);
		
		fieldValue.set("" + (i));
		tstFieldValueSigned(line, i, fieldValue);
		fieldValue.set("" + (reset));
		
		line.setField("f01", "" + i);
		tstFieldValueSigned(line, i, fieldValue);
		line.setField("f01", "" + (reset));
		
		line.setField("f01", (i));
		tstFieldValueSigned(line, i, fieldValue);
	}


	/**
	 * @param line
	 * @param i
	 * @param fieldValue
	 */
	private void tstFieldValueUnsigned(AbstractLine line, int i, IFieldValue fieldValue) {
		tstFieldValue(line, i, fieldValue);
		assertEquals(toString(i), line.getFullLine());
	}

	private void tstFieldValueSigned(AbstractLine line, int i, IFieldValue fieldValue) {
		tstFieldValue(line, i, fieldValue);
		String s;
		
		char charAt;
		if (i >= 0) {
			s = toString(i);
			charAt = s.charAt(2);
			if (charAt == '0') {
				charAt = Conversion.getPositive0EbcdicZoned();
			} else {
				charAt += Conversion.EBCDIC_ZONED_POSITIVE_DIFF;
			}
		} else {
			s = toString(-i);
			charAt = s.charAt(2);
			if (charAt == '0') {
				charAt = Conversion.getNegative0EbcdicZoned();
			} else {
				charAt += Conversion.EBCDIC_ZONED_NEGATIVE_DIFF;
			}
		}
		s = s.substring(0, 2) + charAt;
		
		assertEquals(s, line.getFullLine());
	}

	/**
	 * @param line
	 * @param i
	 * @param fieldValue
	 */
	@SuppressWarnings("deprecation")
	private void tstFieldValue(AbstractLine line, int i, IFieldValue fieldValue) {
		assertEquals(i, fieldValue.asInt());
		assertEquals("" + i, fieldValue.asString());
		assertEquals("" + i, line.getField("f01").toString());
	}
	
	private String toString(int val) {
		String ret = Integer.toString(val);
		return "000".substring(ret.length()) + ret;
	}
	
	
//	private void tstComp() {
//		ICobolIOBuilder iob = JRecordInterface1.COBOL
//				.newIOBuilder(new StringReader(UNSIGNED_COPYBOOK), "TST")
//				.setFont("cp037")
//				;
//		Type
//		
//	}
}
