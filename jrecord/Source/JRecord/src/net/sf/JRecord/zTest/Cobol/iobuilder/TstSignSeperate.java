package net.sf.JRecord.zTest.Cobol.iobuilder;

import java.io.IOException;
import java.io.StringReader;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import junit.framework.TestCase;

/**
 * Test Sign Separate clause
 *  
 * @author Bruce Martin
 *
 */
public class TstSignSeperate extends TestCase {
	private static String COPYBOOK0
			= "       01 HEADER.\n"
			+ "          05 SOME-VALUE  PIC S9(15) SIGN TRAILING SEPARATE CHARACTER.\n";
	private static String COPYBOOK1
			= "       01 HEADER.\n"
			+ "          05 SOME-VALUE  PIC S9(14)V9 SIGN TRAILING SEPARATE CHARACTER.\n";
	private static String COPYBOOK2
			= "       01 HEADER.\n"
			+ "          05 SOME-VALUE  PIC S9(13)V99 SIGN TRAILING SEPARATE CHARACTER.\n";
	private static String EN_SIGN_TRAILING_0
			= "       01 HEADER.\n"
			+ "          05 SOME-VALUE  PIC 9(15)-.\n";
	private static String EN_SIGN_TRAILING_1
			= "       01 HEADER.\n"
			+ "          05 SOME-VALUE  PIC 9(14)V9-.\n";
	private static String EN_SIGN_TRAILING_2
			= "       01 HEADER.\n"
			+ "          05 SOME-VALUE  PIC 9(13)V99-.\n";
	private static String EN_SIGN_TRAILING_1d
			= "       01 HEADER.\n"
			+ "          05 SOME-VALUE  PIC 9(13).9-.\n";
	private static String EN_SIGN_TRAILING_2d
			= "       01 HEADER.\n"
			+ "          05 SOME-VALUE  PIC 9(12).99-.\n";
	private static String EN_SIGN_TRAILING_2pd
			= "       01 HEADER.\n"
			+ "          05 SOME-VALUE  PIC 9(12).99+.\n";
	private static String EN_SIGN_TRAILING_0p
			= "       01 HEADER.\n"
			+ "          05 SOME-VALUE  PIC 9(15)+.\n";
	private static String EN_SIGN_TRAILING_1p
		= "       01 HEADER.\n"
		+ "          05 SOME-VALUE  PIC 9(14)V9+.\n";
	private static String EN_SIGN_TRAILING_2p
		= "       01 HEADER.\n"
		+ "          05 SOME-VALUE  PIC 9(13)V99+.\n";
	private static String EN_SIGN_TRAILING_2m
	= "       01 HEADER.\n"
	+ "          05 SOME-VALUE  PIC 9(13)V99-.\n";
	
	private static String SIGN_LEADING0
			= "       01 HEADER.\n"
			+ "          05 SOME-VALUE  PIC S9(15) SIGN LEADING SEPARATE CHARACTER.\n";
	private static String SIGN_LEADING1
			= "       01 HEADER.\n"
			+ "          05 SOME-VALUE  PIC S9(14)V9 SIGN LEADING SEPARATE CHARACTER.\n";
	private static String SIGN_LEADING2
			= "       01 HEADER.\n"
			+ "          05 SOME-VALUE  PIC S9(13)V99 SIGN LEADING SEPARATE CHARACTER.\n";
	
	private static String SIGN_LEADING0p
			= "       01 HEADER.\n"
			+ "          05 SOME-VALUE  PIC +9(15).\n";
	private static String SIGN_LEADING1p
			= "       01 HEADER.\n"
			+ "          05 SOME-VALUE  PIC +9(14)V9.\n";
	private static String SIGN_LEADING2p
			= "       01 HEADER.\n"
			+ "          05 SOME-VALUE  PIC +9(13)V99.\n";
	
	private static String SIGN_LEADING1pd
			= "       01 HEADER.\n"
			+ "          05 SOME-VALUE  PIC +9(13).9.\n";
	private static String SIGN_LEADING2pd
			= "       01 HEADER.\n"
			+ "          05 SOME-VALUE  PIC +9(12).99.\n";
	
	private String data = "000000000000123+";
	
	public void test1() throws IOException {
		ICobolIOBuilder iob = getIoBuilder(COPYBOOK2);
		
		AbstractLine l = iob.newLine();
		
		l.setData(data);
		
		System.out.println(l.getFieldValue(0, 0).asString());
		assertEquals("1.23", l.getFieldValue(0, 0).asString());
		
		assertTrue(true);
	}
	
	public void testDecimal0() throws IOException {
		tstSignSeperate(COPYBOOK0, 0);
	}
	
	public void testDecimal1() throws IOException {
		tstSignSeperate(COPYBOOK1, 1);
	}
	
	public void testDecimal2() throws IOException {
		tstSignSeperate(COPYBOOK2, 2);
	}
	
	public void testDecimal0a() throws IOException {
		tstSignSeperate(EN_SIGN_TRAILING_0, 0);
		tstSignSeperate(EN_SIGN_TRAILING_0p, 0);
	}
	
	public void testDecimal1a() throws IOException {
		tstSignSeperate(EN_SIGN_TRAILING_1, 1);
		tstSignSeperate(EN_SIGN_TRAILING_1p, 1);
		tstSignSeperateAD(EN_SIGN_TRAILING_1d, 1);
	}
	
	public void testDecimal2a() throws IOException {
		tstSignSeperate(EN_SIGN_TRAILING_2, 2);
		tstSignSeperate(EN_SIGN_TRAILING_2p, 2);
		tstSignSeperate(EN_SIGN_TRAILING_2m, 2);
		tstSignSeperateAD(EN_SIGN_TRAILING_2d, 2);
		tstSignSeperateAD(EN_SIGN_TRAILING_2pd, 2);
	}
	
	public void testSignLeadingDecimal0() throws IOException {
		tstSignSeperateLeading(SIGN_LEADING0, 0);
		tstSignSeperateLeading(SIGN_LEADING0p, 0);
	}
	
	public void testSignLeadingDecimal1() throws IOException {
		tstSignSeperateLeading(SIGN_LEADING1, 1);
		tstSignSeperateLeading(SIGN_LEADING1p, 1);
		tstSignSeperateLeadingAd(SIGN_LEADING1pd, 1);
	}
	
	public void testSignLeadingDecimal2() throws IOException {
		tstSignSeperateLeading(SIGN_LEADING2, 2);
		tstSignSeperateLeading(SIGN_LEADING2p, 2);
		tstSignSeperateLeadingAd(SIGN_LEADING2pd, 2);
	}
	

	private void tstSignSeperate(String copybook, int decimal) throws IOException {
		ICobolIOBuilder iob = getIoBuilder(copybook);
		AbstractLine l = iob.newLine();
		
		String value = format(0, decimal);
		chk(l, 0, value, "000000000000000+");
		for (int i = 3; i < 100000; i += 3) {
			value = format(i, decimal);

			chk(l, i, value, "00000000000000" + i + "+");
			chk(l, i, "-" + value, "00000000000000" + i + "-");
		}
	}

	private void tstSignSeperateAD(String copybook, int decimal) throws IOException {
		ICobolIOBuilder iob = getIoBuilder(copybook);
		AbstractLine l = iob.newLine();
		
		String value = format(0, decimal);
		chk(l, 0, value, addDecimal("000000000000000+", decimal));
		for (int i = 3; i < 100000; i += 3) {
			value = format(i, decimal);

			chk(l, i, value, addDecimal("00000000000000" + i, decimal) + "+");
			chk(l, i, "-" + value, addDecimal("00000000000000" + i, decimal) + "-");
		}
	}
	
	private String addDecimal(String s, int decimal) {
		if (s.endsWith("+") || s.endsWith("-")) {
			decimal = decimal + 1;
		}
		
		return (new StringBuilder(s)).insert(s.length() - decimal, '.').toString();
	}

	private void tstSignSeperateLeading(String copybook, int decimal) throws IOException {
		ICobolIOBuilder iob = getIoBuilder(copybook);
		AbstractLine l = iob.newLine();
		
		String value = format(0, decimal);
		chk(l, 0, value, "+000000000000000");
		for (int i = 3; i < 100000; i += 3) {
			value = format(i, decimal);

			chk(l, i, value, "+00000000000000" + i);
			chk(l, i, "-" + value, "-00000000000000" + i);
		}
	}

	private void tstSignSeperateLeadingAd(String copybook, int decimal) throws IOException {
		ICobolIOBuilder iob = getIoBuilder(copybook);
		AbstractLine l = iob.newLine();
		
		String value = format(0, decimal);
		chk(l, 0, value, addDecimal("+000000000000000", decimal));
		for (int i = 3; i < 100000; i += 3) {
			value = format(i, decimal);

			chk(l, i, value, addDecimal("+00000000000000" + i, decimal));
			chk(l, i, "-" + value, addDecimal("-00000000000000" + i, decimal));
		}
	}

	/**
	 * @param l
	 * @param i
	 * @param value
	 * @param s
	 */
	public void chk(AbstractLine l, int i, String value, String s) {
		s = s.substring(0, 1) +  s.substring(s.length() - 15);
		
		l.getFieldValue(0, 0).set(value);		
		assertEquals(i + "", s, l.getFullLine());
		
		l.setData(s);		
		assertEquals(i + "", value, l.getFieldValue(0, 0).asString());
	}
	
	private String format(int val, int decimal) {
		StringBuilder b = new StringBuilder(20).append(val);
		if (decimal > 0) {
			while (b.length() <= decimal) {
				b.insert(0, '0');
			}
			b.insert(b.length() - decimal, '.');
		}
		return b.toString();
	}

	/**
	 * @return
	 */
	public ICobolIOBuilder getIoBuilder(String copybook) {
		ICobolIOBuilder iob = JRecordInterface1.COBOL.newIOBuilder(new StringReader(copybook), "copybookname")
			        .setFileOrganization(Constants.IO_DEFAULT)
			        .setDialect(ICopybookDialects.FMT_INTEL)
			        .setSplitCopybook(CopybookLoader.SPLIT_NONE);
		return iob;
	}
}
