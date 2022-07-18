package net.sf.JRecord.zTest.Types.validate;

import static org.junit.Assert.*;

import java.io.IOException;
import java.io.StringReader;
import java.util.Arrays;

import org.junit.Test;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.CharLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class TstValidateFieldValue {

	public static String COBOL_1 = ""
			+ "       01  Valid-tst.\n"
			+ "           03 field-1                 pic x(6).\n"
			+ "           03 field-2                 pic 9(6).\n"
			+ "           03 field-3                 pic s9(6).\n"
			+ "           03 field-4                 pic z(5)9.\n"
			+ "           03 field-5                 pic -(5)9.\n"
			+ "           03 field-6                 pic ---,--9.\n"
			+ "           03 field-7                 pic -ZZ,ZZ9.\n"
			+ "           03 field-8                 pic s9(5) sign leading.\n"
			+ "           03 field-9                 pic s9(5) sign trailing.\n"
			+ "           03 field-10                pic s9(5) sign leading separate.\n"
			+ "           03 field-11                pic s9(5) sign trailing separate.\n"
			;
	public static String COBOL_2 = ""
			+ "       01  Valid-tst-1.\n"
			+ "           03 field-7                 pic 9(6) comp.\n"
			+ "           03 field-8                 pic s9(6) comp.\n"
			+ "           03 field-9                 pic 9(6) comp-5.\n"
			+ "           03 field-10                pic s9(6) comp-5.\n"
			+ "           03 field-11                pic 9(6) comp-3.\n"
			+ "           03 field-12                pic s9(6) comp-3.\n"
			;

	@Test
	public void testMainframe() throws IOException {
		ICobolIOBuilder iob = createIoBuilder(ICopybookDialects.FMT_MAINFRAME, "cp037");
		
		LayoutDetail schema = iob.getLayout();

		tstMethod(schema, new Line(schema));
		tstMethod(schema, new CharLine(schema, ""));
	}

	@Test
	public void testAsciiMainframe() throws IOException {
		ICobolIOBuilder iob = createIoBuilder(ICopybookDialects.FMT_MAINFRAME, "");
		
		LayoutDetail schema = iob.getLayout();

		tstMethod(schema, new Line(schema));
		tstMethod(schema, new CharLine(schema, ""));
	}


	@Test
	public void testAscii() throws IOException {
		ICobolIOBuilder iob = createIoBuilder(ICopybookDialects.FMT_GNU_COBOL, "");
		
		LayoutDetail schema = iob.getLayout();

		tstMethod(schema, new Line(schema));
		tstMethod(schema, new CharLine(schema, ""));
	}


	@Test
	public void testMainframeBin() throws IOException {
		ICobolIOBuilder iob = createBinIoBuilder(ICopybookDialects.FMT_MAINFRAME, "cp037");
		
		LayoutDetail schema = iob.getLayout();

		tstMethod(schema, new Line(schema));
	}

	@Test
	public void testAsciiMainframeBin() throws IOException {
		ICobolIOBuilder iob = createBinIoBuilder(ICopybookDialects.FMT_MAINFRAME, "");
		
		LayoutDetail schema = iob.getLayout();

		tstMethod(schema, new Line(schema));
	}


	@Test
	public void testAsciiBin() throws IOException {
		ICobolIOBuilder iob = createBinIoBuilder(ICopybookDialects.FMT_GNU_COBOL, "");
		
		LayoutDetail schema = iob.getLayout();

		tstMethod(schema, new Line(schema));
	}

	/**
	 * @param schema
	 * @param line
	 */
	@SuppressWarnings("deprecation")
	protected void tstMethod(LayoutDetail schema, AbstractLine line) {
		int fieldCount = schema.getRecord(0).getFieldCount();
		
		for (int i = 0 ; i < 101; i++) {
			for (int fldNum = 0 ; fldNum < fieldCount; fldNum++) {
				line.setField(0, fldNum, "" + i);
				FieldDetail field = schema.getField(0, fldNum);
				assertTrue(i + " " + fldNum, 
						line.getFieldValue(0, fldNum).isValid());
				assertTrue(i + " " + fldNum, 
						line.getFieldValue(field).isValid());
			}
			if (line instanceof Line) {
				byte[] data = line.getData();
				Arrays.fill(data, (byte) 0);
			} else {
				char[] chars = new char[schema.getMaximumRecordLength()];
				line.setData(new String(chars));
			}
		}
		for (int fldNum = 0 ; fldNum < fieldCount; fldNum++) {
			FieldDetail field = schema.getField(0, fldNum);
			if ((!TypeManager.isBinary(field.getType())) || fldNum > 3) {
				assertFalse("Field-Number: " + fldNum, line.getFieldValue(0, fldNum).isValid());
				assertFalse("Field-Number: " + fldNum, line.getFieldValue(field).isValid());
			}
		}
	}

	/**
	 * 
	 */
	private ICobolIOBuilder createIoBuilder(int dialect, String font) {
		return JRecordInterface1.COBOL.newIOBuilder(new StringReader(COBOL_1), "TextFields")
				.setDialect(dialect)
				.setFont(font);
	}
	

	/**
	 * 
	 */
	private ICobolIOBuilder createBinIoBuilder(int dialect, String font) {
		return JRecordInterface1.COBOL.newIOBuilder(new StringReader(COBOL_2), "TextFields")
				.setDialect(dialect)
				.setFont(font);
	}


}
