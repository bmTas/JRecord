package net.sf.JRecord.zTest.Types.validate;

import static org.junit.Assert.*;

import java.io.IOException;
import java.io.StringReader;

import org.junit.Test;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.CharLine;
import net.sf.JRecord.Details.fieldValue.IFieldValue;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Types.smallBin.CheckZoned;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.cb2xml.def.Cb2xmlConstants;

public class TstAsciiZoned {


	private String signedCopybook   = "    03  Field   pic s9(3).";
	private String unsignedCopybook = "    03  Field   pic 9(3).";

	
	@Test
	public void testValid() throws IOException {
		ICobolIOBuilder ioBuilder = createSignedIoBuilder("cp037");
		checkValid(ioBuilder.newLine(), -999, ioBuilder);
		checkValidText(new CharLine(ioBuilder.getLayout(), ""), -999, ioBuilder);
		
		ioBuilder = createSignedIoBuilder("");
		checkValid(ioBuilder.newLine(), -999, ioBuilder);
		checkValidText(new CharLine(ioBuilder.getLayout(), ""), -999, ioBuilder);
	}
	
	
	@Test
	public void testValidCp273() throws IOException {

		ICobolIOBuilder ioBuilder = createSignedIoBuilder("cp273");
		checkValidBin(ioBuilder.newLine(), -999, ioBuilder);
	}
	
	
	@Test
	public void testValidPositive() throws IOException {
		ICobolIOBuilder ioBuilder = createUnSignedIoBuilder("cp037");
		checkValid(ioBuilder.newLine(), 0, ioBuilder);
		checkValidText(new CharLine(ioBuilder.getLayout(), ""), 0, ioBuilder);
		
		ioBuilder = createSignedIoBuilder("");
		checkValid(ioBuilder.newLine(), 0, ioBuilder);
		checkValidText(new CharLine(ioBuilder.getLayout(), ""), 0, ioBuilder);
	}
	
	
	@Test
	public void testValidPositiveCp273() throws IOException {

		ICobolIOBuilder ioBuilder = createUnSignedIoBuilder("cp273");
		checkValidBin(ioBuilder.newLine(), 0, ioBuilder);
	}
	
	@Test
	public void testInValid() throws IOException {
		ICobolIOBuilder ioBuilder = createSignedIoBuilder("cp037");
		checkInvalid(ioBuilder.newLine(), ioBuilder);
		ioBuilder = createSignedIoBuilder("");
		checkInvalidBin(ioBuilder.newLine(), ioBuilder);
		checkInvalidText(ioBuilder.newLine(), ioBuilder);
	}

	
	@Test
	public void testInValiCp273d() throws IOException {
		ICobolIOBuilder ioBuilder = createSignedIoBuilder("cp273");
		checkInvalidBin(ioBuilder.newLine(), ioBuilder);
	}

	
	/**
	 * @param start
	 * @param ioBldr
	 * @throws IOException
	 */
	private void checkValid(AbstractLine line, int start, ICobolIOBuilder ioBldr) throws IOException {
		checkValidBin(line, start, ioBldr);
		checkValidText(line, start, ioBldr);
	}
	
	private void checkValidBin(AbstractLine line, int start, ICobolIOBuilder ioBldr) throws IOException {
		FieldDetail fieldDef = line.getLayout().getField(0, 0);
		
		for (int i = start; i < 1000; i++) {
			line.getFieldValue(fieldDef).set(i);
			assertTrue("" + i,
					CheckZoned.checkAsciiZoned(fieldDef, getField(fieldDef, line)));
			assertTrue(i + "",
					CheckZoned.checkAsciiZoned(fieldDef.getPos(), fieldDef,  line.getData()));
		}
	}

	private void checkValidText(AbstractLine line, int start, ICobolIOBuilder ioBldr) throws IOException {
		FieldDetail fieldDef = line.getLayout().getField(0, 0);
		
		for (int i = start; i < 1000; i++) {
			line.getFieldValue(fieldDef).set(i);
			assertTrue("" + i,
					CheckZoned.checkAsciiZoned(fieldDef, getField(fieldDef, line)));
		}
	}
	
	private String getField(FieldDetail fieldDef, AbstractLine line) {
		String fullLine = line.getFullLine();
		return fullLine.length() <= fieldDef.getLen() ? fullLine : fullLine.substring(0, fieldDef.getLen());

	}


	/**
	 * @param ioBldr
	 * @throws IOException
	 */
	private void checkInvalid(AbstractLine line, ICobolIOBuilder ioBldr) throws IOException {
		checkInvalidBin(line, ioBldr);
		checkInvalidSignDigit(line);
		checkInvalidText(line, ioBldr);
	}


	/**
	 * @param ioBldr
	 * @throws IOException
	 */
	private void checkInvalidBin(AbstractLine line, ICobolIOBuilder ioBldr) throws IOException {
		FieldDetail fieldDef = line.getLayout().getField(0, 0);
		IFieldValue fieldValue = line.getFieldValue(fieldDef);
		char[] checkChar = {'a', 'a', '-'};
		
		for (int i = 0; i < 3; i++) {
			fieldValue.set(123);
			char[] chars = fieldValue.asString().toCharArray();
			chars[i] = checkChar[i];
			line.setData(new String(chars));
			assertFalse(
					CheckZoned.checkAsciiZoned(fieldDef, getField(fieldDef, line)));
			assertFalse(
					CheckZoned.checkAsciiZoned(fieldDef.getPos(), fieldDef,  line.getData()));
		}
	}


	/**
	 * @param line
	 */
	protected void checkInvalidSignDigit(AbstractLine line) {
		FieldDetail fieldDef = line.getLayout().getField(0, 0);
		IFieldValue fieldValue = line.getFieldValue(fieldDef);
		
		fieldValue.set(123);
		byte[] data = line.getData();
		int lb = data[2] & 0xff;
		data[2] = (byte) (lb | 0x0F);
		assertFalse(
				CheckZoned.checkAsciiZoned(fieldDef, getField(fieldDef, line)));
		data[2] = (byte) (lb & 0x0F);
		assertFalse(
				CheckZoned.checkAsciiZoned(fieldDef, getField(fieldDef, line)));
	}
	
	private void checkInvalidText(AbstractLine line, ICobolIOBuilder ioBldr) throws IOException {
		FieldDetail fieldDef = line.getLayout().getField(0, 0);
		IFieldValue fieldValue = line.getFieldValue(fieldDef);
		char[] checkChar = {'a', 'a', '-'};
		
		for (int i = 0; i < 3; i++) {
			fieldValue.set(123);
			char[] chars = fieldValue.asString().toCharArray();
			chars[i] = checkChar[i];
			line.setData(new String(chars));
			String fullLine = line.getFullLine();
			String v = fullLine.length() <= fieldDef.getLen() ? fullLine : fullLine.substring(0, fieldDef.getLen());
			assertFalse("" + i,
					CheckZoned.checkAsciiZoned(fieldDef, v));
		}
	}

	
	/**
	 * @return
	 */
	protected ICobolIOBuilder createSignedIoBuilder(String font) {
		return JRecordInterface1.COBOL.newIOBuilder(new StringReader(signedCopybook), "Signed-numeric")
				.setCopybookFileFormat(Cb2xmlConstants.FREE_FORMAT)
				.setDialect(ICopybookDialects.FMT_GNU_COBOL )
				.setFont(font);
	}

	/**
	 * @return
	 */
	protected ICobolIOBuilder createUnSignedIoBuilder(String font) {
		return JRecordInterface1.COBOL.newIOBuilder(new StringReader(unsignedCopybook), "Signed-numeric")
				.setCopybookFileFormat(Cb2xmlConstants.FREE_FORMAT)
				.setDialect(ICopybookDialects.FMT_GNU_COBOL )
				.setFont(font);
	}
}
