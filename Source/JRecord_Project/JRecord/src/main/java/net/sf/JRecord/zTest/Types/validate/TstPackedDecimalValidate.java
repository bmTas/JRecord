package net.sf.JRecord.zTest.Types.validate;

import static org.junit.Assert.*;

import java.io.IOException;
import java.io.StringReader;

import org.junit.Test;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.fieldValue.IFieldValue;
import net.sf.JRecord.Types.smallBin.CheckPackedDecimal;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.cb2xml.def.Cb2xmlConstants;

public class TstPackedDecimalValidate {

	private String signedComp3Copybook   = "    03  Field-comp-3   pic s9(3) comp-3.";
	private String unsignedComp3Copybook = "    03  Field-comp-3   pic 9(3) comp-3.";
	
	@Test
	public void testValid() throws IOException {
		checkValid(-999, createSignedIoBuilder());
	}
	
	@Test
	public void testInValid() throws IOException {
		checkInvalid(createSignedIoBuilder());
	}
	
	
	@Test
	public void testValidUnsigned() throws IOException {
		checkValid(0, createUnSignedIoBuilder());
	}
	
	@Test
	public void testInValidUnsigned() throws IOException {
		ICobolIOBuilder iob = createUnSignedIoBuilder();
		checkInvalid(iob);
		checkNegativeNumbers(iob);
	}

	/**
	 * @param start
	 * @param ioBldr
	 * @throws IOException
	 */
	private void checkValid(int start, ICobolIOBuilder ioBldr) throws IOException {
		AbstractLine line = ioBldr.newLine();
		FieldDetail comp3Field = line.getLayout().getField(0, 0);
		
		for (int i = start; i < 1000; i++) {
			line.getFieldValue(comp3Field).set(i);
			assertTrue(
					CheckPackedDecimal.checkPackedDecimal(comp3Field.getPos(), comp3Field, line.getData()));
		}
	}

	/**
	 * @param start
	 * @param ioBldr
	 * @throws IOException
	 */
	private void checkNegativeNumbers(ICobolIOBuilder ioBldr) throws IOException {
		AbstractLine line = ioBldr.newLine();
		FieldDetail comp3Field = line.getLayout().getField(0, 0);
		
		for (int i = -999; i < 0; i++) {
			line.getFieldValue(comp3Field).set(i);
			assertFalse(
					CheckPackedDecimal.checkPackedDecimal(comp3Field.getPos(), comp3Field, line.getData()));
		}
	}


	/**
	 * @param ioBldr
	 * @throws IOException
	 */
	private void checkInvalid(ICobolIOBuilder ioBldr) throws IOException {
		AbstractLine line = ioBldr.newLine();
		FieldDetail comp3Field = line.getLayout().getField(0, 0);
		IFieldValue fieldValue = line.getFieldValue(comp3Field);
		
		fieldValue.set(123);
		line.getData()[0] |= 0xF0;
		assertFalse(
				CheckPackedDecimal.checkPackedDecimal(comp3Field.getPos(), comp3Field, line.getData()));
		
		fieldValue.set(123);
		line.getData()[0] |= 0x0F;
		assertFalse(
				CheckPackedDecimal.checkPackedDecimal(comp3Field.getPos(), comp3Field, line.getData()));

		fieldValue.set(123);
		line.getData()[1] |= 0xF0;
		assertFalse(
				CheckPackedDecimal.checkPackedDecimal(
						comp3Field.getPos(), comp3Field, line.getData()));

		fieldValue.set(123);
		line.getData()[1] &= 0xF3;
		assertFalse(
				CheckPackedDecimal.checkPackedDecimal(comp3Field.getPos(), comp3Field, line.getData()));
	}
	
	
	/**
	 * @return
	 */
	protected ICobolIOBuilder createSignedIoBuilder() {
		return JRecordInterface1.COBOL.newIOBuilder(new StringReader(signedComp3Copybook), "Signed-Comp3")
				.setCopybookFileFormat(Cb2xmlConstants.FREE_FORMAT);
	}

	/**
	 * @return
	 */
	protected ICobolIOBuilder createUnSignedIoBuilder() {
		return JRecordInterface1.COBOL.newIOBuilder(new StringReader(unsignedComp3Copybook), "Unsigned-Comp3")
				.setCopybookFileFormat(Cb2xmlConstants.FREE_FORMAT);
	}

}
