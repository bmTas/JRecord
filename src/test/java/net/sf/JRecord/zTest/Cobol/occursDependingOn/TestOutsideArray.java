package net.sf.JRecord.zTest.Cobol.occursDependingOn;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;


import org.junit.jupiter.api.Test;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.CharLine;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.Details.fieldValue.IFieldValue;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.cb2xml.copybookReader.ReadCobolCopybook;

/**
 * Check exception occurs when array index > max allowed index in an occurs depending array
 * 
 * @author Bruce Martin 
 *
 */
class TestOutsideArray {

	private static final String FIELD_1_START = "Field: item (1)";
	private static final String FIELD_2_START = "Field: item (2)";
	private static final String ARRAY_INDEX_ERROR_MESSAGE_1 = ""
			+ " Index is greater than that allowed by occurs depending on field.\n"
			+ "Use the isFieldInLine() method\n"
			+ "or access the Occurs Depending on fields: Count";
	private static final String ARRAY_INDEX_ERROR_MESSAGE_2 = ""
			+ "Field: item (0, 1) Index is greater than that allowed by occurs depending on field.\n"
			+ "Use the isFieldInLine() method\n"
			+ "or access the Occurs Depending on fields: Count1, Count2";

	private static final String INDEX_ERROR_MESSAGE = "Index is greater than that allowed by occurs depending";


	@Test
	void testOneElementArray() throws IOException {
		checkOneElementArray(new CopybookDetails(false));
	}
	
	@Test
	void testOneElementArrayCharLine() throws IOException {
		checkOneElementArray(new CopybookDetails(true));
	}

	private void checkOneElementArray(CopybookDetails cd) throws IOException {
		AbstractLine line = cd.oneElementLine();
		
		assertEquals(1, line.getFieldValue("Count").asInt());
		IFieldValue fieldValue1 = line.getFieldValue("item (0)");
		assertTrue(fieldValue1.isFieldInRecord());
		assertEquals("aa", fieldValue1.asString());
		
		checkInvalidIndex(line, line.getFieldValue("item (1)"), "bb");
	}

	@Test
	void testTwoElementArray() throws IOException {
		check2ElementArray(new CopybookDetails(false));
	}

	@Test
	void testTwoElementArrayCharLine() throws IOException {
		check2ElementArray(new CopybookDetails(true));
	}


	@Test
	void testNestedArray() throws IOException {
		ICobolIOBuilder iob = JRecordInterface1.COBOL.newIOBuilder(
				new ReadCobolCopybook()
					.setCopybookName("Test-Occurs-Depending")
					.addFreeFormatCobolText(""
							+ " 01 Test-Copybook.\n"
							+ "    03  Count1  pic 9.\n"
							+ "    03  Count2  pic 9.\n"
							+ "    03  filler occurs 1 to 9 depending on Count1.\n"
							+ "       05 occurs 1 to 9 depending on Count2.\n"
							+ "          07 item  pic xx.\n")
				);

		AbstractLine line = iob.newLine();
		line.setData("11aabbccdd");
		
		IFieldValue fieldValue = line.getFieldValue("item (0, 1)");

		assertFalse(fieldValue.isFieldInRecord());
		
		// should be an exception because array in dex > maximum allowed index as 
		// specified by the occurs depending value
		try {
			fieldValue.asString();
		} catch (RuntimeException e) {
			assertEquals(ARRAY_INDEX_ERROR_MESSAGE_2, e.getMessage());
		}	

	}

	private void check2ElementArray(CopybookDetails cd) throws IOException {
		AbstractLine line = cd.twoElementLine();
		
		assertEquals(2, line.getFieldValue("Count").asInt());
		IFieldValue fieldValue1 = line.getFieldValue("item (0)");
		assertTrue(fieldValue1.isFieldInRecord());
		assertEquals("aa", fieldValue1.asString());
		
		IFieldValue fieldValue2 = line.getFieldValue("item (1)");
		assertTrue(fieldValue2.isFieldInRecord());
		assertEquals("bb", fieldValue2.asString());
		
		checkInvalidIndex(line, line.getFieldValue("item (2)"), "cc");
	}

	

	private void checkInvalidIndex(AbstractLine line, IFieldValue fieldValue, String value) {
		assertFalse(fieldValue.isFieldInRecord());
		boolean invalidField = false;
		
		// should be an exception because array in dex > maximum allowed index as 
		// specified by the occurs depending value
		try {
			fieldValue.asString();
		} catch (RuntimeException e) {
			invalidField = indexErrorCheck(e.toString());
			String message = e.getMessage();
			String part1 = message.startsWith(FIELD_1_START) ? FIELD_1_START : FIELD_2_START;
			assertEquals(part1 + ARRAY_INDEX_ERROR_MESSAGE_1, message);
		}	
		assertTrue(invalidField);
		invalidField = false;
		try {
			fieldValue.asHex();
		} catch (RuntimeException e) {
			invalidField = indexErrorCheck(e.toString());
		}	
		assertTrue(invalidField);
		invalidField = false;
		try {
			fieldValue.setToSpaces();
		} catch (RuntimeException e) {
			invalidField = indexErrorCheck(e.toString());
		}	
		assertTrue(invalidField);
		
		if (line instanceof Line) {
			invalidField = false;
			try {
				fieldValue.setToHighValues();
			} catch (RuntimeException e) {
				invalidField = indexErrorCheck(e.toString());
			}	
			assertTrue(invalidField);
		}

		IFieldValue countValue = line.getFieldValue("Count");
		countValue.set(countValue.asInt() + 1);
		
		assertTrue(fieldValue.isFieldInRecord());
		assertEquals(value, fieldValue.asString());

	}

	boolean indexErrorCheck(String errorMsg) {
		return errorMsg.indexOf(INDEX_ERROR_MESSAGE) >= 0;
	}
	
	private static class CopybookDetails {
		
		final ICobolIOBuilder iob;
		final boolean isCharLine;
		
		CopybookDetails(boolean isCharLine) throws IOException {
			this.isCharLine = isCharLine;
			
			iob = JRecordInterface1.COBOL.newIOBuilder(
					new ReadCobolCopybook()
						.setCopybookName("Test-Occurs-Depending")
						.addFreeFormatCobolText(""
								+ " 01 Test-Copybook.\n"
								+ "    03  Count  pic 9.\n"
								+ "    03  filler occurs 1 to 10 depending on Count.\n"
								+ "       05 item  pic xx.\n")
					);
		}
		
		AbstractLine oneElementLine() throws IOException {
			AbstractLine line = newLine();
			line.setData("1aabbcc");
			return line;
		}
		
		AbstractLine twoElementLine() throws IOException {
			AbstractLine line = newLine();
			line.setData("2aabbcc");
			return line;
		}

		private AbstractLine newLine() throws IOException {
			if (isCharLine) {
				return new CharLine(iob.getLayout(), "");
			}
			return iob.newLine();
		}
		
	}
}
