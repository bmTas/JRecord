package net.sf.JRecord.zTest.io.continuous;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.io.UnsupportedEncodingException;

import org.junit.jupiter.api.Test;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.fieldValue.IFieldValue;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

class TestIsPresetMethod {

	@Test
	void testIsPresentAscii() throws UnsupportedEncodingException, IOException {
		doTest( new TstDataContinuousMultiByteFonts(Conversion.DEFAULT_ASCII_CHARSET));
	}
	

	@Test
	void testIsPresentUtf8() throws UnsupportedEncodingException, IOException {
		doTest( new TstDataContinuousMultiByteFonts("utf8"));
	}
	
	void doTest(TstDataContinuousMultiByteFonts testData) throws UnsupportedEncodingException, IOException {
		ICobolIOBuilder ioBuilder = testData.getIoBuilder();
		AbstractLineReader reader = ioBuilder.newReader(testData.getDataStream());
		AbstractLine line;
		
		while ((line = reader.read()) != null) {
			int maxIndex = line.getFieldValue("P01-COUNT").asInt();
			for (int index = 0; index <  10; index++) {
				IFieldValue fieldValue = line.getFieldValue("P01-K-H-DD-R (" + index + ")");
				assertEquals(index < maxIndex, fieldValue.isFieldPresent());
				assertEquals(index < maxIndex, fieldValue.isFieldInRecord());
			}
		}
		reader.close();
	}

}
