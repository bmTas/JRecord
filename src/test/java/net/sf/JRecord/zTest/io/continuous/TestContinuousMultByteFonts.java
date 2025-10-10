package net.sf.JRecord.zTest.io.continuous;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.List;

import org.junit.jupiter.api.Test;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.CharLineProvider;
import net.sf.JRecord.Details.DefaultLineProvider;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.ContinuousLineReader;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

class TestContinuousMultByteFonts {

	@Test
	void testAscii() throws UnsupportedEncodingException, IOException {
		testFont(Conversion.DEFAULT_ASCII_CHARSET);
	}

	@Test
	void testEbcdic() throws UnsupportedEncodingException, IOException {
		testFont("cp037");
	}

	@Test
	void testUtf8() throws UnsupportedEncodingException, IOException {
		testFont("utf8");
		testFontAlterativeData("utf8");
	}

	@Test
	void testUtf16() throws UnsupportedEncodingException, IOException {
		testFont("utf16");
		testFontAlterativeData("utf16");
	}

	@Test
	void testUtf32() throws UnsupportedEncodingException, IOException {
		testFont("utf32");
		testFontAlterativeData("utf32");
	}

	@Test
	void tesBinLine() throws UnsupportedEncodingException, IOException {
		TstDataContinuousMultiByteFonts testData = new TstDataContinuousMultiByteFonts(Conversion.DEFAULT_ASCII_CHARSET);
		AbstractLineReader reader = ContinuousLineReader.newReader(new DefaultLineProvider() {});
		
		//System.out.println(reader.getClass().getSimpleName());
		runDelegateTest(testData, reader);
	}

	@Test
	void testCharLine() throws UnsupportedEncodingException, IOException {
		
		runDelegateTest(new TstDataContinuousMultiByteFonts("utf8"), ContinuousLineReader.newReader(new CharLineProvider() {}));
		runDelegateTest(new TstDataContinuousMultiByteFonts("utf16"), ContinuousLineReader.newReader(new CharLineProvider() {}));
		runDelegateTest(new TstDataContinuousMultiByteFonts("utf32"), ContinuousLineReader.newReader(new CharLineProvider() {}));
	}

	private void runDelegateTest(TstDataContinuousMultiByteFonts testData, AbstractLineReader reader)
			throws IOException, UnsupportedEncodingException {
		assertEquals("DelegateReader", reader.getClass().getSimpleName());
		
		reader.open(testData.getDataStream(), testData.getIoBuilder().getLayout());
		runTest(testData.getExpectedLines(), reader);
	}

	private void testFont(String encoding) throws UnsupportedEncodingException, IOException {
		TstDataContinuousMultiByteFonts testData = new TstDataContinuousMultiByteFonts(encoding);
		
		ICobolIOBuilder ioBuilder = testData.getIoBuilder();
		AbstractLineReader reader = ioBuilder.newReader(testData.getDataStream());
		runTest(testData.getExpectedLines(), reader);
	}

	private void testFontAlterativeData(String encoding) throws UnsupportedEncodingException, IOException {
		TstDataContinuousMultiByteFonts testData = new TstDataContinuousMultiByteFonts(encoding);
		
		ICobolIOBuilder ioBuilder = testData.getIoBuilder();
		AbstractLineReader reader = ioBuilder.newReader(testData.getAlternateDataStream());
		runTest(testData.getAlternateExpectedLines(), reader);
	}

	private void runTest(List<String> expectedLines, AbstractLineReader reader) throws IOException {
		AbstractLine line;
		int index = 0;
		
		while ((line = reader.read()) != null) {
			assertEquals(expectedLines.get(index++), line.getFullLine());
		}
		assertEquals(expectedLines.size(), index);
	}
}
