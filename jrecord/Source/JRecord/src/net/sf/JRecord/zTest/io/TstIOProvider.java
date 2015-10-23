package net.sf.JRecord.zTest.io;

import net.sf.JRecord.Common.BasicFileSchema;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Details.CharLineProvider;
import net.sf.JRecord.Details.DefaultLineProvider;
import net.sf.JRecord.Details.XmlLineProvider;
import net.sf.JRecord.IO.LineIOProvider;
import junit.framework.TestCase;

public class TstIOProvider extends TestCase {
	
		int[] stdTests = {
				Constants.IO_DEFAULT, Constants.IO_BIN_TEXT, Constants.IO_BIN_CSV, 
				Constants.IO_BIN_CSV_NAME_1ST_LINE, Constants.IO_TEXT_LINE, 
				Constants.IO_NAME_1ST_LINE, Constants.IO_CSV, Constants.IO_CSV_NAME_1ST_LINE, 
		};
		int[] unicodeTests = {
				Constants.IO_UNICODE_TEXT, 	Constants.IO_UNICODE_CSV, 
				Constants.IO_UNICODE_CSV_NAME_1ST_LINE, Constants.IO_UNICODE_NAME_1ST_LINE
		};

	public void testGetLineProvider() {
		LineIOProvider iop = new LineIOProvider();
	
		for (int io : stdTests) {
			assertTrue(iop.getLineProvider(BasicFileSchema.newCsvSchema(io, false, Conversion.getDefaultSingleByteCharacterset(), "", "")) instanceof DefaultLineProvider);
			assertTrue(iop.getLineProvider(BasicFileSchema.newCsvSchema(io, false, "CP037", "", "")) instanceof DefaultLineProvider);
			assertTrue(iop.getLineProvider(BasicFileSchema.newCsvSchema(io, false, "UTF-016", "", "")) instanceof CharLineProvider);
		}
		for (int io : unicodeTests) {
			assertTrue(iop.getLineProvider(BasicFileSchema.newCsvSchema(io, false, Conversion.getDefaultSingleByteCharacterset(), "", "")) instanceof CharLineProvider);
			assertTrue(iop.getLineProvider(BasicFileSchema.newCsvSchema(io, false, "CP037", "", "")) instanceof CharLineProvider);
			assertTrue(iop.getLineProvider(BasicFileSchema.newCsvSchema(io, false, "UTF-016", "", "")) instanceof CharLineProvider);
		}
	}
	
	@SuppressWarnings("deprecation")
	public void testGetLineProvider2() {
		LineIOProvider iop = new LineIOProvider();

		for (int io : stdTests) {
			assertTrue(iop.getLineProvider(io, Conversion.getDefaultSingleByteCharacterset()) instanceof DefaultLineProvider);
			assertTrue(iop.getLineProvider(io, "CP037") instanceof DefaultLineProvider);
			assertTrue(iop.getLineProvider(io, "UTF-016") instanceof CharLineProvider);
		}
		for (int io : unicodeTests) {
			assertTrue(iop.getLineProvider(io, Conversion.getDefaultSingleByteCharacterset()) instanceof CharLineProvider);
			assertTrue(iop.getLineProvider(io, "CP037") instanceof CharLineProvider);
			assertTrue(iop.getLineProvider(io, "UTF-016") instanceof CharLineProvider);
		}
		
		assertTrue(iop.getLineProvider(Constants.IO_XML_BUILD_LAYOUT, "") instanceof XmlLineProvider);
		assertTrue(iop.getLineProvider(Constants.IO_XML_USE_LAYOUT, "") instanceof XmlLineProvider);
	}

}
