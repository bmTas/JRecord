package net.sf.JRecord.zTest.Cobol.occursDependingOn;

import java.io.IOException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.Numeric.ICopybookDialects;
import junit.framework.TestCase;

public class TstReadingOccursDepending extends TestCase {
	
	public void test01()  throws IOException, RecordException {
		tst(Constants.IO_STANDARD_TEXT_FILE);
	}
	
	public void test02()  throws IOException, RecordException {
		tst(Constants.IO_STANDARD_UNICODE_TEXT_FILE);
	}

	
	private void tst(int io) throws IOException, RecordException {
		String copybookFileName = WriteSampleFile.class.getResource("OccursDepending1.cbl").getFile();
		String fileName = WriteSampleFile.class.getResource("OccursDependingOn.txt").getFile();
		
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(copybookFileName, ICopybookDialects.FMT_MAINFRAME)
					.setFileOrganization(io);
		AbstractLineReader r= ioBuilder.newReader(fileName);
		try {
			for (int i = 0; i < 16; i++) {
				for (int j = 0; j < 12; j++) {
					Code.checkLine(r.read(), i, j);
				}
			}
		} finally {
			r.close();
		}
	}
}
