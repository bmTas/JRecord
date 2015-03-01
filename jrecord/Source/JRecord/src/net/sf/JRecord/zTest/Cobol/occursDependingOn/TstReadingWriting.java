package net.sf.JRecord.zTest.Cobol.occursDependingOn;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.Numeric.ICopybookDialects;
import junit.framework.TestCase;

public class TstReadingWriting extends TestCase {
	
	public void test01()  throws IOException, RecordException {
		tst("OccursDepending1.cbl", Constants.IO_STANDARD_TEXT_FILE);
	}
	
	public void test02()  throws IOException, RecordException {
		tst("OccursDepending1.cbl", Constants.IO_VB);
	}
	
	public void test03()  throws IOException, RecordException {
		tst("OccursDepending1.cbl", Constants.IO_STANDARD_UNICODE_TEXT_FILE);
	}
	
	public void test04()  throws IOException, RecordException {
		tst("OccursDepending2.cbl", Constants.IO_STANDARD_TEXT_FILE);
	}
	
	public void test05()  throws IOException, RecordException {
		tst("OccursDepending2.cbl", Constants.IO_VB_FUJITSU);
	}
	
	private void tst(String copybook, int fileOrg) throws IOException, RecordException {
		String copybookFileName = WriteSampleFile.class.getResource("OccursDepending1.cbl").getFile();
		
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(copybookFileName, ICopybookDialects.FMT_MAINFRAME)
					.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE);
		for (int i = 0; i < 16; i++) {
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			AbstractLineWriter w = ioBuilder.newWriter(out);
			try {
				for (int j = 0; j < 12; j++) {
					w.write(Code.generateLine(ioBuilder.newLine(), i, j));
				}
			} finally {
				w.close();
			}
			AbstractLineReader r= ioBuilder.newReader(new ByteArrayInputStream(out.toByteArray()));
			try {
				for (int j = 0; j < 12; j++) {
					Code.checkLine(r.read(), i, j);
				}
			} finally {
				r.close();
			}
		}
	}
}
