package net.sf.JRecord.zTest.Cobol.occursDependingOn;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
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
	
	public void test06()  throws IOException, RecordException {
		tstSales("OccursDepending3.cbl", Constants.IO_VB, true);
	}

	
	public void test07()  throws IOException, RecordException {
		tstSales("OccursDepending4.cbl", Constants.IO_VB, true);
	}
	
	
	public void test08()  throws IOException, RecordException {
		tstSales("OccursDepending5.cbl", Constants.IO_VB_OPEN_COBOL, false);
	}
	
	private void tst(String copybook, int fileOrg) throws IOException, RecordException {
		String copybookFileName = WriteSampleFile.class.getResource(copybook).getFile();
		
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(copybookFileName, ICopybookDialects.FMT_MAINFRAME)
					.setFileOrganization(fileOrg);
		for (int i = 0; i < 16; i++) {
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			AbstractLineWriter w = ioBuilder.newWriter(out);
			try {
				for (int j = 0; j <= 12; j++) {
					w.write(Code.generateLine(ioBuilder.newLine(), i, j));
				}
			} finally {
				w.close();
			}
			AbstractLineReader r= ioBuilder.newReader(new ByteArrayInputStream(out.toByteArray()));
			try {
				for (int j = 0; j <= 12; j++) {
					Code.checkLine(r.read(), i, j);
				}
			} finally {
				r.close();
			}
		}
	}
	
	
	private void tstSales(String copybook, int fileOrg, boolean hasValue) throws IOException, RecordException {
		String copybookFileName = WriteSampleFile.class.getResource(copybook).getFile();
		
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(copybookFileName, ICopybookDialects.FMT_MAINFRAME)
					.setFileOrganization(fileOrg);
		for (int i = 0; i < 16; i++) {
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			AbstractLineWriter w = ioBuilder.newWriter(out);
			try {
				for (int j = 0; j <= 12; j++) {
					w.write(Code.generateSalesLine(ioBuilder.newLine(), i, j, hasValue));
				}
			} finally {
				w.close();
			}
			AbstractLineReader r= ioBuilder.newReader(new ByteArrayInputStream(out.toByteArray()));
			try {
				for (int j = 0; j <= 12; j++) {
					Code.checkSalesRecord(r.read(), i, j, hasValue);
				}
			} finally {
				r.close();
			}
		}
	}

}
