package net.sf.JRecord.zTest.Cobol.occursDependingOn;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import junit.framework.TestCase;

public class TstReadingWriting11 extends TestCase {
	
	public void test01()  throws IOException, RecordException {
		tst("ArrayDep.cbl", Constants.IO_STANDARD_TEXT_FILE, false);
	}
	
	
	public void test02()  throws IOException, RecordException {
		tst("ArrayDep2.cbl", Constants.IO_STANDARD_TEXT_FILE, true);
	}
	
	private void tst(String copybook, int fileOrg, boolean sep) throws IOException, RecordException {
		String copybookFileName = WriteSampleFile.class.getResource(copybook).getFile();
		
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(copybookFileName, ICopybookDialects.FMT_MAINFRAME)
					.setFileOrganization(fileOrg);
		StringBuilder b = new StringBuilder();
		AbstractLine lastLine = null;
		for (int i = 0; i <= 5; i++) {
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			AbstractLineWriter w = ioBuilder.newWriter(out);
			try {
				for (int j = 0; j <= 5; j++) {
					lastLine = generateLine(ioBuilder.newLine(), i, j, sep);
					w.write(lastLine);
				}
			} finally {
				w.close();
			}
			if (i > 2) {
				System.out.print(lastLine.getFieldValue("Level (2)").asString() + " ");
				System.out.print(lastLine.getFieldValue("Attr (2, 2)").asString());
			}
			byte[] byteArray = out.toByteArray();
			b.append(new String(byteArray)).append('\n');
			//System.out.println(new String(byteArray));
			AbstractLineReader r= ioBuilder.newReader(new ByteArrayInputStream(byteArray));
			try {
				for (int j = 0; j <= 5; j++) {
					checkLine(r.read(), i, j);
				}
			} finally {
				r.close();
			}
		}
		System.out.println(b.toString());
	}
	
	private AbstractLine generateLine(AbstractLine line, int levelCount, int attrCount, boolean sep) throws RecordException {
		line.getFieldValue("Record-Type").set("r");
		line.getFieldValue("Level-Count").set(levelCount);
		line.getFieldValue("Attr-Count").set(attrCount);
		
		for (int i = 0; i < levelCount; i++) {
			if (sep) {
				line.getFieldValue("sep1 (" + i + ")").set("~");
			}
			line.getFieldValue("Level (" + i + ")").set(i * 100 + attrCount);
			for (int j = 0; j < attrCount; j++) {
				line.getFieldValue("Attr (" + i + ", " + j + ")").set(i * 10 + j);
				if (sep) {
					line.getFieldValue("sep2 ("+ i + ", " + j + ")").set(" ");
				}
			}
		}
		return line;
	}

	private void checkLine(AbstractLine line, int levelCount, int attrCount) throws RecordException {
		assertEquals("r", line.getFieldValue("Record-Type").asString());
		assertEquals(levelCount, line.getFieldValue("Level-Count").asInt());
		assertEquals(attrCount, line.getFieldValue("Attr-Count").asInt());
		
		
		String pref = levelCount + ", " + attrCount + " : ";
		
		for (int i = 0; i < levelCount; i++) {
			assertEquals(pref + i, i * 100 + attrCount, line.getFieldValue("Level (" + i + ")").asInt());
			for (int j = 0; j < attrCount; j++) {
				assertEquals(pref + i + ", " + j, i * 10 + j, line.getFieldValue("Attr (" + i + ", " + j + ")").asInt());
			}
		}

	}

}
