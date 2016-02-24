package net.sf.JRecord.zTest.io;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zTest.Common.TstConstants;
import junit.framework.TestCase;

public class TstContinuousIO02 extends TestCase {

	private String copybookName = TstConstants.COBOL_DIRECTORY + "FCUSDAT.cbl";
	private String filename     = TstConstants.SAMPLE_DIRECTORY + "ZOS.FCUSTDAT_150.vb.bin";
	public void test01() throws FileNotFoundException, IOException {
		ICobolIOBuilder  iobVb = JRecordInterface1.COBOL
				.newIOBuilder(copybookName)
				.setFileOrganization(Constants.IO_VB)
				.setFont("cp037");
		ICobolIOBuilder  iobC = JRecordInterface1.COBOL
				.newIOBuilder(copybookName)
				.setFileOrganization(Constants.IO_CONTINOUS_NO_LINE_MARKER)
				.setFont("cp037");
		AbstractLineReader r = iobVb.newReader(filename);
		ByteArrayOutputStream os1 = new ByteArrayOutputStream(20000);
		//ByteArrayOutputStream os2 = new ByteArrayOutputStream(20000);
		ByteArrayOutputStream os3 = new ByteArrayOutputStream(20000);
		ArrayList<byte[]> inData = new ArrayList<byte[]>(150);
		
		//AbstractLineWriter wVb = iobVb.newWriter(os2);
		AbstractLineWriter wC  = iobC.newWriter(os3);
		AbstractLine line;
		
		while ((line = r.read()) != null) {
			byte[] data = line.getData();
			inData.add(data);
			os1.write(data);
			
			//wVb.write(line);
			wC .write(line);
		}
		os1.close();
		//wVb.close();
		wC.close();
		
		byte[] bytes1 = os1.toByteArray();
		byte[] bytes3 = os3.toByteArray();
		
		System.out.println("Checking the Continuous Writer !!!");
		assertTrue(Arrays.equals(bytes1, bytes3));
		
		AbstractLineReader rC = iobC.newReader(new ByteArrayInputStream(bytes1));
		int i = 0;
		while ((line = rC.read()) != null) {
			byte[] data = line.getData();
			assertTrue("Line: " + (i + 1), Arrays.equals(inData.get(i++), data));
		}
	}
}
