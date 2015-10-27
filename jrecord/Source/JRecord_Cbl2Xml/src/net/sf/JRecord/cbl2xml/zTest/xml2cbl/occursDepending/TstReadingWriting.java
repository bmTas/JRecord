package net.sf.JRecord.cbl2xml.zTest.xml2cbl.occursDepending;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import javax.xml.bind.JAXBException;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.cbl2xml.impl.Cobol2GroupXml;
import net.sf.JRecord.cbl2xml.zTest.xml2cbl.Cb2XmlCode;
import net.sf.JRecord.cbl2xml.zTest.xml2cbl.Code;
import net.sf.JRecord.cbl2xml.zTest.xml2cbl.TstXmlConstants;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
//import net.sf.JRecord.zTest.Cobol.occursDependingOn.Code;
//import net.sf.JRecord.zTest.Cobol.occursDependingOn.WriteSampleFile;
import junit.framework.TestCase;

public class TstReadingWriting extends TestCase {
	
	private static final int MAX_SALES_COUNT = 12;

	public void test01()  throws IOException, RecordException,  XMLStreamException, JAXBException {
		tst("OccursDepending1.cbl", Constants.IO_STANDARD_TEXT_FILE, "1");
	}
	
//	public void test02()  throws IOException, RecordException {
//		tst("OccursDepending1.cbl", Constants.IO_VB);
//	}
//	
//	public void test03()  throws IOException, RecordException {
//		tst("OccursDepending1.cbl", Constants.IO_STANDARD_UNICODE_TEXT_FILE);
//	}
	
	public void test04()  throws IOException, RecordException, XMLStreamException, JAXBException {
		tst("OccursDepending2.cbl", Constants.IO_STANDARD_TEXT_FILE, "2");
	}
//	
//	public void test05()  throws IOException, RecordException {
//		tst("OccursDepending2.cbl", Constants.IO_VB_FUJITSU);
//	}
	
	private void tst(String copybook, int fileOrg, String id) throws IOException, RecordException, XMLStreamException, JAXBException {
		String copybookFileName = TstXmlConstants.COBOL_DIRECTORY + copybook;
		
		ICobolIOBuilder ioBuilder = JRecordInterface1.COBOL
				.newIOBuilder(copybookFileName)
				    .setDialect(ICopybookDialects.FMT_MAINFRAME)
					.setFileOrganization(fileOrg);
		for (int i = 0; i < 16; i++) {
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			AbstractLineWriter w = ioBuilder.newWriter(out);
			try {
				for (int j = 0; j <= MAX_SALES_COUNT; j++) {
					w.write(Code.generateLine(ioBuilder.newLine(), i, j));
				}
			} finally {
				w.close();
			}
			
			ICobol2Xml cbl2xml = Cobol2GroupXml.newCobol2Xml(copybookFileName)
		      .setFileOrganization(fileOrg)
		      .setDialect(ICopybookDialects.FMT_MAINFRAME);
			
			System.out.println("--> " + i);
//			String xmlFileName = TstConstants.TEMP_DIRECTORY + "XmlOut" + id + "_" + i + ".xml";
			String xmlFileName = TstXmlConstants.XML_DIRECTORY + "XmlOut" + id + "_" + i + ".xml";
			System.out.println(xmlFileName);
			//FileOutputStream outStream = new FileOutputStream(xmlFileName);
			byte[] bytes = out.toByteArray();
			ByteArrayOutputStream xmlOut = new ByteArrayOutputStream();
			cbl2xml.cobol2xml(new ByteArrayInputStream(bytes), 
					xmlOut);
			xmlOut.close();
			byte[] xmlByteArray = xmlOut.toByteArray();
			Cb2XmlCode.compare("XmlCompare " +id + " " + i, xmlFileName, xmlByteArray);
			
			ByteArrayOutputStream out2 = new ByteArrayOutputStream();
			ByteArrayInputStream xmlInStream = new ByteArrayInputStream(xmlByteArray);
			cbl2xml.xml2Cobol(xmlInStream, out2);
			xmlInStream.close();
			System.out.println();
			System.out.println();
			
			checkCobolData(ioBuilder, i, out);
			checkCobolData(ioBuilder, i, out2);
		}
	}

	/**
	 * @param ioBuilder
	 * @param i
	 * @param out2
	 * @throws IOException
	 * @throws RecordException
	 */
	private void checkCobolData(ICobolIOBuilder ioBuilder, int i,
			ByteArrayOutputStream out2) throws IOException, RecordException {
		AbstractLineReader r= ioBuilder.newReader(new ByteArrayInputStream(out2.toByteArray()));
		try {
			for (int j = 0; j <= MAX_SALES_COUNT; j++) {
				Code.checkLine(r.read(), i, j);
			}
		} finally {
			r.close();
		}
	}
}
