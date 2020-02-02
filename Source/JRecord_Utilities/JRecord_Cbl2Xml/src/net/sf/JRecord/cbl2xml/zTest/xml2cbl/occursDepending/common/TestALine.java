package net.sf.JRecord.cbl2xml.zTest.xml2cbl.occursDepending.common;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;


import javax.xml.stream.XMLStreamException;

import junit.framework.TestCase;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;

public class TestALine {

	private final CblItem cblItem;
	private final ICobol2Xml cbl2Xml;
	private final ISchemaIOBuilder iob;
	private int testsRun = 0;
//	ByteArrayOutputStream osXml;
//	ByteArrayOutputStream osCbl;


	public TestALine(CblItem cblItem, ICobol2Xml cbl2Xml) {
		super();
		
		this.cblItem = cblItem;
		this.cbl2Xml = cbl2Xml;
		this.iob = cbl2Xml.asIOBuilder();
		
//		int len = 500;
//		
//		try {
//			len = iob.getLayout().getMaximumRecordLength();
//		} catch (IOException e) {
//			e.printStackTrace();
//		}
//		
//		 osXml = new ByteArrayOutputStream(Math.max(500, len * 10));
//		 osCbl = new ByteArrayOutputStream(Math.max(50, len + 5));
	}


	/**
	 * @param indexs
	 * @throws RuntimeException
	 */
	public void doTest(ArrayIndex indexs) throws RuntimeException {
		doTest(indexs.toString(), indexs);
	}
	
	public void doTest(String id, ArrayIndex indexs) throws RuntimeException {

		AbstractLine l = null;
		String xml = ProcessCobolTree.generateXml(cblItem, indexs);
		
		ByteArrayOutputStream osXml = new ByteArrayOutputStream(xml.length());
		ByteArrayOutputStream osCbl = new ByteArrayOutputStream();
		String actual;

		try {
			l = ProcessCobolTree.generateLine(iob.newLine(), cblItem, indexs);
			cbl2Xml.cobol2xml(new ByteArrayInputStream(l.getData()), osXml);
			cbl2Xml.xml2Cobol(new ByteArrayInputStream(xml.getBytes()), osCbl);
			osXml.close();
			osCbl.close();
		} catch (IOException e) {
			throw new RuntimeException(e);
		} catch (XMLStreamException e) {
			throw new RuntimeException(e);
		}
		
		actual = new String(osXml.toByteArray());
		doCheck(id, xml, actual);
		
		actual = osCbl.toString();//new String(osCbl.toByteArray());
		while (actual.endsWith("\n") || actual.endsWith("\r")) {
			actual = actual.substring(0, actual.length() - 1);
		}
		doCheck(id, l.getFullLine(), actual);
		testsRun += 1;
		osXml = null;
		osCbl = null;		
		//System.out.println(">>>" + xml);
		//System.out.print('.');
	}


	/**
	 * @return the testsRun
	 */
	public final int getTestsRun() {
		return testsRun;
	}


	/**
	 * @param expected
	 * @param actual
	 */
	public void doCheck(String id, String expected, String actual) {
		if (! expected.equals(actual)) {
			TestCase.assertEquals( id, expected, actual);
		}
	}

}
