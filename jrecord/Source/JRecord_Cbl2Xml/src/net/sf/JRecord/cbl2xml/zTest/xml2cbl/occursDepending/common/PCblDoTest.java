package net.sf.JRecord.cbl2xml.zTest.xml2cbl.occursDepending.common;

import java.io.StringReader;

import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cbl2xml.Cobol2Xml;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;

public class PCblDoTest extends PCblBase {

	private boolean doTest = false;
	

	private final TestALine testALine;
	
	public PCblDoTest(CblItem cblItem) {
		String cobol = ProcessCobolTree.generateCobol(cblItem);
		
		ICobol2Xml  cbl2Xml = Cobol2Xml.newCobol2Xml(new StringReader(cobol), cblItem.name)
				.setDialect(ICopybookDialects.FMT_GNU_COBOL);
		this.testALine = new TestALine(cblItem, cbl2Xml);
	}


	@Override
	public void startArray(String name, ArrayIndex indexs) {
		if (doTest) {
			this.testALine.doTest(indexs);
		}
	}


	@Override
	public int getCount(CblItem c, ArrayIndex indexs, ArrayIndex countIndexs) {
		doTest = ! c.hasChildren; 
		return c.arraySize;
	}

}
