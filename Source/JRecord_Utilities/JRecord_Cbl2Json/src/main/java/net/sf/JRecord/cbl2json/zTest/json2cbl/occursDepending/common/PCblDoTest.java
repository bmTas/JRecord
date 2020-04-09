package net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common;

import java.io.StringReader;

import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.def.ICobol2Json;

public class PCblDoTest extends PCblBase {

	private boolean doTest = false;
	

	private final TestALine testALine;
	
	public PCblDoTest(CblItem cblItem) {
		String cobol = ProcessCobolTree.generateCobol(cblItem);
		
		ICobol2Json  cbl2Xml = Cobol2Json.newCobol2Json(new StringReader(cobol), cblItem.name)
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
