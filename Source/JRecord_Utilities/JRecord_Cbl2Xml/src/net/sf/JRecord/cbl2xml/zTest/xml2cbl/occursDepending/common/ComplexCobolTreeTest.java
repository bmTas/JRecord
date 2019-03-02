package net.sf.JRecord.cbl2xml.zTest.xml2cbl.occursDepending.common;

import java.io.StringReader;
import java.util.ArrayList;

import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cbl2xml.Cobol2Xml;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;

public class ComplexCobolTreeTest {


	private final TestALine testALine;
	private final CblItem cobolItem;
	private final ArrayIndex emptyIndex = new ArrayIndex();


	public ComplexCobolTreeTest(CblItem cblItem) {
		this.cobolItem = cblItem;

		String cobol = ProcessCobolTree.generateCobol(cblItem);
		System.out.println();
		System.out.println(cobol);
		System.out.println();
		
		ICobol2Xml  cbl2Xml = Cobol2Xml.newCobol2Xml(new StringReader(cobol), cblItem.name)
				.setDialect(ICopybookDialects.FMT_GNU_COBOL);
		this.testALine = new TestALine(cblItem, cbl2Xml);

		doTest();
	}
	
	private void doTest() {
		ArrayList<CblItem> odArrays = new ArrayList<CblItem>();
		parseTree(odArrays, cobolItem);
		runTest(odArrays, 0);
	}

	private void parseTree(ArrayList<CblItem> odArrays, CblItem cblItem) {
		switch (cblItem.itemType) {
		case CblItem.INT_OD_ARRAY_FIELD:
		case CblItem.OD_ARRAY:
			odArrays.add(cblItem);
		}
		
		if (cblItem.hasChildren) {
			for (CblItem c : cblItem.children) {
				parseTree(odArrays, c);
			}
		}
	}
	
	private void runTest(ArrayList<CblItem> odArrays, int idx) {
		if (idx < odArrays.size()) {
			long time = System.currentTimeMillis();
			int count = odArrays.get(idx).arraySize;
			for (int i = 0; i <= count; i++) {
				odArrays.get(idx).setCount(i);
				runTest(odArrays, idx+1);
				if (idx == 0) {
					System.out.println("========= " + i + " " + odArrays.size() + " " + testALine.getTestsRun()
							+ " >> " + (System.currentTimeMillis() - time));
				}
			}
		} else {
			this.testALine.doTest(emptyIndex);
		}
	}
}
