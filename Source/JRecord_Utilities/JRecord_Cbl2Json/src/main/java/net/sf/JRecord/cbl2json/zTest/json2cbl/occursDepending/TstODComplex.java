package net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending;

//import static org.junit.Assert.*;

import java.io.StringReader;

import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common.ArrayIndex;
import net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common.CblItem;
import net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common.ComplexCobolTreeTest;
import net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common.ProcessCobolTree;
import net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common.TestALine;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.def.ICobol2Json;

import org.junit.Test;

public class TstODComplex {

	CblItem size1, size11, size111, size112, size12, size2;
	CblItem cblComplex01 = CblItem.groupField("OD-Test3",
			CblItem.intField("Fld1"),
			(size1 = CblItem.odArray("array1", "Size1", 4,
					CblItem.intField("Fld11"),
					CblItem.intField("Fld12"),
					(size11 = CblItem.odArray("array11", "Size11", 5,
							CblItem.intField("Fld111"),
							CblItem.intField("Fld112"),
							//CblItem.sizeField("Size111", 4),
							(size111 = CblItem.odArray("array111", "Size111", 4,
									CblItem.intField("Fld1111"),
									CblItem.intField("Fld1112")
							)),
							(size112 = CblItem.odArray("array112", "Size112", 3,
									CblItem.intField("Fld1121"),
									CblItem.intField("Fld1122"),
									CblItem.intField("Fld1123")
							)),
							CblItem.intField("Fld115")
					)),
					(size12 = CblItem.odArray("array12", "Size12", 4,
							CblItem.intField("Fld121")
					))
			)),
			(size2 = CblItem.odArray("array2", "Size2", 3,
					CblItem.intField("Fld21"),
					CblItem.intField("Fld22")
			))
	);
	
	static CblItem cblComplex02 = CblItem.groupField("OD-Test3",
			CblItem.intField("Fld1"),
			CblItem.odArray("array1", "Size1", 4,
					CblItem.groupField("Group11", 
						CblItem.intField("Fld11"),
						CblItem.intField("Fld12")
					),
					CblItem.odArray("array11", "Size11", 5,
							CblItem.intField("Fld111"),
							CblItem.intField("Fld112"),
							//CblItem.sizeField("Size111", 4),
							CblItem.odArray("array111", "Size111", 4,
									CblItem.intField("Fld1111"),
									CblItem.intField("Fld1112")
							),
							CblItem.odArray("array112", "Size112", 3,
									CblItem.intField("Fld1121"),
									CblItem.intField("Fld1122"),
									CblItem.intField("Fld1123")
							),
							CblItem.intField("Fld115"),
							CblItem.intOdArrayField("inta116", "size116", 5)
					),
					CblItem.odArray("array12", "Size12", 4,
							CblItem.intField("Fld121")
					)
			),
			CblItem.groupField("group2", 
				CblItem.odArray("array2", "Size2", 3,
						CblItem.intField("Fld21"),
						CblItem.intField("Fld22")
				),
				CblItem.array("array3", 3, 
						CblItem.intField("Fld31"),
						CblItem.intField("Fld32"),
						CblItem.intOdArrayField("intFlda33", "size33", 5)
				)
			)
	);


	@Test
	public void testCompexOccursDepending01() {
		String cobol = ProcessCobolTree.generateCobol(cblComplex01);
		
		ICobol2Json  cbl2Xml = Cobol2Json.newCobol2Json(new StringReader(cobol), cblComplex01.name)
				.setDialect(ICopybookDialects.FMT_GNU_COBOL);
		TestALine testALine = new TestALine(cblComplex01, cbl2Xml);
		int max = 999;

		//size1, size11, size111, size112, size12, size2;
		doTest(testALine, 1, 0, 0, 0);
		for (int s1 = 1; s1 <= size1.arraySize; s1++) {
			doTest(testALine, 1, s1, 0, 0);
			for (int s11 = 1; s11 <= size11.arraySize; s11++) {
				doTest(testALine, 1, s1, s11, 0);
				for (int s111 = 1; s111 <= size111.arraySize; s111++) {
					doTest(testALine, max, s1, s11, s111);
				}
			}
		}
	}


	/**
	 * @param testALine
	 * @param max
	 * @param s1
	 * @param s11
	 * @param s111
	 * @throws RuntimeException
	 */
	public void doTest(TestALine testALine, int max, int s1, int s11, int s111)
			throws RuntimeException {
		for (int s112 = 0; s112 <= size112.arraySize && s111 < max; s112++) {
			ArrayIndex ai = new ArrayIndex();
			for (int s12 = 0; s12 <= size12.arraySize && s12 < max; s12++) {
				for (int s2 = 1; s2 <= size2.arraySize; s2++) {
					String id = s1 + ", " + s11 + ", " + s111 + ", " + s112  + ", " + s12  + ", " + s2;
					size1.setCount(s1);
					size11.setCount(s11);
					size111.setCount(s111);
					size112.setCount(s112);
					size12.setCount(s12);
					size2.setCount(s2);
					
					testALine.doTest(id, ai);
				}
			}
		}
	}

	@Test
	public void testCompexOccursDepending02() {
		new ComplexCobolTreeTest(cblComplex02);
	}
	
	public static void main(String[] args) {
		new ComplexCobolTreeTest(cblComplex02);
	}
}
