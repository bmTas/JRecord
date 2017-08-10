package net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending;

import junit.framework.TestCase;
import net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common.CblItem;
import net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common.GenCobol;
import net.sf.JRecord.cbl2json.zTest.json2cbl.occursDepending.common.ProcessCobolTree;

public class TstOD4levels  extends TestCase {


	CblItem CblDef01 = CblItem.groupField("OD-Test1",
			CblItem.intField("Fld1"),
			//CblItem.sizeField("Size1", 4),
			CblItem.odArray("array1", "Size1", 4,
					CblItem.intField("Fld11"),
					CblItem.intField("Fld12")
			)
	);

	CblItem CblDef02 = CblItem.groupField("OD-Test2",
			CblItem.intField("Fld1"),
			//CblItem.sizeField("Size1", 4),
			CblItem.odArray("array1", "Size1", 4,
					CblItem.intField("Fld11"),
					CblItem.intField("Fld12"),
					//CblItem.sizeField("Size11", 4),
					CblItem.odArray("array11", "Size11", 5,
							CblItem.intField("Fld111"),
							CblItem.intField("Fld112")
					)
			)
	);

	CblItem CblDef03 = CblItem.groupField("OD-Test3",
			CblItem.intField("Fld1"),
			//CblItem.sizeField("Size1", 4),
			CblItem.odArray("array1", "Size1", 4,
					CblItem.intField("Fld11"),
					CblItem.intField("Fld12"),
					//CblItem.sizeField("Size11", 4),
					CblItem.odArray("array11", "Size11", 5,
							CblItem.intField("Fld111"),
							CblItem.intField("Fld112"),
							//CblItem.sizeField("Size111", 4),
							CblItem.odArray("array111", "Size111", 4,
									CblItem.intField("Fld1111"),
									CblItem.intField("Fld1112")
							)
					)
			)
	);
	

	CblItem CblDef04 = CblItem.groupField("OD-Test4",
			CblItem.intField("Fld1"),
			//CblItem.sizeField("Size1", 4),
			CblItem.odArray("array1", "Size1", 4,
					CblItem.intField("Fld11"),
					CblItem.intField("Fld12"),
					//CblItem.sizeField("Size11", 4),
					CblItem.odArray("array11", "Size11", 5,
							CblItem.intField("Fld111"),
							CblItem.intField("Fld112"),
							//CblItem.sizeField("Size111", 4),
							CblItem.odArray("array111", "Size111", 4,
									CblItem.intField("Fld1111"),
									CblItem.intField("Fld1112"),
									CblItem.odArray("array111", "Size1111", 3,
											CblItem.intField("Fld11111"),
											CblItem.intField("Fld11112")
									)
						)
					)
			)
	);

	
	public void test1level() {
		ProcessCobolTree.testConversion(CblDef01);
	}
	
	
	public void test2levels() {
		ProcessCobolTree.testConversion(CblDef02);
	}
	
	
	public void test3levels() {
		ProcessCobolTree.testConversion(CblDef03);
	}
	
	public void test4levels() {
		ProcessCobolTree.testConversion(CblDef04);
	}

	public void testCbl() {
		String cbl = GenCobol.genCobol(CblDef03);
		System.out.println(cbl);
	}

}
