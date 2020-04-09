package net.sf.JRecord.zTest.External;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Option.ICobolSplitOptions;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.IItem;


/**
 * Testing loading Cobol Item Tree
 * 
 * @author Bruce Martin
 *
 */
public class TstCobolLoader extends TestCase {

	public void testBasicCopybook() throws IOException {
		String basicCopybook 
				= "          03 field-1     pic x(3).\n"
				+ "          03 field-2     pic x(3).\n";
		
		String basic01Copybook  
				= "       01 group-1.\n"
				+ basicCopybook;
		CobolCopybookLoader loader = new CobolCopybookLoader();
		
		ExternalRecord xrec = loader.loadCopyBook(
				new StringReader(basicCopybook), "rec1", ICobolSplitOptions.SPLIT_NONE, 
				0, "", Cb2xmlConstants.USE_STANDARD_COLUMNS,
				ICopybookDialects.FMT_MAINFRAME, 0, null);
		
		
		tstSimpleCopybook(xrec.getItems());
		
		xrec = loader.loadCopyBook(
				new StringReader(basic01Copybook), "rec2", ICobolSplitOptions.SPLIT_NONE, 
				0, "", Cb2xmlConstants.USE_STANDARD_COLUMNS,
				ICopybookDialects.FMT_MAINFRAME, 0, null);
		
		List<? extends IItem> items = xrec.getItems();
		
		assertEquals(1, items.size());
		assertEquals("group-1", items.get(0).getFieldName());
		tstSimpleCopybook(items.get(0).getChildItems());
	}

	private void tstSimpleCopybook(List<? extends IItem> items) {
		assertEquals(2, items.size());
		assertEquals("field-1", items.get(0).getFieldName());
		assertEquals("field-2", items.get(1).getFieldName());
	}


	public void testSplit01() throws IOException {
		String groupsAt01 
				= "       01 group-1.\n"
				+ "          10 record-type    pic x(1).\n"
				+ "          10 field-1        pic x(3).\n"
				+ "       01 group-2.\n"
				+ "          10 record-type    pic x(1).\n"
				+ "          10 field-2        pic x(4).\n"
				+ "          10 field-3        pic x(3).\n";
		

		
		CobolCopybookLoader loader = new CobolCopybookLoader();
		ExternalRecord xrec = loader.loadCopyBook(
				new StringReader(groupsAt01), "rec1", ICobolSplitOptions.SPLIT_01_LEVEL, 
				0, "", Cb2xmlConstants.USE_STANDARD_COLUMNS,
				ICopybookDialects.FMT_MAINFRAME, 0, null);

		tstGroupRecord(xrec);
	}

	public void testSplitTopLevel() throws IOException {
		String repeatingGroups 
				= "       03 group-1.\n"
				+ "          10 record-type    pic x(1).\n"
				+ "          10 field-1        pic x(3).\n"
				+ "       03 group-2.\n"
				+ "          10 record-type    pic x(1).\n"
				+ "          10 field-2        pic x(4).\n"
				+ "          10 field-3        pic x(3).\n";
		
		String cpy2 
				= "       01 top-1.\n"
				+ repeatingGroups;

		
		CobolCopybookLoader loader = new CobolCopybookLoader();
		ExternalRecord xrec = loader.loadCopyBook(new StringReader(repeatingGroups), 
				"rec1", ICobolSplitOptions.SPLIT_TOP_LEVEL, 
				0, "", Cb2xmlConstants.USE_STANDARD_COLUMNS,
				ICopybookDialects.FMT_MAINFRAME, 0, null);

		tstGroupRecord(xrec);
		
		xrec = loader.loadCopyBook(new StringReader(repeatingGroups), 
				"rec1", ICobolSplitOptions.SPLIT_HIGHEST_REPEATING, 
				0, "", Cb2xmlConstants.USE_STANDARD_COLUMNS,
				ICopybookDialects.FMT_MAINFRAME, 0, null);

		tstGroupRecord(xrec);
		
		xrec = loader.loadCopyBook(new StringReader(cpy2), 
				"rec1", ICobolSplitOptions.SPLIT_HIGHEST_REPEATING, 
				0, "", Cb2xmlConstants.USE_STANDARD_COLUMNS,
				ICopybookDialects.FMT_MAINFRAME, 0, null);

		tstGroupRecord(xrec);

	}


	@SuppressWarnings("unchecked")
	public void testSplitRedefines() throws IOException {
		String basicRedefine 
				= "       03 record-type    pic x(1).\n"
				+ "       03 group-1.\n"
				+ "          10 field-1        pic x(3).\n"
				+ "       03 group-2 redefines group-1.\n"
				+ "          10 field-2        pic x(4).\n"
				+ "          10 field-3        pic x(3).\n"
				+ "       03 group-3 redefines group-1.\n"
				+ "          10 field-4        pic x(5).\n"
			;
		
		String redefineWith01level 
				= "       01 top-1.\n"
				+ basicRedefine;
		String redefineWithTrailingGroup
				= basicRedefine
				+ "       03 group-4.\n"
				+ "          10 field-5        pic x(3).\n";
		String redefineWith01levelAndTrailingGroup
				= redefineWith01level
				+ "       03 group-4.\n"
				+ "          10 field-5        pic x(3).\n";

		
		CobolCopybookLoader loader = new CobolCopybookLoader();
		ExternalRecord xrec;		
		
		xrec = loader.loadCopyBook(
				new StringReader(basicRedefine), 
				"rec1", ICobolSplitOptions.SPLIT_REDEFINE, 
				0, "", Cb2xmlConstants.USE_STANDARD_COLUMNS,
				ICopybookDialects.FMT_MAINFRAME, 0, null);

		assertEquals(3, xrec.getNumberOfRecords());
	
		tstRedef1(2, xrec.getRecord(0).getItems(), xrec.getRecord(1).getItems(), xrec.getRecord(2).getItems());

		   /* -------------------------------------------------------------- */
		xrec = loader.loadCopyBook(new StringReader(redefineWith01level), 
				"rec1", ICobolSplitOptions.SPLIT_REDEFINE, 
				0, "", Cb2xmlConstants.USE_STANDARD_COLUMNS,
				ICopybookDialects.FMT_MAINFRAME, 0, null);
		
		assertEquals(3, xrec.getNumberOfRecords());
		tstRedefIneIn01(2, xrec);
		
		   /* ---------------------------------------------------------------- */
		xrec = loader.loadCopyBook(new StringReader(redefineWithTrailingGroup), 
				"rec1", ICobolSplitOptions.SPLIT_REDEFINE, 
				0, "", Cb2xmlConstants.USE_STANDARD_COLUMNS,
				ICopybookDialects.FMT_MAINFRAME, 0, null);
		
		assertEquals(3, xrec.getNumberOfRecords());
		
		List<? extends IItem> g1 = xrec.getRecord(0).getItems();
		List<? extends IItem> g2 = xrec.getRecord(1).getItems();
		List<? extends IItem> g3 = xrec.getRecord(2).getItems();
		tstRedef1(3, g1, g2, g3);

		tstRedefFollowingGroups(g1, g2, g3);
		
		   /* ---------------------------------------------------------------- */
		xrec = loader.loadCopyBook(new StringReader(redefineWith01levelAndTrailingGroup), 
				"rec1", ICobolSplitOptions.SPLIT_REDEFINE, 
				0, "", Cb2xmlConstants.USE_STANDARD_COLUMNS,
				ICopybookDialects.FMT_MAINFRAME, 0, null);
		
		assertEquals(3, xrec.getNumberOfRecords());
		tstRedefIneIn01(3, xrec);
		
		g1 = xrec.getRecord(0).getItems();
		g2 = xrec.getRecord(1).getItems();
		g3 = xrec.getRecord(2).getItems();
	
		tstRedefFollowingGroupsWith01(g1, g2, g3);
	}

	private void tstRedefFollowingGroupsWith01(List<? extends IItem>... grps) {
		List<? extends IItem>[] childGrps = new ArrayList[grps.length];
		
		for (int i = 0; i < childGrps.length; i++) {
			assertEquals(1, grps[i].size());
			assertEquals("top-1", grps[i].get(0).getFieldName());
			childGrps[i] = grps[i].get(0).getChildItems();
		}
		
		tstRedef1(3, childGrps[0], childGrps[1], childGrps[2]);
		tstRedefFollowingGroups(childGrps);
	}

	private void tstRedefFollowingGroups(List<? extends IItem>... grp) {

		for (List<? extends IItem> g : grp) {
			assertEquals(3, g.size());
			IItem lastGrp = g.get(2);
			List<? extends IItem> lastGrpItms = lastGrp.getChildItems();
			assertEquals("group-4", lastGrp.getFieldName());
			assertEquals(1, lastGrpItms.size());
			assertEquals("field-5", lastGrpItms.get(0).getFieldName());
		}
	}
	private void tstRedefIneIn01(int recCount, ExternalRecord xrec) {
		List<? extends IItem> g1 = xrec.getRecord(0).getItems();
		List<? extends IItem> g2 = xrec.getRecord(1).getItems();
		List<? extends IItem> g3 = xrec.getRecord(2).getItems();

		assertEquals(1, g1.size());
		assertEquals(1, g2.size());
		assertEquals(1, g3.size());

		assertEquals("top-1", g1.get(0).getFieldName());
		assertEquals("top-1", g2.get(0).getFieldName());
		assertEquals("top-1", g3.get(0).getFieldName());
		assertEquals(1, g1.get(0).getPosition());
		assertEquals(1, g2.get(0).getPosition());
		assertEquals(1, g3.get(0).getPosition());

		tstRedef1(recCount, g1.get(0).getChildItems(), g2.get(0).getChildItems(), g3.get(0).getChildItems());
	}

	private void tstRedef1(int recCount, List<? extends IItem> g1, List<? extends IItem> g2, List<? extends IItem> g3) {
		assertEquals(recCount, g1.size());
		assertEquals(recCount, g2.size());
		assertEquals(recCount, g3.size());
		
		assertEquals("record-type", g1.get(0).getFieldName());
		assertEquals("record-type", g2.get(0).getFieldName());
		assertEquals("record-type", g3.get(0).getFieldName());
		assertEquals(1, g1.get(0).getPosition());
		assertEquals(1, g2.get(0).getPosition());
		assertEquals(1, g3.get(0).getPosition());
		assertEquals("group-1", g1.get(1).getFieldName());
		assertEquals("group-2", g2.get(1).getFieldName());
		assertEquals("group-3", g3.get(1).getFieldName());
		assertEquals(2, g1.get(1).getPosition());
		assertEquals(2, g2.get(1).getPosition());
		assertEquals(2, g3.get(1).getPosition());
		
		List<? extends IItem> g11 = g1.get(1).getChildItems();
		List<? extends IItem> g21 = g2.get(1).getChildItems();
		List<? extends IItem> g31 = g3.get(1).getChildItems();
		
		assertEquals(1, g11.size());
		assertEquals(2, g21.size());
		assertEquals(1, g31.size());
		
		assertEquals("field-1", g11.get(0).getFieldName());
		assertEquals("field-2", g21.get(0).getFieldName());
		assertEquals("field-3", g21.get(1).getFieldName());
		assertEquals("field-4", g31.get(0).getFieldName());
		assertEquals(2, g11.get(0).getPosition());
		assertEquals(2, g21.get(0).getPosition());
		assertEquals(6, g21.get(1).getPosition());
		assertEquals(2, g31.get(0).getPosition());
	}
	
	/**
	 * General multi-record test
	 * @param xrec
	 */
	private void tstGroupRecord(ExternalRecord xrec) {

		assertEquals(2, xrec.getNumberOfRecords());

		
		List<? extends IItem> g1 = xrec.getRecord(0).getItems();
		List<? extends IItem> g2 = xrec.getRecord(1).getItems();
		
		assertEquals(1, g1.size());
		assertEquals(1, g2.size());
		
		assertEquals("group-1", g1.get(0).getFieldName());
		assertEquals(1, g1.get(0).getPosition());
		assertEquals("group-2", g2.get(0).getFieldName());
		assertEquals(1, g2.get(0).getPosition());
		
		tstChildRecordItems(g1.get(0).getChildItems(), g2.get(0).getChildItems());
	}

	private void tstChildRecordItems(List<? extends IItem> group1, List<? extends IItem> group2) {
		
		assertEquals("record-type", group1.get(0).getFieldName());
		assertEquals("field-1", group1.get(1).getFieldName());
		assertEquals(1, group1.get(0).getPosition());
		assertEquals(2, group1.get(1).getPosition());

		assertEquals("record-type", group2.get(0).getFieldName());
		assertEquals(1, group1.get(0).getPosition());
		assertEquals("field-2", group2.get(1).getFieldName());
		assertEquals("field-3", group2.get(2).getFieldName());
		assertEquals(1, group2.get(0).getPosition());
		assertEquals(2, group2.get(1).getPosition());
		assertEquals(6, group2.get(2).getPosition());
	}
}
