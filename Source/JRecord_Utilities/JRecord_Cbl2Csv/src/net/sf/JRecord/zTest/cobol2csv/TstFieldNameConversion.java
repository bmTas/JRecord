package net.sf.JRecord.zTest.cobol2csv;

import static org.junit.Assert.*;

import org.junit.Test;

import net.sf.JRecord.cbl2csv.args.FieldNameUpdaters;
import net.sf.JRecord.cbl2csv.args.IUpdateFieldName;

public class TstFieldNameConversion {

	@Test
	public void testDropMinus() {
		String[][] tsts = {
				{"Field-Name-1", "FieldName1"},
				{"Field Name 1", "FieldName1"},
				{"Field  Name-1", "FieldName1"},
				{" Field-Name-1 ", "FieldName1"},
				{" Field-Name 1 ", "FieldName1"},
				{"Field Name (1)", "FieldName(1)"},
		};
		
		runTests(FieldNameUpdaters.DROP_MINUS, tsts);
	}

	@Test
	public void testTo_() {
		String[][] tsts = {
				{"Field-Name-1", "Field_Name_1"},
				{"Field Name 1", "Field_Name_1"},
				{"Field  Name-1", "Field_Name_1"},
				{" Field-Name-1 ", "_Field_Name_1"},
				{" Field-Name 1 ", "_Field_Name_1"},
				{"Field Name (1)", "Field_Name_1"},
				{"Field Name (1,2)", "Field_Name_1_2"},
				{"Field Name (1, 2)", "Field_Name_1_2"},
				{"Field Name (1, 2,  3)", "Field_Name_1_2_3"},
		};
		
		runTests(FieldNameUpdaters.TO_UNDERSCORE, tsts);
	}


	@Test
	public void testToCCnoArray() {
		String[][] tsts = {
				{"Field-Name-1", "fieldName1"},
				{"Field Name 1", "fieldName1"},
				{"Field  Name-1", "fieldName1"},
				{" Field-Name-1 ", "fieldName1"},
				{" Field-Name 1 ", "fieldName1"},
				{"Field Name (1)", "fieldName1"},
				{"Field Name (1,2)", "fieldName1_2"},
				{"Field Name (1, 2)", "fieldName1_2"},
				{"Field Name (1, 2,  3)", "fieldName1_2_3"},
		};
		
		runCCTests(FieldNameUpdaters.TO_CAMEL_CASE_NO_ARRAY, tsts);
	}

	@Test
	public void testToCC() {
		String[][] tsts = {
				{"Field-Name-1", "fieldName1"},
				{"Field Name 1", "fieldName1"},
				{"Field  Name-1", "fieldName1"},
				{" Field-Name-1 ", "fieldName1"},
				{" Field-Name 1 ", "fieldName1"},
				{"Field Name (1)", "fieldName(1)"},
				{"Field Name (1,2)", "fieldName(1_2)"},
				{"Field Name (1, 2)", "fieldName(1_2)"},
				{"Field Name (1, 2,  3)", "fieldName(1_2_3)"},
		};
		
		runCCTests(FieldNameUpdaters.TO_CAMEL_CASE, tsts);
	}
	
	public void runCCTests(IUpdateFieldName updater, String[][] tsts) {
		runTests(updater, tsts);
		for (String[] l : tsts) {
			l[0] = l[0].toUpperCase();
		}
		runTests(updater, tsts);
		for (String[] l : tsts) {
			l[0] = l[0].toLowerCase();
		}
		runTests(updater, tsts);
	}
	
	public void runTests(IUpdateFieldName updater, String[][] tsts) {
		for (int i = 0; i < tsts.length; i++) {
			assertEquals(i + ": " + tsts[i][0], tsts[i][1], updater.updateName(tsts[i][0]));
		}
	}
}
