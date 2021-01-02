/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */

package net.sf.JRecord.zTest.detailsSelection;

import junit.framework.TestCase;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.detailsSelection.FieldSelect;
import net.sf.JRecord.detailsSelection.FieldSelectX;
import net.sf.JRecord.zTest.Common.TestCommonCode;

public class TstFieldSelectX1 extends TestCase {

	public static final String[][] GET_RESULTS_TEXT_FIELD = {
		{"=", "net.sf.JRecord.detailsSelection.FieldSelectX$EqualsSelect", "false"},
		{"eq", "net.sf.JRecord.detailsSelection.FieldSelectX$EqualsSelect", "false"},
		{"!=", "net.sf.JRecord.detailsSelection.FieldSelectX$NotEqualsSelect", "false"},
		{"<>", "net.sf.JRecord.detailsSelection.FieldSelectX$NotEqualsSelect", "false"},
		//{"<>", "net.sf.JRecord.detailsSelection.FieldSelectX$EqualsSelect", "false"},
		{"ne", "net.sf.JRecord.detailsSelection.FieldSelectX$NotEqualsSelect", "false"},
		{"<> (Text)", "net.sf.JRecord.detailsSelection.FieldSelectX$NotEqualsSelect", "false"},
		{"<> (Numeric)", "net.sf.JRecord.detailsSelection.FieldSelectX$NotEqualsSelect", "true"},
		{">", "net.sf.JRecord.detailsSelection.FieldSelectX$GreaterThan", "false"},
		{"gt", "net.sf.JRecord.detailsSelection.FieldSelectX$GreaterThan", "false"},
		{">=", "net.sf.JRecord.detailsSelection.FieldSelectX$GreaterThan", "false"},
		{"ge", "net.sf.JRecord.detailsSelection.FieldSelectX$GreaterThan", "false"},
		{"<", "net.sf.JRecord.detailsSelection.FieldSelectX$LessThan", "false"},
		{"lt", "net.sf.JRecord.detailsSelection.FieldSelectX$LessThan", "false"},
		{"<=", "net.sf.JRecord.detailsSelection.FieldSelectX$LessThan", "false"},
		{"le", "net.sf.JRecord.detailsSelection.FieldSelectX$LessThan", "false"},
		{"Starts With", "net.sf.JRecord.detailsSelection.FieldSelect$StartsWith", ""},
		{"Doesn't Contain", "net.sf.JRecord.detailsSelection.FieldSelect$DoesntContain", ""},
		{"Contains", "net.sf.JRecord.detailsSelection.FieldSelect$Contains", ""},
		{"= (Numeric)", "net.sf.JRecord.detailsSelection.FieldSelectX$EqualsSelect", "true"},
		{"> (Numeric)", "net.sf.JRecord.detailsSelection.FieldSelectX$GreaterThan", "true"},
		{">= (Numeric)", "net.sf.JRecord.detailsSelection.FieldSelectX$GreaterThan", "true"},
		{"< (Numeric)", "net.sf.JRecord.detailsSelection.FieldSelectX$LessThan", "true"},
		{"<= (Numeric)", "net.sf.JRecord.detailsSelection.FieldSelectX$LessThan", "true"},
		{"= (Text)", "net.sf.JRecord.detailsSelection.FieldSelectX$EqualsSelect", "false"},
		{"> (Text)", "net.sf.JRecord.detailsSelection.FieldSelectX$GreaterThan", "false"},
		{">= (Text)", "net.sf.JRecord.detailsSelection.FieldSelectX$GreaterThan", "false"},
		{"< (Text)", "net.sf.JRecord.detailsSelection.FieldSelectX$LessThan", "false"},
		{"<= (Text)", "net.sf.JRecord.detailsSelection.FieldSelectX$LessThan", "false"},
		{"Regular Expression", "net.sf.JRecord.detailsSelection.FieldSelect$RegularEx", ""},
	};


	public static final String[][] GET_RESULTS_NUM_FIELD = {
		{"=", "net.sf.JRecord.detailsSelection.FieldSelectX$EqualsSelect", "true"},
		{"eq", "net.sf.JRecord.detailsSelection.FieldSelectX$EqualsSelect", "true"},
		{"!=", "net.sf.JRecord.detailsSelection.FieldSelectX$NotEqualsSelect", "true"},
		//{"<>", "net.sf.JRecord.detailsSelection.FieldSelectX$EqualsSelect", "true"},
		{"<>", "net.sf.JRecord.detailsSelection.FieldSelectX$NotEqualsSelect", "true"},
		{"ne", "net.sf.JRecord.detailsSelection.FieldSelectX$NotEqualsSelect", "true"},
		{"<> (Text)", "net.sf.JRecord.detailsSelection.FieldSelectX$NotEqualsSelect", "false"},
		{"<> (Numeric)", "net.sf.JRecord.detailsSelection.FieldSelectX$NotEqualsSelect", "true"},
		{">", "net.sf.JRecord.detailsSelection.FieldSelectX$GreaterThan", "true"},
		{"gt", "net.sf.JRecord.detailsSelection.FieldSelectX$GreaterThan", "true"},
		{">=", "net.sf.JRecord.detailsSelection.FieldSelectX$GreaterThan", "true"},
		{"ge", "net.sf.JRecord.detailsSelection.FieldSelectX$GreaterThan", "true"},
		{"<", "net.sf.JRecord.detailsSelection.FieldSelectX$LessThan", "true"},
		{"lt", "net.sf.JRecord.detailsSelection.FieldSelectX$LessThan", "true"},
		{"<=", "net.sf.JRecord.detailsSelection.FieldSelectX$LessThan", "true"},
		{"le", "net.sf.JRecord.detailsSelection.FieldSelectX$LessThan", "true"},
		{"Starts With", "net.sf.JRecord.detailsSelection.FieldSelect$StartsWith", ""},
		{"Doesn't Contain", "net.sf.JRecord.detailsSelection.FieldSelect$DoesntContain", ""},
		{"Contains", "net.sf.JRecord.detailsSelection.FieldSelect$Contains", ""},
		{"= (Numeric)", "net.sf.JRecord.detailsSelection.FieldSelectX$EqualsSelect", "true"},
		{"> (Numeric)", "net.sf.JRecord.detailsSelection.FieldSelectX$GreaterThan", "true"},
		{">= (Numeric)", "net.sf.JRecord.detailsSelection.FieldSelectX$GreaterThan", "true"},
		{"< (Numeric)", "net.sf.JRecord.detailsSelection.FieldSelectX$LessThan", "true"},
		{"<= (Numeric)", "net.sf.JRecord.detailsSelection.FieldSelectX$LessThan", "true"},
		{"= (Text)", "net.sf.JRecord.detailsSelection.FieldSelectX$EqualsSelect", "false"},
		{"> (Text)", "net.sf.JRecord.detailsSelection.FieldSelectX$GreaterThan", "false"},
		{">= (Text)", "net.sf.JRecord.detailsSelection.FieldSelectX$GreaterThan", "false"},
		{"< (Text)", "net.sf.JRecord.detailsSelection.FieldSelectX$LessThan", "false"},
		{"<= (Text)", "net.sf.JRecord.detailsSelection.FieldSelectX$LessThan", "false"},

	};
	
	private static final String COBOL_COPBOOK 
		=  "       01 Detail-Record.\n"
		+ "          05 Field-1                                Pic X(04).\n"
		+ "          05 Field-2                                Pic X(05).\n"
		+ "          05 Field-3                                Pic X(07).\n";



	public void testGetTextField() {
		FieldSelect fs;
		String num;
		FieldDetail fd = new FieldDetail("", "", Type.ftChar, 0, "", 0, "");
		fd.setPosOnly(1);

		for (int i = 0; i < GET_RESULTS_TEXT_FIELD.length; i++) {
			fs = FieldSelectX.get("", "0", GET_RESULTS_TEXT_FIELD[i][0], fd);

			num = "";
			if (fs instanceof FieldSelectX) {
				num = "" + ((FieldSelectX) fs).isNumeric();
			}
			assertEquals("Checking Class For: " + GET_RESULTS_TEXT_FIELD[i][0], GET_RESULTS_TEXT_FIELD[i][1], fs.getClass().getName());
			assertEquals("Checking Numeric For: " + GET_RESULTS_TEXT_FIELD[i][0], GET_RESULTS_TEXT_FIELD[i][2], num);
			//System.out.println("\t{\"" + Constants.VALID_COMPARISON_OPERATORS[i]
			//		+"\", \"" + fs.getClass().getName() + "\", \"" + num + "\"}," );
		}
	}



	public void testGetNumField() {
		FieldSelect fs;
		String num;
		FieldDetail fd = new FieldDetail("", "", Type.ftNumLeftJustified, 0, "", 0, "");
		fd.setPosOnly(1);

		for (int i = 0; i < GET_RESULTS_NUM_FIELD.length; i++) {
			fs = FieldSelectX.get("", "0", GET_RESULTS_NUM_FIELD[i][0], fd);

			num = "";
			if (fs instanceof FieldSelectX) {
				num = "" + ((FieldSelectX) fs).isNumeric();
			}
			assertEquals("Checking Class For: " + GET_RESULTS_NUM_FIELD[i][0], GET_RESULTS_NUM_FIELD[i][1], fs.getClass().getName());
			assertEquals("Checking Numeric For: " + GET_RESULTS_NUM_FIELD[i][0], GET_RESULTS_NUM_FIELD[i][2], num);
//			System.out.println("\t{\"" + Constants.VALID_COMPARISON_OPERATORS[i]
//					+"\", \"" + fs.getClass().getName() + "\", \"" + num + "\"}," );
		}
	}

	public void testRegExp() {
		String[][] data = {
				{"1 .*", "1   ", "t"},
				{"1 .*", "1  2", "t"},
				{"1 .*", "1", "t"},
				{"1 .*", "2   ", "f"},
				{"1 .*", "2  2", "f"},
				{"1 .*", "2", "f"},
				{"12.*", "1234", "t"},
				{"12.*", "12 4", "t"},
				{"12.*", "123", "t"},
				{"12.*", "12", "t"},
				{"12.*", "1", "f"},
				{"1   .*", "1   ", "t"},
				{"1   .*", "1  2", "f"},
				{"1   .*", "1", "t"},
				{"1   .*", "12", "f"},
		};
		
		Line line = new Line(getLayout(), "            ");
		String fldName = "Field-1";
		FieldDetail fd = new FieldDetail(fldName, "", Type.ftChar, 0, "", 0, "");
		fd.setPosLen(1, 4); 
		for (String[] tstData : data) {
			FieldSelect fs = FieldSelectX.get(fldName, tstData[0], "Regular Expression", fd);
			line.getFieldValue(fd).set(tstData[1]);
			assertEquals(line.getFullLine() + " >" + tstData[0] + " >" + tstData[1],
					"t".equals(tstData[2]),
					fs.isSelected(line));
		} 
	}
	
	
	private LayoutDetail getLayout() throws RecordException {
		return TestCommonCode.getLayoutFromCobolStr(COBOL_COPBOOK, "DETAIL-RECORD",
				CopybookLoader.SPLIT_NONE, "", ICopybookDialects.FMT_MAINFRAME);

	}
}
