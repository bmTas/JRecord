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

package net.sf.JRecord.zTest.Cobol;

import java.io.ByteArrayInputStream;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Log.TextLog;


import net.sf.JRecord.Numeric.ICopybookDialects;
import junit.framework.TestCase;

/**
 * This class checks the type and positive attribute for
 * various Cobol Numeric Types
 *
 * @author Bruce Martin
 *
 */
public class TstCobol2 extends TestCase {

	private static final String BLANK_FILLER_COPYBOOK1
					=  "           01  ZZ-TTT5.\n"
					 + "             03 f01         pic 999V99.\n"
					 + "             03             pic x.\n"
					 + "             03 f02         pic 999V99 comp-3.\n"
					 + "             03             pic x.\n"
					 + "             03 f03         pic 999V99 comp.\n"
					 + "             03             pic x.\n"
					 + "             03 f04         pic 999V99 comp-5.\n"
					 + "\n"
					 + "             03 f05         pic 9(08).\n"
					 + "             03      redefines f05.\n"
					 + "               05 yyyy      pic 9(4).\n"
					 + "               05 MM        pic 99.\n"
					 + "               05 DD        pic 99.\n";
	private static final String BLANK_FILLER_COPYBOOK2
					= BLANK_FILLER_COPYBOOK1
					 + "             03             pic x(10).\n";


	private static final String MIXED_FILLER_COPYBOOK
				=  "           01  ZZ-TTT5.\n"
				 + "             03 f01         pic 999V99.\n"
				 + "             03 filler      pic x.\n"
				 + "             03 f02         pic 999V99 comp-3.\n"
				 + "             03             pic x.\n"
				 + "             03 f03         pic 999V99 comp.\n"
				 + "             03 filler      pic x.\n"
				 + "             03 f04         pic 999V99 comp-5.\n"
				 + "\n"
				 + "             03 f05         pic 9(08).\n"
				 + "             03        redefines f05.\n"
				 + "               05 yyyy      pic 9(4).\n"
				 + "               05 MM        pic 99.\n"
				 + "               05 DD        pic 99.\n"
				 + "             03 filler      pic x(10).\n";



	private static final String FILLER_COPYBOOK
				=  "           01  ZZ-TTT5.\n"
				 + "             03 f01         pic 999V99.\n"
				 + "             03 filler      pic x.\n"
				 + "             03 f02         pic 999V99 comp-3.\n"
				 + "             03 filler      pic x.\n"
				 + "             03 f03         pic 999V99 comp.\n"
				 + "             03 filler      pic x.\n"
				 + "             03 f04         pic 999V99 comp-5.\n"
				 + "\n"
				 + "             03 f05         pic 9(08).\n"
				 + "             03 filler redefines f05.\n"
				 + "               05 yyyy      pic 9(4).\n"
				 + "               05 MM        pic 99.\n"
				 + "               05 DD        pic 99.\n"
				 + "             03 filler      pic x(10).\n";



	private int[][] intelFields = {
			{ 1, 5, 22}, { 7, 3, 33}, {11, 4, 23}, {16, 4, 23}, {20, 8, 25},
			{20, 4, 25}, {24, 2, 25}, {26, 2, 25}, {28, 10, 0}, 			};
	private int[][] mainframeFields = {
			{ 1, 5, 22}, { 7, 3, 33}, {11, 4, 39}, {16, 4, 39}, {20, 8, 25},
			{20, 4, 25}, {24, 2, 25}, {26, 2, 25}, {28, 10, 0}, 			};
	private String[] blankNames  = {"f01", "f02", "f03", "f04", "f05", "yyyy", "MM", "DD", "", };
	private String[] fillerNames = {"f01", "f02", "f03", "f04", "f05", "yyyy", "MM", "DD", "filler", };


	public void testBlankConversion() throws RecordException {

		System.out.println();
		System.out.println("Blank Intel 1: ");
		testConversion(ICopybookDialects.FMT_INTEL, intelFields, blankNames, BLANK_FILLER_COPYBOOK1, true);

		System.out.println();
		System.out.println("Blank Mainframe 1: ");
		testConversion(ICopybookDialects.FMT_MAINFRAME, mainframeFields, blankNames, BLANK_FILLER_COPYBOOK1, true);

		System.out.println();
		System.out.println("Blank Intel 2: ");
		testConversion(ICopybookDialects.FMT_INTEL, intelFields, blankNames, BLANK_FILLER_COPYBOOK2);

		System.out.println();
		System.out.println("Blank Mainframe 2: ");
		testConversion(ICopybookDialects.FMT_MAINFRAME, mainframeFields, blankNames, BLANK_FILLER_COPYBOOK2);
	}


	public void testMixedConversion() throws RecordException {

		System.out.println();
		System.out.println("Blank Intel: ");
		testConversion(ICopybookDialects.FMT_INTEL, intelFields, fillerNames, FILLER_COPYBOOK);

		System.out.println();
		System.out.println("Blank Mainframe: ");
		testConversion(ICopybookDialects.FMT_MAINFRAME, mainframeFields, fillerNames, FILLER_COPYBOOK);
	}



	public void testFillerConversion() throws RecordException {

		System.out.println();
		System.out.println("Blank Intel: ");
		testConversion(ICopybookDialects.FMT_INTEL, intelFields, fillerNames, MIXED_FILLER_COPYBOOK);

		System.out.println();
		System.out.println("Blank Mainframe: ");
		testConversion(ICopybookDialects.FMT_MAINFRAME, mainframeFields, fillerNames, MIXED_FILLER_COPYBOOK);
	}


	public void testConversion(int cobolDialect, int[][] fields, String[] names, String copybook) throws RecordException {
		testConversion(cobolDialect, fields, names, copybook, false);
	}


	public void testConversion(int cobolDialect, int[][] fields, String[] names, String copybook, boolean differentLengths) throws RecordException {
		LayoutDetail l = getCobolLayout(cobolDialect, copybook);

		System.out.println();
		System.out.print("{");
		for (int i = 0; i < l.getRecord(0).getFieldCount(); i++) {
			FieldDetail field = l.getRecord(0).getField(i);

			System.out.print("{" + field.getPos() + ", " + field.getLen() + ", " + field.getType() + "}, ");
		}
		System.out.println("};");
		System.out.print("{");
		for (int i = 0; i < l.getRecord(0).getFieldCount(); i++) {
			FieldDetail field = l.getRecord(0).getField(i);

			System.out.print("\"" + field.getName() + "\", ");
		}
		System.out.println("};");
		System.out.println();

		if (differentLengths) {
			assertEquals(fields.length - 1, l.getRecord(0).getFieldCount());
			assertEquals(names.length - 1, l.getRecord(0).getFieldCount());
		} else {
			assertEquals(fields.length, l.getRecord(0).getFieldCount());
			assertEquals(names.length, l.getRecord(0).getFieldCount());
		}
		for (int i = 0; i < l.getRecord(0).getFieldCount(); i++) {
			FieldDetail field = l.getRecord(0).getField(i);

			//assertTrue("Field: " + field.getName(), t instanceof TypeNum && ((TypeNum) t).isPositive());
			//assertEquals(types[i], field.getType());
			assertEquals("Field Name: ", names[i], field.getName());
			assertEquals("Field Position: ", fields[i][0], field.getPos());
			assertEquals("Field Length: ",   fields[i][1], field.getLen());
			assertEquals("Field Type: ",     fields[i][2], field.getType());
		}

	}



	private static LayoutDetail getCobolLayout(int cobolDialect, String cobolCopybook) throws RecordException {
		CobolCopybookLoader loader = new CobolCopybookLoader();
		ByteArrayInputStream bs = new ByteArrayInputStream(cobolCopybook.getBytes());


		return loader.loadCopyBook(bs, "ZZ-TTT4", CopybookLoader.SPLIT_01_LEVEL, 0, "", cobolDialect, 0, new TextLog())
				.asLayoutDetail();
	}
}
