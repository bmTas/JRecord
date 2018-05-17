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

package net.sf.JRecord.zTest.Numeric;

import net.sf.JRecord.Numeric.ConversionManager;
import net.sf.JRecord.Numeric.Convert;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Types.Type;
import net.sf.cb2xml.def.Cb2xmlConstants;
import junit.framework.TestCase;

public class TestCommaDecimalPictAnalysis  extends TestCase{

	Convert bc = ConversionManager.getInstance().getConverter4code(ICopybookDialects.FMT_MAINFRAME_COMMA_DECIMAL);

	public void testCharPictures() {
        chkPict("99/99/99", Type.ftChar);
        chkPict("99.99.99", Type.ftChar);
        chkPict("99-99-99", Type.ftChar);
        chkPict("Z9-Z9-99", Type.ftChar);
	}

	public void testNumericPictures() {
        chkPict("-999,99",Type.ftNumCommaDecimal);
        chkPict("9999,99",Type.ftNumCommaDecimalPositive);
        chkPict("+999,99",Type.ftNumCommaDecimalPN);
        chkPict("-9(2),99",Type.ftNumCommaDecimal);
        chkPict("99(2),99",Type.ftNumCommaDecimalPositive);
        chkPict("+9(2),99",Type.ftNumCommaDecimalPN);
        chkPict("9(3),99",Type.ftNumCommaDecimalPositive);
        chkPict("--9,99",Type.ftNumRightJustCommaDp);
        chkPict("ZZ9,99",Type.ftNumRightJustCommaDp);
        chkPict("zz9,99",Type.ftNumRightJustCommaDp);
        chkPict("+++9,99",Type.ftNumRightJustCommaDpPN);
        chkPict("-(2)9,99",Type.ftNumRightJustCommaDp);
        chkPict("Z(2)9,99",Type.ftNumRightJustCommaDp);
        chkPict("+(2)9,99",Type.ftNumRightJustCommaDpPN);
        chkPict("-,--9.99",Type.ftNumRightJustified);
        chkPict("Z,ZZ9.99",Type.ftNumRightJustified);
        chkPict("+,++9.99",Type.ftNumRightJustified);
        chkPict("-9,999.9",Type.ftNumRightJustified);
        chkPict("99,999.9",Type.ftNumRightJustified);
        chkPict("+9,999.9",Type.ftNumRightJustified);
        chkPict("S9999V99",Type.ftZonedNumeric);
        chkPict("s9999v99",Type.ftZonedNumeric);
        chkPict("9999V99",Type.ftAssumedDecimalPositive);
        chkPict("9999v99",Type.ftAssumedDecimalPositive);

        chkPict("-999",Type.ftNumZeroPadded);
        chkPict("9999",Type.ftNumZeroPaddedPositive);
        chkPict("+999",Type.ftNumZeroPaddedPN);
        chkPict("-9(2)",Type.ftNumZeroPadded);
        chkPict("99(2)",Type.ftNumZeroPaddedPositive);
        chkPict("+9(2)",Type.ftNumZeroPaddedPN);
        chkPict("9(3)",Type.ftNumZeroPaddedPositive);
        chkPict("--9",Type.ftNumRightJustified);
        chkPict("ZZ9",Type.ftNumRightJustified);
        chkPict("+++9",Type.ftNumRightJustifiedPN);
        chkPict("-(2)9",Type.ftNumRightJustified);
        chkPict("Z(2)9",Type.ftNumRightJustified);
        chkPict("+(2)9",Type.ftNumRightJustifiedPN);
        chkPict("-,--9",Type.ftNumRightJustified);
        chkPict("Z,ZZ9",Type.ftNumRightJustified);

        chkPict("-9,999",Type.ftNumCommaDecimal);
        chkPict("S9999",Type.ftZonedNumeric);
        chkPict("99999",Type.ftNumZeroPaddedPositive);


        chkPict("-999,",Type.ftNumCommaDecimal);
        chkPict("9999,",Type.ftNumCommaDecimalPositive);
        chkPict("+999,",Type.ftNumCommaDecimalPN);
        chkPict("-9(2),",Type.ftNumCommaDecimal);
        chkPict("99(2),",Type.ftNumCommaDecimalPositive);
        chkPict("+9(2),",Type.ftNumCommaDecimalPN);
        chkPict("9(3),",Type.ftNumCommaDecimalPositive);
        chkPict("--9,",Type.ftNumRightJustCommaDp);
        chkPict("ZZ9,",Type.ftNumRightJustCommaDp);
        chkPict("+++9,",Type.ftNumRightJustCommaDpPN);
        chkPict("-(2)9,",Type.ftNumRightJustCommaDp);
        chkPict("Z(2)9,",Type.ftNumRightJustCommaDp);
        chkPict("z(2)9,",Type.ftNumRightJustCommaDp);
        chkPict("+(2)9,",Type.ftNumRightJustCommaDpPN);
        chkPict("-,--9.",Type.ftNumRightJustified);
        chkPict("Z,ZZ9.",Type.ftNumRightJustified);
        chkPict("z,Zz9.",Type.ftNumRightJustified);
        chkPict("+,++9.",Type.ftNumRightJustified);
        chkPict("-9,999.",Type.ftNumRightJustified);
        chkPict("99,999.",Type.ftNumRightJustified);
        chkPict("+9,999.",Type.ftNumRightJustified);
        chkPict("S9999V",Type.ftZonedNumeric);
        chkPict("s9999v",Type.ftZonedNumeric);
        chkPict("9999V",Type.ftAssumedDecimalPositive);
	}

	public void testNumericCompPictures() {
		chkCompPict("", "S9999V99",Type.ftBinaryBigEndian);
		chkCompPict("", "s9999v99",Type.ftBinaryBigEndian);
//		chkCompPict("", "9999V99",Type.ftBinaryBigEndianPositive);
		chkCompPict("", "9999V99",Type.ftPositiveBinaryBigEndian);

		chkCompPict("-3", "S9999V99",Type.ftPackedDecimal);
		chkCompPict("-3", "s9999v99",Type.ftPackedDecimal);
		chkCompPict("-3", "9999V99",Type.ftPackedDecimalPostive);

		chkCompPict("-5", "S9999V99",Type.ftBinaryBigEndian);
		chkCompPict("-5", "s9999v99",Type.ftBinaryBigEndian);
//		chkCompPict("-5", "9999V99",Type.ftBinaryBigEndianPositive);
		chkCompPict("-5", "9999V99",Type.ftPositiveBinaryBigEndian);
	}

	private void chkPict(String picture, int type) {
		assertEquals("Checking: " + picture, type, bc.getTypeIdentifier("", picture, false, false, ""));
	}


	private void chkCompPict(String compId, String picture, int type) {
		assertEquals("Checking: " + picture, type, bc.getTypeIdentifier(Cb2xmlConstants.COMP + compId, picture, false, false, ""));
	}
}
