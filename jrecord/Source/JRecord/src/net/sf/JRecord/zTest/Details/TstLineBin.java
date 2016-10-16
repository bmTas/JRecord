/*
 * @Author Bruce Martin
 * Created on 25/05/2005
 *
 * Purpose:
 */
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

package net.sf.JRecord.zTest.Details;



import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;

import junit.framework.TestCase;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 *
 *
 * @author Bruce Martin
 *
 */
public class TstLineBin extends TestCase {

	private CopybookLoader copybookInt = new CobolCopybookLoader();

    private static String copyBookName    = "Cbl_Line_Test_Record";
    private static String copyBookDTAR020 = "DTAR020";

    private  byte[] rec =
    		{  97, 115, 100, 102,  32,  32,  32,  32,  32,  32,  32
            ,  32,  32,  32, 113, 119, 101, 114, 116, 121,   0,   0
            ,   0,   1,  35,  64,  94,  78, -39,  22,-121,  43,   2
            ,  67, 104,  87, -49,  51,  52,  53,  54,  32,  32,  32
            ,  32,  32,  32,  32,  32,  32,  49,  50,  51,  48,  48
            ,  48,  48,  48,  52,  53,  54,  21,   3,   0,   0,  48
            ,  48,  48,  49,  50,  51,  52,  53,  48,  48,  32,  32
            ,  32,  32,  32,  50,  51,  46,  54,  55,   0,   0,   0
            ,  69, 103,  57,  48,   0,   0,  50,  51,  52,  46,  53
            ,  54,   0,   0,   1,  65,  23, -41,-124,   0, 127,  24
            , 121,   0, -54,-102,  59,   0,   0,-118,  93, 120,  69
            ,  99,   1, 121,  24,   1,  99,  69, 120,  93,-118,   0
            ,   0,   1,  35,  79,   1,  35,  15,  49,  75,  48,  48
            ,  49,  75,-128,-127,-125,   0,   0 };

    private  byte[] recDtar020 =
    { -10,  -7, -10,  -7, -12, -15, -11,  -8,   2,  12,   0,  64,  17,-116
        ,  40,  12,   0,   0,   0,   0,  28,   0,   0,   0,   0,  80,  28 };

    private static int xmlTotal = 0;

 //   private static LayoutDetail copyBook = null;

    private AbstractLine line;

    /**
     * @see TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();

        LayoutDetail copyBook = 
                copybookInt.loadCopyBook(
                        TstConstants.COBOL_DIRECTORY + copyBookName + ".cbl",
                        CopybookLoader.SPLIT_NONE, 0, "",
                        ICopybookDialects.FMT_MAINFRAME, 0, null
                ).asLayoutDetail();

        line = new Line(copyBook, rec);
    }

    /**
     * @see TestCase#tearDown()
     */
    protected void tearDown() throws Exception {
        super.tearDown();

        line = null;
    }


    /**
     * Check the Line.getField function (Text Field)
     */
    @SuppressWarnings("deprecation")
	public void testGetField() throws Exception {

        assertEquals(" 1 GetField - Character Field " + line.getField(0, 0 + xmlTotal),
                "asdf", line.getField(0, 0));
        assertEquals(" 2 GetField - Character Field ", "    qwerty", line.getField(0, 1 + xmlTotal));
        assertEquals(" 3 GetField - Decimal Field ", "123", line.getField(0, 2 + xmlTotal));
        assertEquals(" 7 GetField - Num Right Just", "123", line.getField(0, 6 + xmlTotal));
        assertEquals(" 8 GetField - Num Zero Padded Field ", "456", line.getField(0, 7));
        assertEquals("10 GetField - Assumed Decimal ", "123.4500", line.getField(0, 9));
        assertEquals("11 GetField - Num 2 Decimal ", "23.67", line.getField(0, 10));
        assertEquals("12 GetField - Decimal ", "45.67", line.getField(0, 11));
        //assertEquals("13 GetField - Positive Int ", "123.45", line.getField(0, 12));

        assertEquals("14 GetField - Num 2 Decimal Digits ", "234.56", line.getField(0, 13));
        //assertEquals("15 GetField - Mainframe Binary ", "321", line.getField(0, 14));
        //assertEquals("16 GetField - Mainframe Binary with Decimal", "4000000.00", line.getField(0, 15));
        //assertEquals("17 GetField - Byte ", "127", line.getField(0, 16));
        //assertEquals("18 GetField - Small Int ", "31000", line.getField(0, 17));
        //assertEquals("19 GetField - Int ", "1000000000", line.getField(0, 18));
        //assertEquals("20 GetField - Long ", "100000000000000000", line.getField(0, 19));
        assertEquals("23 GetField - Mainframe Packed ", "1234", line.getField(0, 22));
        assertEquals("24 GetField - Mainframe Packed with decimal", "123.0", line.getField(0, 23));

        assertEquals("25 GetField - Mainframe Zoned ", "-12", line.getField(0, 24));
        assertEquals("26 GetField - Mainframe Zoned with decimal", "-0.12", line.getField(0, 25));

        //assertEquals("27 GetField - Bit 1byte ", "10000000", line.getField(0, 26));
        //assertEquals("28 GetField - Bit 2byte ", "1000000110000011", line.getField(0, 27));

        LayoutDetail copyBook1 = 
                copybookInt.loadCopyBook(
                        TstConstants.COBOL_DIRECTORY + copyBookName + ".cbl",
                        CopybookLoader.SPLIT_NONE, 0, "",
                        ICopybookDialects.FMT_MAINFRAME, 0, null
                ).asLayoutDetail();

        AbstractLine line1 = new Line(copyBook1, rec);

        assertEquals("15 GetField - Mainframe Binary ", "321", line1.getField(0, 14));
        assertEquals("16 GetField - Mainframe Binary with Decimal", "4000000.00", line1.getField(0, 15));
        assertEquals("21 GetField - Mainframe Small Int ", "31000", line1.getField(0, 20));
        assertEquals("22 GetField - Mainframe Long ", "100000000000000000", line1.getField(0, 21));
    }


    /**
     * check Line.getFieldText
     */
    public void testGetFieldText() {
    	//"436857cf"
    	byte[] b = {0x43, 0x68, 0x57, (byte) 0xcf};
        assertEquals("GetFieldText - Character Field ", "asdf", line.getFieldText(0, 0));
        assertEquals("GetFieldText - Character Field ", "    qwerty", line.getFieldText(0, 1));

 //       assertEquals("GetFieldText - 4 >> ", "ChW�", line.getFieldText(0, 4));
 //       assertEquals("GetFieldText - 4 >> ", "ChW�", line.getFieldText(0, 4));
        assertEquals("GetFieldText - 4 >> ", new String(b), line.getFieldText(0, 4));
        assertEquals("GetFieldText - 5 >> ", "3456", line.getFieldText(0, 5));
        assertEquals("GetFieldText - 6 >> ", "     123", line.getFieldText(0, 6));
    }


    /**
     * Check GetFieldHex (retrieve a valuer as Hex)
     */
    public void testGetFieldHex() {

        assertEquals("GetFieldHex - GetHex", "15030000", line.getFieldHex(0,  8));
        assertEquals("GetFieldHex - GetHex", "39300000", line.getFieldHex(0, 12));

        System.out.println("\"" + line.getFieldHex(0, 3) + "\"");
        System.out.println("\"" + line.getFieldHex(0, 4) + "\"");
        assertEquals("GetFieldHex - GetHex 1 ", "20202020717765727479", line.getFieldHex(0, 1));
        assertEquals("GetFieldHex - GetHex 2 ", "0000000123", line.getFieldHex(0, 2));
        assertEquals("GetFieldHex - GetHex 3 ", "405e4ed916872b02", line.getFieldHex(0, 3));
        assertEquals("GetFieldHex - GetHex 4 ", "436857cf", line.getFieldHex(0, 4));
        assertEquals("GetFieldHex - GetHex 5 ", "3334353620202020", line.getFieldHex(0, 5));
        assertEquals("GetFieldHex - GetHex 6 ", "2020202020313233", line.getFieldHex(0, 6));
        assertEquals("GetFieldHex - GetHex 7 ", "3030303030343536", line.getFieldHex(0, 7));
        assertEquals("GetFieldHex - GetHex 9 ", "30303031323334353030", line.getFieldHex(0, 9));

        assertEquals("GetFieldHex - GetHex 11 ", "0000004567", line.getFieldHex(0, 11));
        assertEquals("GetFieldHex - GetHex 10 ", "202020202032332e3637", line.getFieldHex(0, 10));
        assertEquals("GetFieldHex - GetHex 13 ", "3233342e3536", line.getFieldHex(0, 13));
        assertEquals("GetFieldHex - GetHex 14 ", "00000141", line.getFieldHex(0, 14));

    }

    public void testGetFieldValueString() throws Exception {

        assertEquals(" 1 GetFieldValue - Character Field " + line.getFieldValue(0, 0).asString(),
                "asdf", line.getFieldValue(0, 0).asString());
        assertEquals(" 2 GetFieldValue - Character Field ", "    qwerty", line.getFieldValue(0, 1).asString());
        assertEquals(" 3 GetFieldValue - Decimal Field ", "123", line.getFieldValue(0, 2).asString());
        assertEquals(" 7 GetFieldValue - Num Right Just", "123", line.getFieldValue(0, 6).asString());
        assertEquals(" 8 GetFieldValue - Num Zero Padded Field ", "456", line.getFieldValue(0, 7).asString());
        assertEquals("10 GetFieldValue - Assumed Decimal ", "123.4500", line.getFieldValue(0, 9).asString());
        assertEquals("11 GetFieldValue - Num 2 Decimal ", "23.67", line.getFieldValue(0, 10).asString());
        assertEquals("12 GetFieldValue - Decimal ", "45.67", line.getFieldValue(0, 11).asString());
        //assertEquals("13 GetFieldValue - Positive Int ", "123.45", line.getFieldValue(0, 12).asString());

        assertEquals("14 GetFieldValue - Num 2 Decimal Digits ", "234.56", line.getFieldValue(0, 13).asString());
        //assertEquals("15 GetFieldValue - Mainframe Binary ", "321", line.getFieldValue(0, 14).asString());
        //assertEquals("16 GetFieldValue - Mainframe Binary with Decimal", "4000000.00", line.getFieldValue(0, 15).asString());
        //assertEquals("17 GetFieldValue - Byte ", "127", line.getFieldValue(0, 16));
        //assertEquals("18 GetFieldValue - Small Int ", "31000", line.getFieldValue(0, 17));
        //assertEquals("19 GetFieldValue - Int ", "1000000000", line.getFieldValue(0, 18));
        //assertEquals("20 GetFieldValue - Long ", "100000000000000000", line.getFieldValue(0, 19));
        assertEquals("23 GetFieldValue - Mainframe Packed ", "1234", line.getFieldValue(0, 22).asString());
        assertEquals("24 GetFieldValue - Mainframe Packed with decimal", "123.0", line.getFieldValue(0, 23).asString());

        assertEquals("25 GetFieldValue - Mainframe Zoned ", "-12", line.getFieldValue(0, 24).asString());
        assertEquals("26 GetFieldValue - Mainframe Zoned with decimal", "-0.12", line.getFieldValue(0, 25).asString());

        //assertEquals("27 GetFieldValue - Bit 1byte ", "10000000", line.getFieldValue(0, 26));
        //assertEquals("28 GetFieldValue - Bit 2byte ", "1000000110000011", line.getFieldValue(0, 27));

        LayoutDetail copyBook1 = 
                copybookInt.loadCopyBook(
                        TstConstants.COBOL_DIRECTORY + copyBookName + ".cbl",
                        CopybookLoader.SPLIT_NONE, 0, "",
                        ICopybookDialects.FMT_MAINFRAME, 0, null
                ).asLayoutDetail();

        AbstractLine line1 = new Line(copyBook1, rec);

        assertEquals("15 GetFieldValue - Mainframe Binary ", "321", line1.getFieldValue(0, 14).asString());
        assertEquals("16 GetFieldValue - Mainframe Binary with Decimal", "4000000.00", line1.getFieldValue(0, 15).asString());
        assertEquals("21 GetFieldValue - Mainframe Small Int ", "31000", line1.getFieldValue(0, 20).asString());
        assertEquals("22 GetFieldValue - Mainframe Long ", "100000000000000000", line1.getFieldValue(0, 21).asString());
    }



    public void testGetFieldValue() throws Exception {

//        assertEquals(" 1 GetFieldValue - Character Field " + line.getFieldValue(0, 0).asString(),
//                "asdf", line.getFieldValue(0, 0).asString());
//       assertEquals(" 2 GetFieldValue - Character Field ", "    qwerty", line.getFieldValue(0, 1).asString());
    	checkAllNums(" 3 GetFieldValue - Decimal Field ", "123", 2);
    	checkAllNums(" 7 GetFieldValue - Num Right Just", "123", 6);
    	checkAllNums(" 8 GetFieldValue - Num Zero Padded Field ", "456", 7);
    	checkDecimal(line, "10 GetFieldValue - Assumed Decimal ", "123.4500", 9);
        checkDecimal(line, "11 GetFieldValue - Num 2 Decimal ", "23.67", 10);
        checkDecimal(line, "12 GetFieldValue - Decimal ", "45.67", 11);
        //assertEquals("13 GetFieldValue - Positive Int ", "123.45", line.getFieldValue(0, 12));

        checkDecimal(line, "14 GetFieldValue - Num 2 Decimal Digits ", "234.56", 13);
        //assertEquals("15 GetFieldValue - Mainframe Binary ", "321", line.getFieldValue(0, 14));
        //assertEquals("16 GetFieldValue - Mainframe Binary with Decimal", "4000000.00", line.getFieldValue(0, 15));
        //assertEquals("17 GetFieldValue - Byte ", "127", line.getFieldValue(0, 16));
        //assertEquals("18 GetFieldValue - Small Int ", "31000", line.getFieldValue(0, 17));
        //assertEquals("19 GetFieldValue - Int ", "1000000000", line.getFieldValue(0, 18));
        //assertEquals("20 GetFieldValue - Long ", "100000000000000000", line.getFieldValue(0, 19));
        checkAllNums("23 GetFieldValue - Mainframe Packed ", "1234", 22);
        checkDecimal(line, "24 GetFieldValue - Mainframe Packed with decimal", "123.0", 23);


        checkAllNums("25 GetFieldValue - Mainframe Zoned ", "-12", 24);
        checkDecimal(line, "26 GetFieldValue - Mainframe Zoned with decimal", "-0.12",  25);

        //assertEquals("27 GetFieldValue - Bit 1byte ", "10000000", line.getFieldValue(0, 26));
        //assertEquals("28 GetFieldValue - Bit 2byte ", "1000000110000011", line.getFieldValue(0, 27));

        LayoutDetail copyBook1 = 
                copybookInt.loadCopyBook(
                        TstConstants.COBOL_DIRECTORY + copyBookName + ".cbl",
                        CopybookLoader.SPLIT_NONE, 0, "",
                        ICopybookDialects.FMT_MAINFRAME, 0, null
                ).asLayoutDetail();

        AbstractLine line1 = new Line(copyBook1, rec);

        checkAllNums(line1, "15 GetFieldValue - Mainframe Binary ", "321", 14);
        checkDecimal(line1, "16 GetFieldValue - Mainframe Binary with Decimal", "4000000.00", 15);
        checkAllNums(line1, "21 GetFieldValue - Mainframe Small Int ", "31000", 20);
        checkDecimal(line1, "22 GetFieldValue - Mainframe Long ",  "100000000000000000", 21);
        assertEquals("22 GetFieldValue - Mainframe Long  e) ",
        		new BigInteger("100000000000000000"), line1.getFieldValue(0, 21).asBigInteger());
     }

    private void checkAllNums(String id, String val, int fld) {

    	checkAllNums(line, id,  val, fld);
    }


    private void checkAllNums(AbstractLine pLine, String id, String val, int fld) {

    	if (Integer.parseInt(val) != pLine.getFieldValue(0, fld + xmlTotal).asInt()) {
//        	int i =  line.getFieldValue(0, fld).asInt();
        	System.out.println(">> " + fld + " " + Integer.parseInt(val)
    			+ " " + pLine.getFieldValue(0, fld + xmlTotal).asString()
    			+ " " + pLine.getFieldValue(0, fld + xmlTotal).asInt() + " " + pLine.getFieldValue(0, fld).asBigDecimal());
    	}
        assertEquals(id + " a) ", Integer.parseInt(val), pLine.getFieldValue(0, fld + xmlTotal).asInt());
        checkDecimal(pLine, id, val,  fld);
        assertEquals(id + " e) ", new BigInteger(val), pLine.getFieldValue(0, fld + xmlTotal).asBigInteger());
    }


    private void checkDecimal(AbstractLine pLine, String id, String val, int fld) {

        assertEquals(id + " b) ", Float.parseFloat(val), pLine.getFieldValue(0, fld + xmlTotal).asFloat(), 0.000001);
        assertEquals(id + " c) ", Double.parseDouble(val), pLine.getFieldValue(0, fld + xmlTotal).asDouble(), 0.000001);
        assertEquals(id + " d) ", new BigDecimal(val), pLine.getFieldValue(0, fld + xmlTotal).asBigDecimal());
    }



    /**
     * Test Line.setField
     *
     * @throws RecordException conversion error (should not occur)
     */
    public void testSetField() throws RecordException {

        checkAssignment(" 3 setField - Decimal Field ",   2, "456");
        checkAssignment(" 4 setField - Double Field ",    3, "989.878");
        //checkAssignment(" 5 setField - Float  Field ",    4, "898.343");
        checkAssignmentText(" 7 setField - Num Right Just",   6, "876", "     876");
        checkAssignmentText(" 8 setField - Num Zero Padded Field ", 7, "876", "00000876");
        //checkAssignmentHex(" 9 setField - Postive Int ",     8, "987", "db030000");
        checkAssignmentText("10 setField - Assumed Decimal ", 9, "767.4500", "0007674500");
        checkAssignmentText("10a setField - Assumed Decimal ",9, "0.0127", "0000000127");
        checkAssignmentText("11 setField - Num 2 Decimal ",  10, "45.67", "     45.67");
        checkAssignmentText("11a setField - Num 2 Decimal ", 10, "0.67", "      0.67");
        //checkAssignmentHex("12 setField - Decimal ",        11, "67.67", "0000006767");
        //checkAssignmentHex("12a setField - Decimal ",       11, "0.67", "0000000067");
        //checkAssignmentHex("13 setField - Positive Int ",   12, "765.45", "012b0100");
        //checkAssignmentHex("13a setField - Positive Int ",  12, "0.45", "2d000000");
        //checkAssignmentHex("13 setField - Positive Int ",   12, "20000000.00", "00943577");

        checkAssignmentText("14 setField - Num 2 Decimal Digits ", 13, "543.56", "543.56");
        checkAssignmentText("14a setField - Num 2 Decimal Digits", 13, "0.56", "0.56");
        //checkAssignmentHex("15 setField - Mainframe Binary ",     14, "432", "000001b0");

        //checkAssignmentHex("16 setField - Mainframe Binary with Decimal", 15, "4000000.00", "17d78400");
        //checkAssignmentHex("17 setField - Byte ", 16, "126", "7e");
        //checkAssignmentHex("18 setField - Small Int ", 17, "31000", "1879");
        //checkAssignmentHex("19 setField - Int ", 18, "1000000000", "00ca9a3b");
        //checkAssignmentHex("20 setField - Long ", 19, "1000000000000000000", "000064a7b3b6e00d");
        //checkAssignmentHex("20a setField - Long ", 19, "4000000000000000000", "0000909dceda8237");

        checkAssignmentHex("23 setField - Mainframe Packed ", 22, "23451", "23451c");
        checkAssignmentHex("23a setField - Mainframe Packed ", 22, "1", "00001c");
        checkAssignmentHex("24 setField - Mainframe Packed with decimal", 23, "1234.0", "12340c");
        checkAssignmentHex("24a setField - Mainframe Packed with decimal", 23, "0.1", "00001c");

        checkAssignmentText("25 setField - Mainframe Zoned ", 24, "23", "23");
        checkAssignmentText("26 setField - Mainframe Zoned with decimal", 25, "23.45", "2345");

        //checkAssignmentHex("27 setField - Bit 1byte ", 26, "10010010", "92");
        //checkAssignmentHex("28 setField - Bit 2byte ", 27, "1000100101010101", "8955");

        //checkAssignmentHex("27a setField - Bit 1byte ", 26, "10010", "12");
        //checkAssignmentHex("28a setField - Bit 2byte ", 27, "100101010101", "0955");


        checkAssignment(" 4n setField - Double Field ",    3, "-989.878");
        //checkAssignment(" 5n setField - Float  Field ",    4, "-898.343");
        checkAssignmentText(" 6n setField - Num  Field ",      5, "-7475", "-7475");
        checkAssignmentText(" 7n setField - Num Right Just",   6, "-876", "    -876");
       // checkAssignmentText(" 8n setField - Num Zero Padded Field ", 7, "-876", "-0000876");
        checkAssignError("Assign negaqtive number to positive field", 7, "-876");

        checkAssignmentText("10n setField - Assumed Decimal ", 9, "-67.4500", "00067450}");
        checkAssignmentText("11n setField - Num 2 Decimal ",  10, "-45.67", "    -45.67");
        //checkAssignment("12n setField - Decimal ",        11, "-67.67");

        checkAssignmentText("14n setField - Num 2 Decimal Digits ", 13, "-43.56", "-43.56");
        //checkAssignmentHex("17n setField - Byte ", 16, "-126", "82");
        //checkAssignmentHex("18n setField - Small Int ", 17, "-31000", "e886");
        //checkAssignmentHex("19n setField - Int ", 18, "-1000000000", "003665c4");
        //checkAssignmentHex("20n setField - Long ", 19, "-1000000000000000000", "00009c584c491ff2");
        checkAssignmentHex("23n setField - Mainframe Packed ", 22, "-23451", "23451d");
        checkAssignmentHex("24n setField - Mainframe Packed with decimal", 23, "-1234.0", "12340d");
        checkAssignmentHex("24n2 setField - Mainframe Packed with decimal", 23, "-0.1", "00001d");

        checkAssignmentText("25n1 setField - Mainframe Zoned ", 24, "-23", "2L");
        checkAssignmentText("26n1 setField - Mainframe Zoned with decimal", 25, "-23.45", "234N");

        checkAssignment(" 4n1 setField - Double Field ",    3, "-989.878");
        //checkAssignment(" 5n1 setField - Float  Field ",    4, "-898.343");
        checkAssignment(" 6n1 setField - Num  Field ",      5, "-7475");
        checkAssignment(" 7n1 setField - Num Right Just",   6, "-876");
//        checkAssignment(" 8n1 setField - Num Zero Padded Field ", 7, "-876");
        checkAssignError("Assign negative number to positive field", 7, "-876");

        checkAssignmentText("10n1 setField - Assumed Decimal ", 9, "-0.0127", "00000012P");
        checkAssignmentText("11n1 setField - Num 2 Decimal ",  10, "-1.27", "     -1.27");
        //checkAssignment("12n setField - Decimal ",        11, "-67.67");

        checkAssignment("14n1 setField - Num 2 Decimal Digits ", 13, "-1.26");

        checkAssignmentHex("23n2 setField - Mainframe Packed ", 22, "-1", "00001d");
        checkAssignmentHex("24n2 setField - Mainframe Packed with decimal", 23, "-1.0", "00010d");

        checkAssignmentText("25n2 setField - Mainframe Zoned ", 24, "-3", "0L");
        checkAssignmentText("26n2 setField - Mainframe Zoned with decimal", 25, "-0.45", "004N");
    }

    public void testSetFieldMainframe() throws Exception {
        LayoutDetail copyBook1 = 
                copybookInt.loadCopyBook(
                        TstConstants.COBOL_DIRECTORY + copyBookName + ".cbl",
                        CopybookLoader.SPLIT_NONE, 0, "",
                        ICopybookDialects.FMT_MAINFRAME, 0, null
                ).asLayoutDetail();

        line = new Line(copyBook1, rec);

        checkAssignmentHex("15n setField - Mainframe Binary ",     14, "-432", "fffffe50");

        checkAssignmentHex("16n setField - Mainframe Binary with Decimal", 15, "-4000000.00", "e8287c00");

        checkAssignmentHex("21 setField - Mainframe Small Int ", 20, "31000", "7918");
        checkAssignmentHex("22 setField - Mainframe Long ", 21, "1000000000000000000", "0de0b6b3a7640000");
        checkAssignmentHex("22a setField - Mainframe Long ", 21, "4000000000000000000", "3782dace9d900000");
        checkAssignmentHex("23 setField - Mainframe Packed ", 22, "23451", "23451c");
        checkAssignmentHex("23a setField - Mainframe Packed ", 22, "1", "00001c");
        checkAssignmentHex("24 setField - Mainframe Packed with decimal", 23, "1234.0", "12340c");
        checkAssignmentHex("24a setField - Mainframe Packed with decimal", 23, "0.1", "00001c");


        checkAssignmentText("25 setField - Mainframe Zoned ", 24, "23", "23");
        checkAssignmentText("26 setField - Mainframe Zoned with decimal", 25, "23.45", "2345");

        checkAssignmentHex("21n setField - Mainframe Small Int ", 20, "-31000", "86e8");
        checkAssignmentHex("22n setField - Mainframe Long ", 21, "-1000000000000000000", "f21f494c589c0000");
        checkAssignmentHex("23n setField - Mainframe Packed ", 22, "-23451", "23451d");
        checkAssignmentHex("24n setField - Mainframe Packed with decimal", 23, "-1234.0", "12340d");
        checkAssignmentHex("24n2 setField - Mainframe Packed with decimal", 23, "-0.1", "00001d");

        checkAssignmentHex("15n1 setField - Mainframe Binary ",     14, "-127", "ffffff81");

        checkAssignmentHex("16n2 setField - Mainframe Binary with Decimal", 15, "-1.27", "ffffff81");
        //checkAssignmentHex("17n2 setField - Byte ", 16, "-126", "82");
        //checkAssignmentHex("18n2 setField - Small Int ", 17, "-127", "81ff");
        //checkAssignmentHex("19n2 setField - Int ", 18, "-127", "81ffffff");
        //checkAssignmentHex("20n2 setField - Long ", 19, "-127", "81ffffffffffffff");
        checkAssignmentHex("21n2 setField - Mainframe Small Int ", 20, "-31000", "86e8");
        checkAssignmentHex("22n2 setField - Mainframe Long ", 21, "-127", "ffffffffffffff81");
}



    /**
     * Checks assignment to a numeric field
     *
     * @param msg Error Message
     * @param fldNum Field Number to assign a value to
     * @param val value to assign
     *
     * @throws RecordException any conversion error
     */
    @SuppressWarnings("deprecation")
	private void checkAssignment(String msg, int fldNum,  String val)
    					throws RecordException {

        String s;
        line.setField(0, fldNum, val);
        s = line.getField(0, fldNum).toString();
        assertEquals(msg + " " + s + " <> " + val, val, s);
    }


    /**
     * Checks assignment to a numeric field
     *
     * @param msg Error Message
     * @param fldNum Field Number to assign a value to
     * @param val value to assign
     * @param hexVal expected hex value
     *
     * @throws RecordException any conversion error
     */
    @SuppressWarnings("deprecation")
	private void checkAssignmentHex(String msg, int fldNum,  String val, String hexVal)
    					throws RecordException {

        String s;
        line.setField(0, fldNum, val);

        s = line.getFieldHex(0, fldNum);

        assertEquals(msg + " hex check " + s + " <> " + hexVal, hexVal, s);
/*        if (!s.equals(hexVal)) {
            System.out.println("==> " + msg + " " + fldNum + " " + val
                + " " + line.getField(0, fldNum) + " " + s + " " + hexVal);
        }*/

        s = line.getField(0, fldNum).toString();
        assertEquals(msg + " " + s + " <> " + val, val, s);
    }


    /**
     * Checks assignment to a fields
     *
     * @param msg Error Message
     * @param fldNum Field Number to assign a value to
     * @param val value to assign
     * @param text expected hex value
     *
     * @throws RecordException any conversion error
       */
    private void checkAssignmentText(String msg, int fldNum,  String val, String text)
	throws RecordException {

        String s;
        line.setField(0, fldNum, val);

        s = line.getFieldText(0, fldNum);

        //assertEquals(msg + " hex check " + s + " <> " + text, text, s);
        if (!s.equals(text)) {
            System.out.println("==> " + msg + " " + fldNum + " " + val
                + " " + line.getField(0, fldNum) + " " + s + " " + text);
         }

        s = line.getField(0, fldNum).toString();
        assertEquals(msg + " " + s + " <> " + val, val, s);
    }




    /**
     * Test Line.setFieldConversion
     */
    public void testSetFieldConversion() {

        checkConversionError(" 2 setField - Decimal Field ",   2);
        //checkConversionError(" 3 setField - Double Field ",    3);
        //checkConversionError(" 4 setField - Float  Field ",    4);
        //checkConversionError(" 5 setField - Num  Field ",      5);
        checkConversionError(" 5 setField - Num Right Just",   6);
        checkConversionError(" 7 setField - Num Zero Padded Field ", 7);
        checkConversionError(" 8 setField - Postive Int ",     8);
        checkConversionError(" 9 setField - Assumed Decimal ", 9);
        checkConversionError("10 setField - Num 2 Decimal ",  10);
        checkConversionError("11 setField - Decimal ",        11);
        checkConversionError("12 setField - Positive Int ",   12);
        //checkSizeError("12 setField - Positive Int ~ assign negative ",
        //        12, "-5");

        //checkConversionError("13 setField - Num 2 Decimal Digits ", 13);
        checkConversionError("14 setField - Mainframe Binary ",     14);
        //checkConversionError("14 setField - Num 2 Decimal Digits ", 13);
        //checkConversionError("14a setField - Num 2 Decimal Digits", 13);
        checkConversionError("15 setField - Mainframe Binary ",     14);

        checkConversionError("16 setField - Mainframe Binary with Decimal", 15);
        //checkConversionError("17 setField - Byte ", 16);
        //checkConversionError("18 setField - Small Int ", 17);
        //checkConversionError("19 setField - Int ", 18);
        //checkConversionError("20 setField - Long ", 19);
        checkConversionError("21 setField - Mainframe Small Int ", 20);
        checkConversionError("22 setField - Mainframe Long ", 21);

        checkConversionError("23 setField - Mainframe Packed ", 22);
        checkConversionError("23a setField - Mainframe Packed ", 22);
        checkConversionError("24 setField - Mainframe Packed with decimal", 23);

        checkConversionError("25n1 setField - Mainframe Zoned ", 24);
        checkConversionError("26n1 setField - Mainframe Zoned with decimal", 25);

//        checkConversionError("27 setField - Bit 1byte ", 26);
//        checkConversionError("28 setField - Bit 2byte ", 27);
//
//        checkConversionError("27a setField - Bit 1byte ", 26, "200010010");
//        checkConversionError("28a setField - Bit 2byte ", 27, "20000100101010101");

    }

    /**
     * Test Line.setFieldConversion
     */
    public void testSetFieldSizeError() {

        checkSizeError(" 2 setField - Decimal Field ",   2, "12345678901");
        //checkSizeError(" 3 setField - Double Field ",    3, "989.878");
        //checkSizeError(" 4 setField - Float  Field ",    4, "898.343");
        //checkSizeError(" 5 setField - Num  Field ",      5, "123456789");
        checkSizeError(" 5 setField - Num Right Just",   6, "123456789");
        checkSizeError(" 7 setField - Num Zero Padded Field ", 7, "123456789");
        checkSizeError(" 8 setField - Postive Int ",     8, "5000000000");
        checkSizeError(" 9 setField - Assumed Decimal ", 9, "7654321");
        checkSizeError("10 setField - Num 2 Decimal ",  10, "987654321");
        checkSizeError("11 setField - Decimal ",        11, "987654321");
        checkSizeError("12 setField - Positive Int ",   12, "50000000");

        //checkSizeError("13 setField - Num 2 Decimal Digits ", 13, "54356");
        checkSizeError("14 setField - Mainframe Binary ",     14, "5000000000");


        checkSizeError("16 setField - Mainframe Binary with Decimal", 15, "5000000000");
        //checkSizeError("17 setField - Byte ", 16, "128");
        //checkSizeError("18 setField - Small Int ", 17, "33000");
        //checkSizeError("19 setField - Int ", 18, "3000000000");
        //checkSizeError("20 setField - Long ", 19, "80000000000000000000");
        checkSizeError("21 setField - Mainframe Small Int ", 20, "33000");
        checkSizeError("22 setField - Mainframe Long ", 21, "80000000000000000000");

        checkSizeError("23 setField - Mainframe Packed ", 22, "234561");
        checkSizeError("24 setField - Mainframe Packed with decimal", 23, "12345.0");

        checkSizeError("25 setField - Mainframe Zoned ", 24, "123");
        checkSizeError("26 setField - Mainframe Zoned with decimal", 25, "123");
        checkSizeError("26a setField - Mainframe Zoned with decimal", 25, "123.12");

        //checkSizeError("27 setField - Bit 1byte ", 26, "1100010010");
        //checkSizeError("28 setField - Bit 2byte ", 27, "110000100101010101");
    }



    /**
     * Test setting a field with a value that is to long
     *
     * @param fldNum field to use
     * @param value value to assign to the supplied field
     * @param msg error message to use if size error is not thrown
     */
    @SuppressWarnings("deprecation")
	private void checkSizeError(String msg, int fldNum, String value) {

        try {
            line.setField(0, fldNum, value);

            System.out.println("::> " + msg + " " + value + " :: " + line.getField(0, fldNum));
            throw new AssertionError("Size Error: " + msg);
        } catch (RecordException e) {
        }
    }


    /**
     * Test setting a numeric field that is not numeric
     *
     * @param fld2set field to use
     * @param msg error message to use if size error is not thrown
     */
    private void checkConversionError(String msg, int fld2set) {
        checkConversionError(msg, fld2set, "a");
    }


    /**
     * Test setting a field to an invalid value
     *
     * @param fld2set field to use
     * @param msg error message to use if size error is not thrown
     * @param value value to test
     */
    private void checkConversionError(String msg, int fld2set, String value) {
        try {
            line.setField(0, fld2set, value);

            throw new AssertionError("Conversion Error: " + msg);
        } catch (RecordException e) {
        } catch (NumberFormatException e) {
        }
    }

    private void checkAssignError(String msg, int fld2set, String value) {
        try {
            line.setField(0, fld2set, value);

        } catch (RecordException e) {
        	return;
        } catch (Exception e) {
        }
        throw new AssertionError("Check Assign Error: " + msg);
    }

    /**
     * test get record
     *
     */
    public void testGetRecord() {
        try {
        checkAssignment(" 3 setField - Decimal Field ",   2, "456");
        } catch (Exception e) {
        }
        assertTrue("getRecord Error ", Arrays.equals(rec, line.getData()));
    }

    /**
     * Check retrieving Mainframe Field etc
     *
     */
    public void testMainframe() throws Exception {
        AbstractLine mainframeLine = defMainframeRec();
        String expected = "69694158";
        String s = mainframeLine.getField(0, 0).toString();
        byte[] r;

        assertEquals("Testing Mainframe getField " + expected + " <> " + s,
                expected, s);

        try {
            mainframeLine.setField(0, 0, "1");
            mainframeLine.setField(0, 0, s);
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }

        r = mainframeLine.getData();
        System.out.println("Mainframe rec Lengths=> " + recDtar020.length
                		 + " " + r.length);
        assertTrue("getRecord Error ", Arrays.equals(recDtar020, r));
    }


    /**
     * creates a Mainframe record
     *
     * @return Mainframe Line
     */
    private AbstractLine defMainframeRec() throws Exception {
        LayoutDetail dtar0020 = 
                copybookInt.loadCopyBook(
                        TstConstants.COBOL_DIRECTORY + copyBookDTAR020 + ".cbl",
                        CopybookLoader.SPLIT_NONE, 0, "cp037",
                        ICopybookDialects.FMT_MAINFRAME, 0, null
                ).asLayoutDetail();
        return new Line(dtar0020,
                		recDtar020);
    }


}
