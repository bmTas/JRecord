/*
 * @Author Bruce Martin
 * Created on 25/05/2005
 *
 * Purpose:
 */
package net.sf.JRecord.zTest.Types;


import junit.framework.TestCase;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;

/**
 *
 *
 * @author Bruce Martin
 *
 */
public class TstBinType extends TestCase {


    private  byte[] rec1 =
		    { 97, 115, 100, 102, 32, 32, 32, 32, 32, 32, 32
		    	, 32, 32, 32, 113, 119, 101, 114, 116, 121, 0, 0
		    	, 0, 1, 35, 2, 43, -121, 22, -39, 78, 94, 64
		    	, -49, 87, 104, 67, 51, 52, 53, 54, 32, 32, 32
		    	, 32, 32, 32, 32, 32, 32, 49, 50, 51, 48, 48
		    	, 48, 48, 48, 52, 53, 54, 21, 3, 0, 0, 48
		    	, 48, 48, 49, 50, 51, 52, 53, 48, 48, 32, 32
		    	, 32, 32, 32, 50, 51, 46, 54, 55, 0, 0, 0
		    	, 69, 103, 57, 48, 0, 0, 50, 51, 52, 46, 53
		    	, 54, 0, 0, 1, 65, 23, -41, -124, 0, 127, 24
		    	, 121, 0, -54, -102, 59, 0, 0, -118, 93, 120, 69
		    	, 99, 1, 121, 24, 1, 99, 69, 120, 93, -118, 0
		    	, 0, 1, 35, 79, 1, 35, 15, 49, 75, 48, 48
		    	, 49, 75, -128, -127, -125, 0, 0};
    	
//    		{  97, 115, 100, 102,  32,  32,  32,  32,  32,  32,  32
//            ,  32,  32,  32, 113, 119, 101, 114, 116, 121,   0,   0
//            ,   0,   1,  35,  64,  94,  78, -39,  22,-121,  43,   2
//            ,  67, 104,  87, -49,  51,  52,  53,  54,  32,  32,  32
//            ,  32,  32,  32,  32,  32,  32,  49,  50,  51,  48,  48
//            ,  48,  48,  48,  52,  53,  54,  21,   3,   0,   0,  48
//            ,  48,  48,  49,  50,  51,  52,  53,  48,  48,  32,  32
//            ,  32,  32,  32,  50,  51,  46,  54,  55,   0,   0,   0
//            ,  69, 103,  57,  48,   0,   0,  50,  51,  52,  46,  53
//            ,  54,   0,   0,   1,  65,  23, -41,-124,   0, 127,  24
//            , 121,   0, -54,-102,  59,   0,   0,-118,  93, 120,  69
//            ,  99,   1, 121,  24,   1,  99,  69, 120,  93,-118,   0
//            ,   0,   1,  35,  79,   1,  35,  15,  49,  75,  48,  48
//            ,  49,  75,-128,-127,-125,   0,   0 };

    private static byte[] rec2 =
	    	{ 97, 115, 100, 102, 32, 32, 32, 32, 32, 32, 32
	    	, 32, 32, 32, 113, 119, 101, 114, 116, 121, 0, 0
	    	, 0, 2, 51, 2, 43, -121, 22, -39, 78, 94, -64
	    	, -49, 87, 104, -61, 45, 51, 52, 53, 54, 32, 32
	    	, 32, 32, 32, 32, 32, 45, 49, 50, 51, 45, 48
	    	, 48, 48, 48, 52, 53, 54, -110, -13, 1, 0, 45
	    	, 48, 48, 49, 50, 51, 52, 53, 48, 48, 32, 32
	    	, 32, 32, 45, 50, 51, 46, 54, 55, 1, 35, 69
	    	, 103, 0, -103, 72, 35, 0, 45, 51, 52, 46, 53
	    	, 54, -1, -1, 43, -49, -24, 40, 124, 0, -128, -24
	    	, -122, 0, 54, 101, -60, 0, 0, 118, -94, -121, -70
	    	, -100, -2, -122, -24, -2, -100, -70, -121, -94, 118, 0
	    	, 0, 18, 52, 93, 18, 52, 13, 49, 50, 49, 50
	    	, 49, 50, -64, -63, -125, 0, 0, 13, 10};    	
    	
//    	{  97, 115, 100, 102,  32,  32,  32,  32,  32,  32,  32
//        ,  32,  32,  32, 113, 119, 101, 114, 116, 121,   0,   0
//        ,   0,   2,  51, -64,  94,  78, -39,  22,-121,  43,   2
//        , -61, 104,  87, -49,  45,  51,  52,  53,  54,  32,  32
//        ,  32,  32,  32,  32,  32,  45,  49,  50,  51,  45,  48
//        ,  48,  48,  48,  52,  53,  54,-110, -13,   1,   0,  45
//        ,  48,  48,  49,  50,  51,  52,  53,  48,  48,  32,  32
//        ,  32,  32,  45,  50,  51,  46,  54,  55,   1,  35,  69
//        , 103,   0,-103,  72,  35,   0,  45,  51,  52,  46,  53
//        ,  54,  -1,  -1,  43, -49, -24,  40, 124,   0,-128, -24
//        ,-122,   0,  54, 101, -60,   0,   0, 118, -94,-121, -70
//        ,-100,  -2,-122, -24,  -2,-100, -70,-121, -94, 118,   0
//        ,   0,  18,  52,  93,  18,  52,  13,  49,  50,  49,  50
//        ,  49,  50, -64, -63,-125,   0,   0 ,  13,  10 };

    private byte[] rec = rec1;


    private static byte[] recDtar020 =
    { -10,  -7, -10,  -7, -12, -15, -11,  -8,   2,  12,   0,  64,  17,-116
        ,  40,  12,   0,   0,   0,   0,  28,   0,   0,   0,   0,  80,  28 };



    private TypeManager typeManager = new TypeManager();

    private FieldDetail fldChar, fldCharRightJust, fldDecimal, fldDouble,
            fldFloat, fldNum, fldNumRightJust, fldNumZeroPadded, fldPostiveInt,
            fldAssummedDecimal, fldNum2decimal, fldDecimal2digits,
            fldPositiveInt2digit, fldNum2decimal1, fldMainframeInt,
            fldMainframeInt2decimal, fldByte, fldSmallInt, fldInt, fldLong,
            fldMainframeSmallInt, fldMainframeLong, fldMainframePackedDecimal,
            fldMainframePackedwithDecimal, fldZoned, fldZonedWithDecimal,
            fldBit1, fldBit2;


    /**
     * @see TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();

        fldChar				    = getType(1,   10, 0, 0);
        fldCharRightJust		= getType(11,  10, 1, 0);
        fldDecimal				= getType(21,  5, 11, 0);
        fldDouble				= getType(26,  8, 18, 0);
        fldFloat				= getType(34,  4, 17, 0);
        fldNum					= getType(38,  8,  5, 0);
        fldNumRightJust			= getType(46,  8,  6, 0);
        fldNumZeroPadded		= getType(54,  8,  7, 0);
        fldPostiveInt			= getType(62,  4, 16, 0);
        fldAssummedDecimal		= getType(66,  10, 8, 4);
        fldNum2decimal			= getType(76,  10, 6, 2);
        fldDecimal2digits		= getType(86,  5, 11, 2);
        fldPositiveInt2digit	= getType(91,  4, 16, 2);
        fldNum2decimal1			= getType(95,  6,  5, 2);
        fldMainframeInt			= getType(101, 4, 35, 0);
        fldMainframeInt2decimal	= getType(105, 4, 35, 2);
        fldByte					= getType(109, 1, 15, 0);
        fldSmallInt				= getType(110, 2, 15, 0);
        fldInt					= getType(112, 4, 15, 0);
        fldLong					= getType(116, 8, 15, 0);
        fldMainframeSmallInt	= getType(124, 2, 35, 0);
        fldMainframeLong		= getType(126, 8, 35, 0);
        fldMainframePackedDecimal	= getType(134, 3, 31, 0);
        fldMainframePackedwithDecimal	= getType(137, 3, 31, 1);
        fldZoned				= getType(140, 2, 32, 0);
        fldZonedWithDecimal		= getType(142, 4, 32, 2);
        fldBit1					= getType(146, 1, 21, 0);
        fldBit2					= getType(147, 2, 21, 0);

        //    copyBook = copybookInt.getGroup(copyBookName);

        //line = new Line(copyBook, rec);


    }

    /**
     * @see TestCase#tearDown()
     */
    protected void tearDown() throws Exception {
        super.tearDown();

        //line = null;
    }


    private FieldDetail getType(int pos, int len, int type, int decimal) {

        FieldDetail field = new FieldDetail("", "", type, decimal, "", -1, "");

        field.setPosLen(pos, len);

        return field;
    }



    private FieldDetail getTypeMainframe(int pos, int len, int type, int decimal) {

        FieldDetail field = new FieldDetail("", "", type, decimal, "CP037", -1, "");

        field.setPosLen(pos, len);

        return field;
    }


    /**
     * Check the Line.getField function (Text Field)
     */
    public void testGetValue() {

        assertEquals(" 1 GetValue - Character Field ", "asdf", getFldValue(fldChar));
        assertEquals(" 2 GetValue - Character Field ", "    qwerty", getFldValue(fldCharRightJust));
        assertEquals(" 3 GetValue - Decimal Field ", "123", getFldValue(fldDecimal));
        assertEquals(" 4 GetValue - Double Field ", "121.232", getFldValue(fldDouble));
        assertEquals(" 5 GetValue - Float  Field ", "232.343", getFldValue(fldFloat));
        assertEquals(" 6 GetValue - Num  Field ", "3456", getFldValue(fldNum));
        assertEquals(" 7 GetValue - Num Right Just", "123", getFldValue(fldNumRightJust));
        assertEquals(" 8 GetValue - Num Zero Padded Field ", "456", getFldValue(fldNumZeroPadded));
        assertEquals(" 9 GetValue - Postive Int ", "789", getFldValue(fldPostiveInt));
        assertEquals("10 GetValue - Assumed Decimal ", "123.4500", getFldValue(fldAssummedDecimal));
        assertEquals("11 GetValue - Num 2 Decimal ", "23.67", getFldValue(fldNum2decimal));
        assertEquals("12 GetValue - Decimal ", "45.67", getFldValue(fldDecimal2digits));
        assertEquals("13 GetValue - Positive Int ", "123.45", getFldValue(fldPositiveInt2digit));

        assertEquals("14 GetValue - Num 2 Decimal Digits ", "234.56", getFldValue(fldNum2decimal1));
        assertEquals("15 GetValue - Mainframe Binary ", "321", getFldValue(fldMainframeInt));
        assertEquals("16 GetValue - Mainframe Binary with Decimal",
                "4000000.00", getFldValue(fldMainframeInt2decimal));
        assertEquals("17 GetValue - Byte ", "127", getFldValue(fldByte));
        assertEquals("18 GetValue - Small Int ", "31000", getFldValue(fldSmallInt));
        assertEquals("19 GetValue - Int ", "1000000000", getFldValue(fldInt));
        assertEquals("20 GetValue - Long ", "100000000000000000", getFldValue(fldLong));
        assertEquals("21 GetValue - Mainframe Small Int ", "31000",
                getFldValue(fldMainframeSmallInt));
        assertEquals("22 GetValue - Mainframe Long ",
                "100000000000000000", getFldValue(fldMainframeLong));
        assertEquals("23 GetValue - Mainframe Packed ", "1234",
                getFldValue(fldMainframePackedDecimal));
        assertEquals("24 GetValue - Mainframe Packed with decimal",
                "123.0", getFldValue(fldMainframePackedwithDecimal));

        assertEquals("25 GetValue - Mainframe Zoned ", "-12", getFldValue(fldZoned));
        assertEquals("26 GetValue - Mainframe Zoned with decimal",
                "-0.12", getFldValue(fldZonedWithDecimal));

        assertEquals("27 GetValue - Bit 1byte ", "10000000", getFldValue(fldBit1));
        assertEquals("28 GetValue - Bit 2byte ", "1000000110000011", getFldValue(fldBit2));
    }


    /**
     * Check the Line.getField function (Text Field)
     */
    public void testGetValue1() {

        rec = rec2;

        assertEquals(" 1 GetValue - Character Field ", "asdf", getFldValue(fldChar));
        assertEquals(" 2 GetValue - Character Field ", "    qwerty", getFldValue(fldCharRightJust));
        assertEquals(" 3 GetValue - Decimal Field ", "233", getFldValue(fldDecimal));
        assertEquals(" 4 GetValue - Double Field ", "-121.232", getFldValue(fldDouble));
        assertEquals(" 5 GetValue - Float  Field ", "-232.343", getFldValue(fldFloat));
        assertEquals(" 6 GetValue - Num  Field ", "-3456", getFldValue(fldNum));
        assertEquals(" 7 GetValue - Num Right Just", "-123", getFldValue(fldNumRightJust));
        assertEquals(" 8 GetValue - Num Zero Padded Field ", "-456", getFldValue(fldNumZeroPadded));
        assertEquals(" 9 GetValue - Postive Int ", "127890", getFldValue(fldPostiveInt));
        assertEquals("10 GetValue - Assumed Decimal ", "-123.4500",
                getFldValue(fldAssummedDecimal));
        assertEquals("11 GetValue - Num 2 Decimal ", "-23.67", getFldValue(fldNum2decimal));
        assertEquals("12 GetValue - Decimal ", "1234567.00", getFldValue(fldDecimal2digits));
        assertEquals("13 GetValue - Positive Int ", "23123.45", getFldValue(fldPositiveInt2digit));

        assertEquals("14 GetValue - Num 2 Decimal Digits ", "-34.56", getFldValue(fldNum2decimal1));
        assertEquals("15 GetValue - Mainframe Binary ", "-54321", getFldValue(fldMainframeInt));
        assertEquals("16 GetValue - Mainframe Binary with Decimal",
                "-4000000.00", getFldValue(fldMainframeInt2decimal));
        assertEquals("17 GetValue - Byte ", "-128", getFldValue(fldByte));
        assertEquals("18 GetValue - Small Int ", "-31000", getFldValue(fldSmallInt));
        assertEquals("19 GetValue - Int ", "-1000000000", getFldValue(fldInt));
        assertEquals("20 GetValue - Long ", "-100000000000000000", getFldValue(fldLong));
        assertEquals("21 GetValue - Mainframe Small Int ",
                "-31000", getFldValue(fldMainframeSmallInt));
        assertEquals("22 GetValue - Mainframe Long ",
                "-100000000000000000", getFldValue(fldMainframeLong));
        assertEquals("23 GetValue - Mainframe Packed ", "-12345",
                getFldValue(fldMainframePackedDecimal));
        assertEquals("24 GetValue - Mainframe Packed with decimal",
                "-1234.0", getFldValue(fldMainframePackedwithDecimal));

        assertEquals("25 GetValue - Mainframe Zoned ", "12", getFldValue(fldZoned));
        assertEquals("26 GetValue - Mainframe Zoned with decimal",
                "12.12", getFldValue(fldZonedWithDecimal));

        assertEquals("27 GetValue - Bit 1byte ", "11000000", getFldValue(fldBit1));
        assertEquals("28 GetValue - Bit 2byte ", "1100000110000011", getFldValue(fldBit2));
    }


    /**
     * Get field value
     * @param fld field definition
     * @return value of the field
     */
    private String getFldValue(FieldDetail fld) {
        return typeManager.getType(fld.getType()).getField(rec, fld.getPos(), fld).toString();
    }






    /**
     * Test Line.setField
     *
     * @throws RecordException conversion error (should not occur)
     */
    public void testSetField() throws RecordException {

        checkAssignment(" 3 setField - Decimal Field ",   fldDecimal, "456");
        checkAssignment(" 4 setField - Double Field ",    fldDouble, "989.878");
        checkAssignment(" 5 setField - Float  Field ",    fldFloat, "898.343");
        checkAssignmentText(" 6 setField - Num  Field ",      fldNum, "7475", "7475");
        checkAssignmentText(" 7 setField - Num Right Just",   fldNumRightJust, "876", "     876");
        checkAssignmentText(" 8 setField - Num Zero Padded Field ", fldNumZeroPadded,
                "876", "00000876");
        checkAssignmentHex(" 9 setField - Postive Int ",       fldPostiveInt, "987", "db030000");
        checkAssignmentText("10 setField - Assumed Decimal ",  fldAssummedDecimal,
                "767.4500", "0007674500");
        checkAssignmentText("10a setField - Assumed Decimal ", fldAssummedDecimal,
                "0.0127", "0000000127");
        checkAssignmentText1("11 setField - Num 2 Decimal ",  fldNum2decimal, "45.67", "45" + Conversion.getDecimalchar() + "67");
        checkAssignmentText1("11a setField - Num 2 Decimal ", fldNum2decimal, "0.67", "0" + Conversion.getDecimalchar() + "67");
        
        // TODO Localisation !!!!!
        //checkAssignmentText("11b setField - Num 2 Decimal ",  fldNum2decimal, "45" + Conversion.decimalChar + "67", "45" + Conversion.decimalChar + "67");
        //checkAssignmentText("11c setField - Num 2 Decimal ", fldNum2decimal, "0" + Conversion.decimalChar + "67", "      0.67");
        
        
        checkAssignmentHex("12 setField - Decimal ",        fldDecimal2digits,
                "67.67", "0000006767");
        checkAssignmentHex("12a setField - Decimal ",       fldDecimal2digits,
                "0.67", "0000000067");
        checkAssignmentHex("13 setField - Positive Int ",   fldPositiveInt2digit,
                "765.45", "012b0100");
        checkAssignmentHex("13a setField - Positive Int ",  fldPositiveInt2digit,
                "0.45", "2d000000");
        checkAssignmentHex("13 setField - Positive Int ",   fldPositiveInt2digit,
                "20000000.00", "00943577");

        checkAssignmentText("14 setField - Num 2 Decimal Digits ",
                fldPositiveInt2digit, "543.56", "543.56");
        checkAssignmentText("14a setField - Num 2 Decimal Digits",
                fldPositiveInt2digit, "0.56", "0.56");
        checkAssignmentHex("15 setField - Mainframe Binary ",
                fldMainframeInt, "432", "000001b0");

        checkAssignmentHex("16 setField - Mainframe Binary with Decimal",
                fldMainframeInt2decimal, "4000000.00", "17d78400");
        checkAssignmentHex("17 setField - Byte ", fldByte, "126", "7e");
        checkAssignmentHex("18 setField - Small Int ", fldSmallInt, "31000", "1879");
        checkAssignmentHex("19 setField - Int ", fldInt, "1000000000", "00ca9a3b");
        checkAssignmentHex("20 setField - Long ", fldLong,
                "1000000000000000000", "000064a7b3b6e00d");
        checkAssignmentHex("20a setField - Long ", fldLong,
                "4000000000000000000", "0000909dceda8237");
        checkAssignmentHex("21 setField - Mainframe Small Int ",
                fldMainframeSmallInt, "31000", "7918");
        checkAssignmentHex("22 setField - Mainframe Long ",
                fldMainframeLong, "1000000000000000000", "0de0b6b3a7640000");
        checkAssignmentHex("22a setField - Mainframe Long ",
                fldMainframeLong, "4000000000000000000", "3782dace9d900000");

        checkAssignmentHex("23 setField - Mainframe Packed ",
                fldMainframePackedDecimal, "23451", "23451c");
        checkAssignmentHex("23a setField - Mainframe Packed ",
                fldMainframePackedDecimal, "1", "00001c");
        checkAssignmentHex("24 setField - Mainframe Packed with decimal",
                fldMainframePackedwithDecimal, "1234.0", "12340c");
        checkAssignmentHex("24a setField - Mainframe Packed with decimal",
                fldMainframePackedwithDecimal, "0.1", "00001c");

        checkAssignmentText("25 setField - Mainframe Zoned ", fldZoned, "23", "2C");
        checkAssignmentText("26 setField - Mainframe Zoned with decimal",
                fldZonedWithDecimal, "23.45", "234E");

        checkAssignmentHex("27 setField - Bit 1byte ", fldBit1, "10010010", "92");
        checkAssignmentHex("28 setField - Bit 2byte ", fldBit2, "1000100101010101", "8955");

        checkAssignmentHex("27a setField - Bit 1byte ", fldBit1, "10010", "12");
        checkAssignmentHex("28a setField - Bit 2byte ", fldBit2, "100101010101", "0955");


        checkAssignment(" 4n setField - Double Field ",    fldDouble, "-989.878");
        checkAssignment(" 5n setField - Float  Field ",    fldFloat, "-898.343");
        checkAssignmentText(" 6n setField - Num  Field ",      fldNum, "-7475", "-7475");
        checkAssignmentText(" 7n setField - Num Right Just",   fldNumRightJust, "-876", "    -876");
        checkAssignmentText(" 8n setField - Num Zero Padded Field ",
                fldNumZeroPadded, "-876", "-0000876");

        checkAssignmentText("10n setField - Assumed Decimal ",
                fldAssummedDecimal, "-67.4500", "-000674500");
        checkAssignmentText("11n setField - Num 2 Decimal ",
                fldNum2decimal, "-45.67", "    -45.67");

        checkAssignmentText("14n setField - Num 2 Decimal Digits ",
                fldNum2decimal, "-43.56", "    -43.56");
        checkAssignmentHex("15n setField - Mainframe Binary ",
                fldMainframeInt, "-432", "fffffe50");

        checkAssignmentHex("16n setField - Mainframe Binary with Decimal",
                fldMainframeInt2decimal, "-4000000.00", "e8287c00");
        checkAssignmentHex("17n setField - Byte ", fldByte, "-126", "82");
        checkAssignmentHex("18n setField - Small Int ", fldSmallInt, "-31000", "e886");
        checkAssignmentHex("19n setField - Int ", fldInt,
                "-1000000000", "003665c4");
        checkAssignmentHex("20n setField - Long ", fldLong,
                "-1000000000000000000", "00009c584c491ff2");
        checkAssignmentHex("21n setField - Mainframe Small Int ", fldMainframeSmallInt,
                "-31000", "86e8");
        checkAssignmentHex("22n setField - Mainframe Long ", fldMainframeLong,
                "-1000000000000000000", "f21f494c589c0000");
        checkAssignmentHex("23n setField - Mainframe Packed ",
                fldMainframePackedDecimal, "-23451", "23451d");
        checkAssignmentHex("24n setField - Mainframe Packed with decimal",
                fldMainframePackedwithDecimal, "-1234.0", "12340d");
        checkAssignmentHex("24n2 setField - Mainframe Packed with decimal",
                fldMainframePackedwithDecimal, "-0.1", "00001d");

        checkAssignmentText("25n1 setField - Mainframe Zoned ", fldZoned, "-23", "2L");
        checkAssignmentText("26n1 setField - Mainframe Zoned with decimal",
                fldZonedWithDecimal, "-23.45", "234N");

        checkAssignment(" 4n1 setField - Double Field ",    fldDouble, "-989.878");
        checkAssignment(" 5n1 setField - Float  Field ",    fldFloat, "-898.343");
        checkAssignment(" 6n1 setField - Num  Field ",      fldNum, "-7475");
        checkAssignment(" 7n1 setField - Num Right Just",   fldNumRightJust, "-876");
        checkAssignment(" 8n1 setField - Num Zero Padded Field ", fldNumZeroPadded, "-876");

        checkAssignmentText("10n1 setField - Assumed Decimal ",
                fldAssummedDecimal, "-0.0127", "-000000127");
        checkAssignmentText("11n1 setField - Num 2 Decimal ",
                fldNum2decimal, "-1.27", "     -1.27");
        //checkAssignment("12n setField - Decimal ",        11, "-67.67");

        checkAssignment("14n1 setField - Num 2 Decimal Digits ", fldNum2decimal, "-1.26");
        checkAssignmentHex("15n1 setField - Mainframe Binary ",
                fldMainframeInt, "-127", "ffffff81");

        checkAssignmentHex("16n2 setField - Mainframe Binary with Decimal",
                fldMainframeInt2decimal, "-1.27", "ffffff81");
        checkAssignmentHex("17n2 setField - Byte ", fldByte, "-126", "82");
        checkAssignmentHex("18n2 setField - Small Int ", fldSmallInt, "-127", "81ff");
        checkAssignmentHex("19n2 setField - Int ", fldInt, "-127", "81ffffff");
        checkAssignmentHex("20n2 setField - Long ", fldLong, "-127", "81ffffffffffffff");
        checkAssignmentHex("21n2 setField - Mainframe Small Int ",
                fldMainframeSmallInt, "-31000", "86e8");
        checkAssignmentHex("22n2 setField - Mainframe Long ",
                fldMainframeLong, "-127", "ffffffffffffff81");

        checkAssignmentHex("23n2 setField - Mainframe Packed ",
                fldMainframePackedDecimal, "-1", "00001d");
        checkAssignmentHex("24n2 setField - Mainframe Packed with decimal",
                fldMainframePackedwithDecimal, "-1.0", "00010d");

        checkAssignmentText("25n2 setField - Mainframe Zoned ", fldZoned, "-3", "0L");
        checkAssignmentText("26n2 setField - Mainframe Zoned with decimal",
                fldZonedWithDecimal, "-0.45", "004N");

    }


    public void testZoned() throws RecordException {

        int i;
        int num = 20;
        String s, t;

        System.out.println("Checking Zoned ... ");
        for (i = 0; i < 10; i++) {
            s = Integer.toString(num + i);

            checkAssignment("Zoned Error on " + i, fldZoned,  s);
            checkAssignment("Zoned Error on -" + i, fldZoned,  "-" + s);

            setFldValue(fldZoned, "+" + s);
            t = getFldValue(fldZoned);
            assertEquals("Zoned Error on +" + i + " " + t + " <> " + s, s, t);
        }
        //getFldValue(fld);
    }


    public void testZoned2() throws RecordException {

        int i;
        String s, t;

        System.out.println("Checking Zoned ... ");
        for (i = 0; i < 10; i++) {
            s = "1.2" + i;

            checkAssignment("Zoned Error on " + i, fldZonedWithDecimal,  s);
            checkAssignment("Zoned Error on -" + i, fldZonedWithDecimal,  "-" + s);

            setFldValue(fldZonedWithDecimal, "+" + s);
            t = getFldValue(fldZonedWithDecimal);
            assertEquals("Zoned Error on +" + i + " " + t + " <> " + s, s, t);
        }
        //getFldValue(fld);
    }


    /**
     * Checks assignment to a numeric field
     *
     * @param msg Error Message
     * @param fld Field Definition to assign a value to
     * @param val value to assign
     *
     * @throws RecordException any conversion error
     */
    private void checkAssignment(String msg, FieldDetail fld,  String val)
    					throws RecordException {

        String s;
        setFldValue(fld, val);
        s = getFldValue(fld);
        assertEquals(msg + " " + s + " <> " + val, val, s);
    }


    /**
     * Checks assignment to a numeric field
     *
     * @param msg Error Message
     * @param fld Field to assign a value to
     * @param val value to assign
     * @param hexVal expected hex value
     *
     * @throws RecordException any conversion error
     */
    private void checkAssignmentHex(String msg, FieldDetail fld,  String val, String hexVal)
    					throws RecordException {

        String s;
        setFldValue(fld, val);

        s = typeManager.getType(Type.ftHex).getField(rec, fld.getPos(), fld).toString();

        assertEquals(msg + " hex check " + s + " <> " + hexVal, hexVal, s);
/*        if (!s.equals(hexVal)) {
            System.out.println("==> " + msg + " " + fldNum + " " + val
                + " " + line.getField(0, fldNum) + " " + s + " " + hexVal);
        }*/

        s = getFldValue(fld);
        assertEquals(msg + " " + s + " <> " + val, val, s);
    }



    /**
     * Set Fields value
     * @param fld field definition
     * @param val value to assign
     * @throws RecordException any error that occurs
     */
    private void setFldValue(FieldDetail fld, String val) throws RecordException {
        typeManager.getType(fld.getType()).setField(rec, fld.getPos(), fld, val);
    }


    /**
     * Checks assignment to a fields
     *
     * @param msg Error Message
     * @param fld Field Definition to assign a value to
     * @param val value to assign
     * @param text expected hex value
     *
     * @throws RecordException any conversion error
       */
    private void checkAssignmentText(String msg, FieldDetail fld,  String val, String text)
	throws RecordException {

        String s;
        setFldValue(fld, val);

        s = typeManager.getType(Type.ftChar).getField(rec, fld.getPos(), fld).toString();

        //assertEquals(msg + " hex check " + s + " <> " + text, text, s);
        if (!s.equals(text)) {
            System.out.println("==> " + msg + " " + fldNum + " " + val
                + " " + getFldValue(fld) + " >" + s + "< !" + text + "!");
         }

        s = getFldValue(fld);
        assertEquals(msg + " " + s + " <> " + val, val, s);
    }

    private void checkAssignmentText1(String msg, FieldDetail fld,  String val, String text)
    		throws RecordException {

    	        String s;
    	        setFldValue(fld, val);

    	        s = typeManager.getType(Type.ftChar).getField(rec, fld.getPos(), fld).toString();

    	        //assertEquals(msg + " hex check " + s + " <> " + text, text, s);
    	        if (!s.equals(text)) {
    	            System.out.println("==> " + msg + " " + fldNum + " " + val
    	                + " " + getFldValue(fld) + " >" + s + "< !" + text + "!");
    	         }

    	        s = getFldValue(fld);
    	        assertEquals(msg + " " + s + " <> " + val, text, s);
    	    }



    /**
     * Test Line.setFieldConversion
     */
    public void testSetFieldConversion() {

        checkConversionError(" 2 setField - Decimal Field ",   fldDecimal);
        checkConversionError(" 3 setField - Double Field ",    fldDouble);
        checkConversionError(" 4 setField - Float  Field ",    fldFloat);
        checkConversionError(" 5 setField - Num  Field ",      fldNum);
        checkConversionError(" 5 setField - Num Right Just",   fldNumRightJust);
        checkConversionError(" 7 setField - Num Zero Padded Field ", fldNumZeroPadded);
        checkConversionError(" 8 setField - Postive Int ",     fldPostiveInt);
        checkConversionError(" 9 setField - Assumed Decimal ", fldAssummedDecimal);
        checkConversionError("10 setField - Num 2 Decimal ",   fldNum2decimal);
        checkConversionError("11 setField - Decimal ",         fldDecimal2digits);
        checkConversionError("12 setField - Positive Int ",    fldPositiveInt2digit);
        checkSizeError("12 setField - Positive Int ~ assign negative ",
                fldPositiveInt2digit, "-5");

        checkConversionError("13 setField - Num 2 Decimal Digits ", fldNum2decimal);
        checkConversionError("14 setField - Mainframe Binary ",     fldMainframeInt);
        checkConversionError("14 setField - Num 2 Decimal Digits ", fldNum2decimal);
        checkConversionError("14a setField - Num 2 Decimal Digits", fldNum2decimal);
        checkConversionError("15 setField - Mainframe Binary ",     fldMainframeInt);

        checkConversionError("16 setField - Mainframe Binary with Decimal",
                fldMainframeInt2decimal);
        checkConversionError("17 setField - Byte ", fldByte);
        checkConversionError("18 setField - Small Int ", fldSmallInt);
        checkConversionError("19 setField - Int ", fldInt);
        checkConversionError("20 setField - Long ", fldLong);
        checkConversionError("21 setField - Mainframe Small Int ", fldMainframeSmallInt);
        checkConversionError("22 setField - Mainframe Long ", fldMainframeLong);

        checkConversionError("23 setField - Mainframe Packed ", fldMainframePackedDecimal);
        checkConversionError("23a setField - Mainframe Packed ", fldMainframePackedDecimal);
        checkConversionError("24 setField - Mainframe Packed with decimal",
                fldMainframePackedwithDecimal);

        checkConversionError("25n1 setField - Mainframe Zoned ", fldZoned);
        checkConversionError("26n1 setField - Mainframe Zoned with decimal", fldZonedWithDecimal);

        checkConversionError("27 setField - Bit 1byte ", fldBit1);
        checkConversionError("28 setField - Bit 2byte ", fldBit2);

        checkConversionError("27a setField - Bit 1byte ", fldBit1, "200010010");
        checkConversionError("28a setField - Bit 2byte ", fldBit2, "20000100101010101");

    }

    /**
     * Test Line.setFieldConversion
     */
    public void testSetFieldSizeError() {

        checkSizeError(" 2 setField - Decimal Field ",   fldDecimal, "12345678901");
        checkSizeError(" 5 setField - Num  Field ",     fldNum			, "123456789");
        checkSizeError(" 5 setField - Num Right Just",  fldNumRightJust		, "123456789");
        checkSizeError(" 7 setField - Num Zero Padded Field ",
                fldNumZeroPadded	, "123456789");
        checkSizeError(" 8 setField - Postive Int ",    fldPostiveInt		, "5000000000");
        checkSizeError(" 9 setField - Assumed Decimal ", fldAssummedDecimal	, "7654321");
        checkSizeError("10 setField - Num 2 Decimal ",  fldNum2decimal		, "987654321");
        checkSizeError("11 setField - Decimal ",        fldDecimal2digits	, "987654321");
        checkSizeError("12 setField - Positive Int ",   fldPositiveInt2digit	, "50000000");
        checkSizeError("13 setField - Num 2 Decimal Digits ", fldNum2decimal1		, "54356");
        checkSizeError("14 setField - Mainframe Binary ",
                fldMainframeInt2decimal, "5000000000");

        checkSizeError("16 setField - Mainframe Binary with Decimal",
                fldMainframeInt2decimal, "5000000000");
        checkSizeError("17 setField - Byte ", fldByte, "128");
        checkSizeError("18 setField - Small Int ", fldSmallInt, "33000");
        checkSizeError("19 setField - Int ", fldInt, "3000000000");
        System.out.println();
        System.out.println(" I am here !");
        checkSizeError("20 setField - Long ", fldLong, "80000000000000000000");
        checkSizeError("21 setField - Mainframe Small Int ", fldMainframeSmallInt, "33000");
        checkSizeError("22 setField - Mainframe Long ", fldMainframeLong, "80000000000000000000");

        checkSizeError("23 setField - Mainframe Packed ", fldMainframePackedDecimal, "234561");
        checkSizeError("24 setField - Mainframe Packed with decimal",
                fldMainframePackedwithDecimal, "12345.0");

        checkSizeError("25 setField - Mainframe Zoned ", fldZoned, "123");
        checkSizeError("26 setField - Mainframe Zoned with decimal", fldZonedWithDecimal, "123");
        checkSizeError("26a setField - Mainframe Zoned with decimal",
                fldZonedWithDecimal, "123.12");

        checkSizeError("27 setField - Bit 1byte ", fldBit1, "1100010010");
        checkSizeError("28 setField - Bit 2byte ", fldBit2, "110000100101010101");
    }



    /**
     * Test setting a field with a value that is to long
     *
     * @param fld field definition to use
     * @param value value to assign to the supplied field
     * @param msg error message to use if size error is not thrown
     */
    private void checkSizeError(String msg, FieldDetail fld, String value) {

        try {
            setFldValue(fld, value);

            System.out.println("::> " + msg + " " + value + " :: " + getFldValue(fld));
            throw new AssertionError("Size Error: " + msg + " " + value + " :: " + getFldValue(fld));
        } catch (RecordException e) {
        }
    }


    /**
     * Test setting a numeric field that is not numeric
     *
     * @param fld field definition to use
     * @param msg error message to use if size error is not thrown
     */
    private void checkConversionError(String msg, FieldDetail fld) {
        checkConversionError(msg, fld, "a");
    }


    /**
     * Test setting a field to an invalid value
     *
     * @param msg error message to use if size error is not thrown
     * @param fld field definition to use
     * @param value value to test
     */
    private void checkConversionError(String msg, FieldDetail fld, String value) {
        try {
            setFldValue(fld, value);

            throw new AssertionError("Conversion Error: " + msg);
        } catch (RecordException e) {
        } catch (NumberFormatException e) {
        }
    }



    /**
     * Check retrieving Mainframe Field etc
     *
     */
    public void testMainframe() {
        String expected = "69694158";

        FieldDetail fld = getTypeMainframe(1, 8, Type.ftChar, 0);
        String s = typeManager.getType(fld.getType()).getField(recDtar020, fld.getPos(), fld).toString();
        assertEquals("Testing Mainframe getField " + expected + " <> " + s,
                expected, s);

        try {
            typeManager.getType(fld.getType()).setField(recDtar020, fld.getPos(), fld, "1");
            typeManager.getType(fld.getType()).setField(recDtar020, fld.getPos(), fld, s);
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }

}
