/*
 * @Author Bruce Martin
 * Created on 9/01/2007
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

package net.sf.JRecord.zTest.Types;

import java.util.Arrays;

import junit.framework.TestCase;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.CharLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.zTest.Common.TestCommonCode;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 *
 *
 * @author Bruce Martin
 *
 */
public class TstCharType extends TestCase {

    private TypeManager typeManager = new TypeManager();

    private byte[] rec1 = {
            97, 115, 100, 32, 32, 113, 119, 101, 0, 0, 113, 119, 101, 0, 32
    };

    private FieldDetail fldChar, fldCharPadded, fldCharNullTerminated;



    /**
     * @see TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();

        fldChar               = getType(1,  5, Type.ftChar, 0, "");
        fldCharPadded         = getType(6,  5, Type.ftCharNullPadded, 0, "");
        fldCharNullTerminated = getType(11, 5, Type.ftCharNullTerminated, 0, "");
    }

    /**
     * Check the Line.getField function (Text Field)
     */
    public void testGetValue() {

        assertEquals(" 1 GetValue - Character Field ", "asd", getFldValue(fldChar));
        System.out.println("!" +  getFldValue(fldCharPadded) + "!");
        System.out.println("!" +  getFldValue(fldCharNullTerminated) + "!");
        assertEquals(" 2 GetValue - Character null padded ", "qwe", getFldValue(fldCharPadded));
        assertEquals(" 3 GetValue - Character null terminated ", "qwe", getFldValue(fldCharNullTerminated));
    }


    public void testSetField() throws RecordException {

        checkAssignmentText(" 1 setField - Char Field ",       fldChar, "zxc", "zxc");
        checkAssignmentText1(" 2 setField - Char null padded ", fldCharPadded, "zxc", "zxc");
        checkAssignmentText1(" 3 setField - Char null terminated ", fldCharNullTerminated, "zxc", "zxc");

        checkAssignmentText(" 4 setField - Char Field ",       fldChar, "zxc1", "zxc1");
        checkAssignmentText1(" 5 setField - Char null padded ", fldCharPadded, "zxc1", "zxc1");
        checkAssignmentText1(" 6 setField - Char null terminated ", fldCharNullTerminated, "zxc1", "zxc1");

        checkAssignmentText(" 7 setField - Char Field ",       fldChar, "zxc12", "zxc12");
        checkAssignmentText1(" 8 setField - Char null padded ", fldCharPadded, "zxc12", "zxc12");
        checkAssignmentText1(" 9 setField - Char null terminated ", fldCharNullTerminated, "zxc12", "zxc12");

        checkAssignmentText("10 setField - Char Field ",       fldChar, "zxc123", "zxc12");
        checkAssignmentText1("11 setField - Char null padded ", fldCharPadded, "zxc123", "zxc12");
        checkAssignmentText1("12 setField - Char null terminated ", fldCharNullTerminated, "zxc123", "zxc12");

        checkAssignmentText1("14 setField - Char null padded ", fldCharPadded, "", "");
        checkAssignmentText1("15 setField - Char null terminated ", fldCharNullTerminated, "", "");

        System.out.println();
        for (int i = 5; i < rec1.length; i++) {
            System.out.print(rec1[i] + ", ");
        }
        System.out.println();

        checkAssignmentText1("17 setField - Char null padded ", fldCharPadded, "a", "a");
        checkAssignmentText1("18 setField - Char null terminated ", fldCharNullTerminated, "b", "b");
    }

    public void testEbcdicCharsets() throws RecordException {
    	tstCharsets(TstConstants.EBCDIC_SINGLE_BYTE_CHARSETS);
    }

    public void testStdCharsets() throws RecordException {
    	String[] charsets = {"", "cp1525", "utf-8"};
    	tstCharsets(charsets);
    }
   

    public void testStdCharsets1() throws RecordException {
    	String[] charsets = {"", "cp1525", "utf-8", "utf-16"};
    	tstCharsets1(charsets);
    	tstCharsets1(TstConstants.EBCDIC_SINGLE_BYTE_CHARSETS);
    }
   
    private void tstCharsets(String[] charsets) throws RecordException {

    	for (String c : charsets) {
    		tstChar(c);
    		tstCharType(c, Type.ftCharNullTerminated);
       		tstCharType(c, Type.ftCharNullPadded);
 //      		tstCharRestOfRecord(c, Type.ftCharRestOfFixedRecord);
      		tstCharRestOfRecord(c, Type.ftCharRestOfRecord);
    	}
    }

    private void tstCharsets1(String[] charsets) throws RecordException {
    	String cobolCopybook
    			= "      01 COMPANY-RECORD.\n"
    			+ "         05 COMPANY-NAME     PIC X(30).\n";
    	for (String c : charsets) {
           	LayoutDetail schema = TestCommonCode.getLayoutFromCobolStr(
        				cobolCopybook, "COMPANY-RECORD",
        				CopybookLoader.SPLIT_NONE, c, ICopybookDialects.FMT_INTEL);
        	System.out.print("\t" + c);
    		Line l1 = new Line(schema);
//    		tstChar(c, l1);
//    		tstCharType(c, Type.ftCharNullTerminated, l1);
//       		tstCharType(c, Type.ftCharNullPadded, l1);
// //      		tstCharRestOfRecord(c, Type.ftCharRestOfFixedRecord);
//      		tstCharRestOfRecord(c, Type.ftCharRestOfRecord, l1);
      		
    		CharLine lchar = new CharLine(schema, "");
    		tstChar(c, lchar);
//     		tstCharRestOfRecord(c, Type.ftCharRestOfRecord, lchar);
    	}
    }

    private void tstChar(String charset) throws RecordException {
    	FieldDetail f = getType(1,  5, Type.ftChar, 0, charset);
    	Type t = TypeManager.getInstance().getType(Type.ftChar);
    	byte[] r;
    	
    	r = getRecord("12345678", charset, 8);
    	assertEquals("12345", t.getField(r, 1, f));
    	assertEquals("12345678", Conversion.toString(r, charset));
    	r = t.setField(r, 1, f, "abc");
    	assertEquals("abc", t.getField(r, 1, f));
    	assertEquals("abc  678", Conversion.toString(r, charset));
    	r = t.setField(r, 1, f, " de");
    	assertEquals(" de", t.getField(r, 1, f));
    	assertEquals(" de  678", Conversion.toString(r, charset));
    	r = t.setField(r, 1, f, "1234567");
    	assertEquals("12345", t.getField(r, 1, f));
    	assertEquals("12345678", Conversion.toString(r, charset));
  }
    
    
    private void tstCharType(String charset, int type) throws RecordException {
    	FieldDetail f = getType(1,  5, type, 0, charset);
    	Type t = TypeManager.getInstance().getType(f.getType());
    	byte[] r;
    	
    	r = getRecord("12345678", charset, 8);
    	assertEquals("12345", t.getField(r, 1, f));
    	assertEquals("12345678", Conversion.toString(r, charset));
    	r = t.setField(r, 1, f, "abc");
    	assertEquals("abc", t.getField(r, 1, f));
//    	assertEquals("abc  678", Conversion.toString(r, charset));
    	r = t.setField(r, 1, f, " de");
    	assertEquals(" de", t.getField(r, 1, f));
//    	assertEquals(" de  678", Conversion.toString(r, charset));
    	r = t.setField(r, 1, f, "1234567");
    	assertEquals("12345", t.getField(r, 1, f));
    	assertEquals("12345678", Conversion.toString(r, charset));
  }

    
    private void tstCharRestOfRecord(String charset, int type) throws RecordException {
    	FieldDetail f = getType(1,  5, type, 0, charset);
    	Type t = TypeManager.getInstance().getType(f.getType());
    	byte[] r;
    	
    	r = getRecord("12345678", charset, 8);
    	assertEquals("12345678", t.getField(r, 1, f));
    	assertEquals("12345678", Conversion.toString(r, charset));
    	r = t.setField(r, 1, f, "abc");
    	assertEquals("abc", t.getField(r, 1, f));
    	assertEquals("abc", Conversion.toString(r, charset));
    	r = t.setField(r, 1, f, " de");
    	assertEquals(" de", t.getField(r, 1, f));
    	assertEquals(" de", Conversion.toString(r, charset));
    	r = t.setField(r, 1, f, "1234567");
    	assertEquals("1234567", t.getField(r, 1, f));
    	assertEquals("1234567", Conversion.toString(r, charset));
  }

    private void tstChar(String charset, AbstractLine l) throws RecordException {
    	FieldDetail f = getType(1,  5, Type.ftChar, 0, charset);
//    	Type t = TypeManager.getInstance().getType(Type.ftChar);
//    	byte[] r;
    	
    	l.setData("12345678");
    	assertEquals("12345", l.getFieldValue(f).asString());
    	assertEquals("12345678", l.getFullLine());
    	l.setField(f, "abc");
    	assertEquals("abc", l.getFieldValue(f).asString());
    	assertEquals("abc  678", l.getFullLine());
    	l.setField(f, " de");
    	assertEquals(" de", l.getFieldValue(f).asString());
    	assertEquals(" de  678", l.getFullLine());
    	l.setField(f, "1234567");
    	assertEquals("12345", l.getFieldValue(f).asString());
    	assertEquals("12345678", l.getFullLine());
  }
    
    
//    private void tstCharType(String charset, int type, AbstractLine l) throws RecordException {
//    	FieldDetail f = getType(1,  5, type, 0, charset);
// //   	Type t = TypeManager.getInstance().getType(f.getType());
////    	byte[] r;
//    	
//    	l.setData("12345678");
//
//    	assertEquals("12345", l.getFieldValue(f).asString());
//    	assertEquals("12345678", l.getFullLine());
//    	l.setField(f, "abc");
//    	assertEquals("abc", l.getFieldValue(f).asString());
////    	assertEquals("abc  678", Conversion.toString(r, charset));
//    	l.setField(f, " de");
//    	assertEquals(" de",l.getFieldValue(f).asString());
////    	assertEquals(" de  678", Conversion.toString(r, charset));
//    	l.setField(f, "1234567");
//    	assertEquals("12345", l.getFieldValue(f).asString());
//    	assertEquals("12345678", l.getFullLine());
//  }
//
//    
//    private void tstCharRestOfRecord(String charset, int type, AbstractLine l) throws RecordException {
//    	FieldDetail f = getType(1,  5, type, 0, charset);
////    	Type t = TypeManager.getInstance().getType(f.getType());
////    	byte[] r;
//   	
//    	l.setData("12345678");
//    	assertEquals("12345678", l.getFieldValue(f).asString());
//    	assertEquals("12345678", l.getFullLine());
//    	l.setField(f, "abc");
//    	assertEquals("abc", l.getFieldValue(f).asString());
//    	assertEquals("abc", l.getFullLine());
//    	l.setField(f, " de");
//    	assertEquals(" de", l.getFieldValue(f).asString());
//    	assertEquals(" de", l.getFullLine());
//    	l.setField(f, "1234567");
//    	assertEquals("1234567", l.getFieldValue(f).asString());
//    	assertEquals("1234567", l.getFullLine());
//  }

    private byte[] getRecord(String charset, String value, int size) {
    	byte spacebyte = Conversion.getBytes(" ", charset)[0];
    	
    	return getRecord(charset, value, size, spacebyte, spacebyte);
    }
    
    private byte[] getRecord(String charset, String value, int size, byte fillByte1, byte fillByte2) {
  	byte[] b = Conversion.getBytes(charset, value);
    	byte[] ret = new byte[size];
    	
    	System.arraycopy(b, 0, ret, 0, Math.min(b.length, size));
    	if (ret.length > b.length) {
    		ret[b.length] = fillByte1;
    		Arrays.fill(ret, b.length+1, size, fillByte2);
    	}
    	return ret;
    }
    
    private FieldDetail getType(int pos, int len, int type, int decimal, String charset) {

        FieldDetail field = new FieldDetail("", "", type, decimal, charset, -1, "");

        field.setPosLen(pos, len);

        return field;
    }

    /**
     * Get field value
     * @param fld field definition
     * @return value of the field
     */
    private String getFldValue(FieldDetail fld) {
        return typeManager.getType(fld.getType()).getField(rec1, fld.getPos(), fld).toString();
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

        s = typeManager.getType(Type.ftChar).getField(rec1, fld.getPos(), fld).toString();

        //assertEquals(msg + " hex check " + s + " <> " + text, text, s);
        if (!s.equals(text)) {
            System.out.println("==> " + msg + " " + fld + " " + val
                + " " + getFldValue(fld) + " >" + s + "< !" + text + "!");
         }

        s = getFldValue(fld);
        assertEquals(msg + " " + s + " <> " + text, text, s);
    }

    private void checkAssignmentText1(String msg, FieldDetail fld,  String val, String text)
	throws RecordException {

        String s;
        setFldValue(fld, val);

         s = getFldValue(fld);

        //assertEquals(msg + " hex check " + s + " <> " + text, text, s);
        if (!s.equals(text)) {
            System.out.println("==> " + msg + " " + fld + " " + val
                + " " + getFldValue(fld) + " >" + s + "< !" + text + "!");
         }

        assertEquals(msg + " " + s + " <> " + text, text, s);
    }

    
    /**
     * Set Fields value
     * @param fld field definition
     * @param val value to assign
     * @throws RecordException any error that occurs
     */
    private void setFldValue(FieldDetail fld, String val) throws RecordException {
        typeManager.getType(fld.getType()).setField(rec1, fld.getPos(), fld, val);
    }

    public void test4Pad() throws RecordException {
    	byte[] b = {32,32,32,32,32,32};
    	FieldDetail fldChar    = getType(1,  5, Type.ftChar, 0, "");
    	FieldDetail fldNum     = getType(1,  5, Type.ftNumAnyDecimal, 0, "");
    	
    	assertEquals("", typeManager.getType(fldChar.getType()).getField(b, 1, fldChar));
    	
    	b = typeManager.getType(fldChar.getType()).setField(b, 1, fldNum, 0);
    	
    	assertEquals("0", typeManager.getType(fldChar.getType()).getField(b, 1, fldChar));

    }
}
