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


import java.io.FileOutputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import junit.framework.TestCase;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.XmlLineReader;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 *
 *
 * @author Bruce Martin
 *
 */
public class TstXmlLine extends TestCase {

	private CopybookLoader copybookInt = new CobolCopybookLoader();

    private static String copyBookName    = "Cbl_Line_Test_Record";
    private static String copyBookDTAR020 = "DTAR020";
    private static int xmlTotal = 5;

    private static byte[] rec =
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

    private static byte[] recDtar020 =
    { -10,  -7, -10,  -7, -12, -15, -11,  -8,   2,  12,   0,  64,  17,-116
        ,  40,  12,   0,   0,   0,   0,  28,   0,   0,   0,   0,  80,  28 };


    //private static LayoutDetail copyBook = null;

    private AbstractLine line;

    /**
     * @see TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();

        LayoutDetail iCopyBook = 
                copybookInt.loadCopyBook(
                        TstConstants.COBOL_DIRECTORY + copyBookName + ".cbl",
                        CopybookLoader.SPLIT_NONE, 0, "",
                        ICopybookDialects.FMT_MAINFRAME, 0, null
                        //ICopybookDialects.FMT_INTEL, 0, null
                ).asLayoutDetail();

        line = getAsXml(new Line(iCopyBook, rec), "");
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

        assertEquals(" 1 GetField - Character Field " + getLineField( 0),
                "asdf", getLineField( 0));
        assertEquals(" 2 GetField - Character Field ", "    qwerty", getLineField( 1));
        assertEquals(" 3 GetField - Decimal Field ", "123", getLineField( 2));
        assertEquals(" 7 GetField - Num Right Just", "123", getLineField( 6));
        assertEquals(" 8 GetField - Num Zero Padded Field ", "456", getLineField( 7));
        assertEquals("10 GetField - Assumed Decimal ", "123.4500", getLineField( 9));
        assertEquals("11 GetField - Num 2 Decimal ", "23.67", getLineField( 10));
        assertEquals("12 GetField - Decimal ", "45.67", getLineField( 11));
        //assertEquals("13 GetField - Positive Int ", "123.45", getLineField( 12));

        assertEquals("14 GetField - Num 2 Decimal Digits ", "234.56", getLineField( 13));
        //assertEquals("15 GetField - Mainframe Binary ", "321", getLineField( 14));
        //assertEquals("16 GetField - Mainframe Binary with Decimal", "4000000.00", getLineField( 15));
        //assertEquals("17 GetField - Byte ", "127", getLineField( 16));
        //assertEquals("18 GetField - Small Int ", "31000", getLineField( 17));
        //assertEquals("19 GetField - Int ", "1000000000", getLineField( 18));
        //assertEquals("20 GetField - Long ", "100000000000000000", getLineField( 19));
        assertEquals("23 GetField - Mainframe Packed ", "1234", getLineField( 22));
        assertEquals("24 GetField - Mainframe Packed with decimal", "123.0", getLineField( 23));

        assertEquals("25 GetField - Mainframe Zoned ", "-12", getLineField( 24));
        assertEquals("26 GetField - Mainframe Zoned with decimal", "-0.12", getLineField( 25));

        //assertEquals("27 GetField - Bit 1byte ", "10000000", getLineField( 26));
        //assertEquals("28 GetField - Bit 2byte ", "1000000110000011", getLineField( 27));

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

    @SuppressWarnings("deprecation")
	private Object getLineField(int fld) {
    	return line.getField(1, fld + xmlTotal);
    }

    /**
     * check Line.getFieldText
     */
    public void testGetFieldText() {

        assertEquals("GetFieldText - Character Field ", "asdf", line.getFieldText(1, 0 + xmlTotal));
        assertEquals("GetFieldText - Character Field ", "    qwerty", line.getFieldText(0, 1 + xmlTotal));

       // assertEquals("GetFieldText - 4 >> ", "ChW�", line.getFieldText(0, 4));
       // assertEquals("GetFieldText - 5 >> ", "3456", line.getFieldText(1, 5 + xmlTotal));
      //  assertEquals("GetFieldText - 6 >> ", "     123", line.getFieldText(1, 6 + xmlTotal));
    }


 
    public void testGetFieldValueString() throws Exception {

        assertEquals(" 1 GetFieldValue - Character Field " + line.getFieldValue(1, xmlTotal + 0).asString(),
                "asdf", line.getFieldValue(1, xmlTotal + 0).asString());
        assertEquals(" 2 GetFieldValue - Character Field ", "    qwerty", line.getFieldValue(1, xmlTotal + 1).asString());
        assertEquals(" 3 GetFieldValue - Decimal Field ", "123", line.getFieldValue(1, xmlTotal + 2).asString());
        assertEquals(" 7 GetFieldValue - Num Right Just", "123", line.getFieldValue(1, xmlTotal + 6).asString());
        assertEquals(" 8 GetFieldValue - Num Zero Padded Field ", "456", line.getFieldValue(1, xmlTotal + 7).asString());
        assertEquals("10 GetFieldValue - Assumed Decimal ", "123.4500", line.getFieldValue(1, xmlTotal + 9).asString());
        assertEquals("11 GetFieldValue - Num 2 Decimal ", "23.67", line.getFieldValue(1, xmlTotal + 10).asString());
        assertEquals("12 GetFieldValue - Decimal ", "45.67", line.getFieldValue(1, xmlTotal + 11).asString());
        //assertEquals("13 GetFieldValue - Positive Int ", "123.45", line.getFieldValue(1, xmlTotal + 12).asString());

        assertEquals("14 GetFieldValue - Num 2 Decimal Digits ", "234.56", line.getFieldValue(1, xmlTotal + 13).asString());
        //assertEquals("15 GetFieldValue - Mainframe Binary ", "321", line.getFieldValue(1, xmlTotal + 14).asString());
        //assertEquals("16 GetFieldValue - Mainframe Binary with Decimal", "4000000.00", line.getFieldValue(1, xmlTotal + 15).asString());
        //assertEquals("17 GetFieldValue - Byte ", "127", line.getFieldValue(1, xmlTotal + 16));
        //assertEquals("18 GetFieldValue - Small Int ", "31000", line.getFieldValue(1, xmlTotal + 17));
        //assertEquals("19 GetFieldValue - Int ", "1000000000", line.getFieldValue(1, xmlTotal + 18));
        //assertEquals("20 GetFieldValue - Long ", "100000000000000000", line.getFieldValue(1, xmlTotal + 19));
        assertEquals("23 GetFieldValue - Mainframe Packed ", "1234", line.getFieldValue(1, xmlTotal + 22).asString());
        assertEquals("24 GetFieldValue - Mainframe Packed with decimal", "123.0", line.getFieldValue(1, xmlTotal + 23).asString());

        assertEquals("25 GetFieldValue - Mainframe Zoned ", "-12", line.getFieldValue(1, xmlTotal + 24).asString());
        assertEquals("26 GetFieldValue - Mainframe Zoned with decimal", "-0.12", line.getFieldValue(1, xmlTotal + 25).asString());

        //assertEquals("27 GetFieldValue - Bit 1byte ", "10000000", line.getFieldValue(1, xmlTotal + 26));
        //assertEquals("28 GetFieldValue - Bit 2byte ", "1000000110000011", line.getFieldValue(1, xmlTotal + 27));

        LayoutDetail copyBook1 = 
                copybookInt.loadCopyBook(
                        TstConstants.COBOL_DIRECTORY + copyBookName + ".cbl",
                        CopybookLoader.SPLIT_NONE, 0, "",
                        ICopybookDialects.FMT_MAINFRAME, 0, null
                ).asLayoutDetail();

        AbstractLine line1 = getAsXml(new Line(copyBook1, rec), "_2");

        assertEquals("15 GetFieldValue - Mainframe Binary ", "321", line1.getFieldValue(1, xmlTotal + 14).asString());
        assertEquals("16 GetFieldValue - Mainframe Binary with Decimal", "4000000.00", line1.getFieldValue(1, xmlTotal + 15).asString());
        assertEquals("21 GetFieldValue - Mainframe Small Int ", "31000", line1.getFieldValue(1, xmlTotal + 20).asString());
        assertEquals("22 GetFieldValue - Mainframe Long ", "100000000000000000", line1.getFieldValue(1, xmlTotal + 21).asString());
    }



    public void testGetFieldValue() throws Exception {

//        assertEquals(" 1 GetFieldValue - Character Field " + line.getFieldValue(0, 0).asString(),
//                "asdf", line.getFieldValue(0, 0).asString());
//       assertEquals(" 2 GetFieldValue - Character Field ", "    qwerty", line.getFieldValue(0, 1).asString());
    	
    	System.out.println(">> 1 " + getLineField( 0));
       	System.out.println(">> 2 " + getLineField( 1));
       	System.out.println(">> 3 " + getLineField( 2));
       	System.out.println(">> 4 " + getLineField( 3));
       	System.out.println(">> 5 " + getLineField( 4));
       	System.out.println(">> 6 " + getLineField( 5));
       	System.out.println(">> 7 " + getLineField( 6));
       	System.out.println(">> 8 " + getLineField( 7));
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

        AbstractLine line1 = getAsXml(new Line(copyBook1, rec), "_2");

        checkAllNums(line1, "15 GetFieldValue - Mainframe Binary ", "321", 14);
        checkDecimal(line1, "16 GetFieldValue - Mainframe Binary with Decimal", "4000000.00", 15);
        checkAllNums(line1, "21 GetFieldValue - Mainframe Small Int ", "31000", 20);
        checkDecimal(line1, "22 GetFieldValue - Mainframe Long ",  "100000000000000000", 21);
        assertEquals("22 GetFieldValue - Mainframe Long  e) ",
        		new BigInteger("100000000000000000"), line1.getFieldValue(1, 21 + xmlTotal).asBigInteger());
     }
    
    private void checkAllNums(String id, String val, int fld) {
    	
    	checkAllNums(line, id,  val, fld);
    }

    
    private void checkAllNums(AbstractLine pLine, String id, String val, int fld) {
    	
    	System.out.print(">>   " + fld + " >" + val
    			+ " >" + pLine.getFieldValue(1, fld + xmlTotal).asString());
       	System.out.println(" " + pLine.getLayout().getRecord(1).getField(fld + xmlTotal).getName());
    	if (Integer.parseInt(val) != pLine.getFieldValue(1, fld + xmlTotal).asInt()) {
//        	int i =  line.getFieldValue(0, fld).asInt();
        	System.out.println(">> " + fld + " " + Integer.parseInt(val) 
    			+ " " + pLine.getFieldValue(1, fld + xmlTotal).asString()
    			+ " " + pLine.getFieldValue(1, fld + xmlTotal).asInt() + " " + pLine.getFieldValue(1, fld).asBigDecimal());
    	}
        assertEquals(id + " a) ", Integer.parseInt(val), pLine.getFieldValue(1, fld + xmlTotal).asInt());
        checkDecimal(pLine, id, val,  fld);
        assertEquals(id + " e) ", new BigInteger(val), pLine.getFieldValue(1, fld + xmlTotal).asBigInteger());
    }
    
    
    private void checkDecimal(AbstractLine pLine, String id, String val, int fld) {
    	
       	System.out.print(">>>> " + fld + " >" + val 
    			+ " >" + pLine.getFieldValue(1, fld + xmlTotal).asString()
    			+ " " + pLine.getLayout().getRecord(1).getRecordName());
       	System.out.println(" " + pLine.getLayout().getRecord(1).getField(fld + xmlTotal).getName());
        assertEquals(id + " b) ", Float.parseFloat(val), pLine.getFieldValue(1, fld + xmlTotal).asFloat(), 0.000001);
        assertEquals(id + " c) ", Double.parseDouble(val), pLine.getFieldValue(1, fld + xmlTotal).asDouble(), 0.000001);
        assertEquals(id + " d) ", new BigDecimal(val), pLine.getFieldValue(1, fld + xmlTotal).asBigDecimal());
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


        checkAssignment(" 4n setField - Double Field ",    3, "-989.878");
        //checkAssignment(" 5n setField - Float  Field ",    4, "-898.343");
        checkAssignmentText(" 6n setField - Num  Field ",      5, "-7475", "-7475");
        checkAssignmentText(" 7n setField - Num Right Just",   6, "-876", "    -876");
        checkAssignmentText(" 8n setField - Num Zero Padded Field ", 7, "-876", "-0000876");

        checkAssignmentText("10n setField - Assumed Decimal ", 9, "-67.4500", "00067450}");
        checkAssignmentText("11n setField - Num 2 Decimal ",  10, "-45.67", "    -45.67");
        //checkAssignment("12n setField - Decimal ",        11, "-67.67");

        checkAssignmentText("14n setField - Num 2 Decimal Digits ", 13, "-43.56", "-43.56");
 
        checkAssignmentText("25n1 setField - Mainframe Zoned ", 24, "-23", "2L");
        checkAssignmentText("26n1 setField - Mainframe Zoned with decimal", 25, "-23.45", "234N");

        checkAssignment(" 4n1 setField - Double Field ",    3, "-989.878");
        //checkAssignment(" 5n1 setField - Float  Field ",    4, "-898.343");
        checkAssignment(" 6n1 setField - Num  Field ",      5, "-7475");
        checkAssignment(" 7n1 setField - Num Right Just",   6, "-876");
        checkAssignment(" 8n1 setField - Num Zero Padded Field ", 7, "-876");

        checkAssignmentText("10n1 setField - Assumed Decimal ", 9, "-0.0127", "00000012P");
        checkAssignmentText("11n1 setField - Num 2 Decimal ",  10, "-1.27", "     -1.27");
        //checkAssignment("12n setField - Decimal ",        11, "-67.67");

        checkAssignment("14n1 setField - Num 2 Decimal Digits ", 13, "-1.26");


        checkAssignmentText("25n2 setField - Mainframe Zoned ", 24, "-3", "0L");
        checkAssignmentText("26n2 setField - Mainframe Zoned with decimal", 25, "-0.45", "004N");
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
        line.setField(1, fldNum + xmlTotal, val);
        s = getLineField( fldNum ).toString();
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
        line.setField(1, fldNum + xmlTotal, val);

        s = line.getFieldText(0, fldNum + xmlTotal);

        //assertEquals(msg + " hex check " + s + " <> " + text, text, s);
        if (!s.equals(text)) {
            System.out.println("==> " + msg + " " + fldNum + " " + val
                + " " + getLineField( fldNum + xmlTotal) + " " + s + " " + text);
         }

        s = getLineField( fldNum).toString();
        assertEquals(msg + " " + s + " <> " + val, val, s);
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
    
    
    
    private	XMLStreamWriter writer;
    private final AbstractLine getAsXml(AbstractLine pLine, String id) throws Exception {
	   	XMLOutputFactory f ;
	   	String fileName = TstConstants.TEMP_DIRECTORY + "TempXml" + id + ".xml";

	   	try {
	   		f = XMLOutputFactory.newInstance();	
	   	} catch (Exception e) {
	   		Object o =  XMLOutputFactory.newInstance("javax.xml.stream.XMLOutputFactory", 
	   				this.getClass().getClassLoader());
	   		f = (XMLOutputFactory) o;
	   	}
	   	
		writer = f.createXMLStreamWriter(new FileOutputStream(
				fileName
		));
		writer.writeStartDocument();
		
		writer.writeEmptyElement("Line");
		writeAttributes(pLine);
 	       
		writer.writeEndDocument();
	    writer.close();
	    
	    XmlLineReader r = new XmlLineReader(true);
	    
	    r.open(fileName);
	    r.read();
	    
	    return r.read();
	}
	
	
	protected final void writeAttributes(AbstractLine pLine) throws XMLStreamException {
		if (pLine != null) {
			int pref = pLine.getPreferredLayoutIdx();
			RecordDetail record = pLine.getLayout().getRecord(pref);
			int end = record.getFieldCount();
			Object value;
			String name;
			
			for (int i = 0; i < end; i++) {
				value = pLine.getField(pref, i);
				name = fixName(record.getField(i).getName());
				if (value != null && ! "".equals(value) 
				&& (name.equals("Filler-4") || ! name.startsWith("Filler"))) {
					 writer.writeAttribute(name, value.toString());
//					 System.out.println("<<< " + fixName(rec.getField(i).getName()) 
//							 + ": " + value.toString());
					 
				} else {
					if (name.startsWith("Filler")) {
						name = name + i;
					}
					writer.writeAttribute(name, " ");
				}
				
			}
		}
	}
	
	protected final String fixName(String name) {
		return name.replace(" ", "_").replace(":","_").replace(".", "_");
	}

}
