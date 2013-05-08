/*
 * @Author Bruce Martin
 * Created on 9/01/2007
 *
 * Purpose:
 */
package net.sf.JRecord.zTest.Types;

import junit.framework.TestCase;
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

        fldChar               = getType(1,  5, Type.ftChar, 0);
        fldCharPadded         = getType(6,  5, Type.ftCharNullPadded, 0);
        fldCharNullTerminated = getType(11, 5, Type.ftCharNullTerminated, 0);
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

    private FieldDetail getType(int pos, int len, int type, int decimal) {

        FieldDetail field = new FieldDetail("", "", type, decimal, "", -1, "");

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


}
