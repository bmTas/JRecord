/*
 * @Author Bruce Martin
 * Created on 9/01/2007 for RecordEditor Version 0.56
 *
 * Purpose:
 * 	Type for null terminated char String (ie C style string)
 */
package net.sf.JRecord.Types;

import net.sf.JRecord.Common.IFieldDetail;

/**
 * Type for null terminated char String (ie C style string)
 *
 * @author Bruce Martin
 *
 * @version 0.56
 *
 */
public class TypeCharNullTerminated extends TypeChar {

    private byte padByte = 0;


    /**
     * create a null terminated char type (ie C style string)
     */
    public TypeCharNullTerminated() {
        super(true, true);
    }

    /**
     * @see net.sf.JRecord.Types.TypeChar#getFieldEnd(net.sf.RecordEditor.record.types.FieldDetail, byte[])
     */
    @Override
    protected int getFieldEnd(int position, IFieldDetail currField, byte[] record) {
        int ret = position - 1;
        int end = java.lang.Math.min(ret + currField.getLen(), record.length);

        while (ret < end && (record[ret] != padByte)) {
            ret += 1;
        }

        return ret;
    }

    /**
     * @see net.sf.JRecord.Types.TypeChar#getPadByte(java.lang.String)
     */
    protected byte getPadByte(String font) {
        return padByte;
    }
}
