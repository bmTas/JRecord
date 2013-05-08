/*
 * @Author Bruce Martin
 * Created on 5/09/2005 for RecordEditor Version 0.55
 *
 * Purpose:
 *   Float / Double field Type
 *
 * Changes
 * # Version 0.56 Bruce Martin 2007/01/16
 *   - remove unused field val
 *
 * # Version 0.60 Bruce Martin 2007/02/16
 *   - Starting to seperate the Record package out from the RecordEditor
 *     so that it can be used seperately. So classes have been moved
 *     to the record package (ie RecordException + new Constant interface
 */
package net.sf.JRecord.Types;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;

/**
 * Float / Double field Type
 *
 * <p>This class is the interface between the raw data in the file
 * and what is to be displayed on the screen for Float or Double fields
 *
 * @author Bruce Martin
 *
 * @version 0.55
 */
public class TypeFloat extends TypeNum {

	private static final int  LENGTH_OF_DOUBLE  =  8;
	private static final int  LENGTH_OF_FLOAT   =  4;


    /**
     *  Float / Double field Type
     *
     * <p>This class is the interface between the raw data in the file
     * and what is to be displayed on the screen for Float or Double fields
     */
    public TypeFloat() {
        super(true, false, false, false, true);
    }

    /**
     * @see net.sf.JRecord.Types.Type#getField(byte[], int, net.sf.JRecord.Common.FieldDetail)
     */
    public Object getField(byte[] record,
            final int position,
			final IFieldDetail field) {
         Object val = "";

         int fldLength = field.getLen();
         int pos = position - 1;

         if (fldLength == LENGTH_OF_FLOAT) {
        	 val = Float.toString(Float.intBitsToFloat(Conversion.getLittleEndianBigInt(record, pos, pos + fldLength).intValue()));
//             val = Float.toString(
//                     Float.intBitsToFloat(
//                        Conversion.getBigInt(record, pos, fldLength).intValue()));
         } else if (fldLength == LENGTH_OF_DOUBLE) {
        	 val = Double.toString(Double.longBitsToDouble(Conversion.getLittleEndianBigInt(record, pos, pos + fldLength).longValue()));
//             val = Double.toString(
//                 Double.longBitsToDouble(
//                     Conversion.getBigInt(record, pos, fldLength).longValue()));
         }

         return "" + val;
    }


    /**
     * @see net.sf.JRecord.Types.Type#setField(byte[], int, net.sf.JRecord.Common.FieldDetail, java.lang.Object)
     */
    public byte[] setField(byte[] record,
            final int position,
			final IFieldDetail field,
			Object value)
    throws RecordException {

        int len = field.getLen();
        int pos = position - 1;
        double doubleVal  = getBigDecimal(field, value.toString()).doubleValue();

	    if (len == LENGTH_OF_FLOAT) {
	        long l = Float.floatToRawIntBits((float) doubleVal);
	        Conversion.setLongLow2High(record, pos, len, l, true);
	    } else if (len == LENGTH_OF_DOUBLE) {
	        long l = Double.doubleToRawLongBits(doubleVal);
	        Conversion.setLongLow2High(record, pos, len, l, true);
	    }

	    return record;
    }
}
