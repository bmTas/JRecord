/**
 * 
 */
package net.sf.JRecord.Types.smallBin;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldConversionError;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;

/**
 * 
 * Small packed decimal type (< 10 bytes long). It supports
 * setting / retrieving using <i>un-scaled longs</i>.
 * 
 * @author Bruce Martin
 *
 */
public class TypePackedDecimal9 extends TypeBaseXBinary {

	private static final int POSITIVE_SIGNED_NYBLE = 0x0C;

	private static final int UNSIGNED_NYBLE = 0x0F;

	private static final int NEGATIVE_SIGN_NYBLE = CheckPackedDecimal.NEGATIVE_SIGN_NYBLE;

	private static final byte[] TO_NUM = CheckPackedDecimal.TO_NUM;

	
	private static final byte[] TO_PD
		=  { 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09
	       , 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19
	       , 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29
	       , 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39
	       , 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49
	       , 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59
	       , 0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69
	       , 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79
	       , (byte) 0x80, (byte) 0x81, (byte) 0x82, (byte) 0x83, (byte) 0x84, 
	         (byte) 0x85, (byte) 0x86, (byte) 0x87, (byte) 0x88, (byte) 0x89
	       , (byte) 0x90, (byte) 0x91, (byte) 0x92, (byte) 0x93, (byte) 0x94, 
	         (byte) 0x95, (byte) 0x96, (byte) 0x97, (byte) 0x98, (byte) 0x99 };
//	private long[] pot = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
//			10000000000L, 100000000000L, 1000000000000L, 10000000000000L};
	/**
	 * 
	 */
	public TypePackedDecimal9() {
        super(false, true, false);
	}

	/**
	 * @param positive
	 */
	public TypePackedDecimal9(boolean positive) {
        super(positive, true, false);
	}
	


	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.ITypeBinaryNumeric8#asUnscaledLong()
	 */
	@Override
	public long asUnscaledLong(
			byte[] record,
			int position,
			final IFieldDetail field) {
		
	    int pos = position - 1;	
		if (record.length < pos + field.getLen()) {
			throw new RecordException("Invalid Packed Decimal, record is to short: " + field.getName());
		}
		
		long ret = 0;
		int v;

		switch (field.getLen()) {
		case 9: 
			v = TO_NUM[record[pos++] & 0xFF];
			if (v < 0) {invalidMessage(field, pos + 2 - position, record[pos-1]);}
			ret = v;
		case 8: 
			v = TO_NUM[record[pos++] & 0xFF];
			if (v < 0) {invalidMessage(field, pos + 2 - position, record[pos-1]);}
			ret =  ret * 100 + v;
		case 7: 
			v = TO_NUM[record[pos++] & 0xFF];
			if (v < 0) {invalidMessage(field, pos + 2 - position, record[pos-1]);}
			ret = ret * 100 + v;
		case 6: 
			v = TO_NUM[record[pos++] & 0xFF];
			if (v < 0) {invalidMessage(field, pos + 2 - position, record[pos-1]);}
			ret = ret * 100 + v;
		case 5: 
			v = TO_NUM[record[pos++] & 0xFF];
			if (v < 0) {invalidMessage(field, pos + 2 - position, record[pos-1]);}
			ret = ret * 100 + v;
		case 4: 
			v = TO_NUM[record[pos++] & 0xFF];
			if (v < 0) {invalidMessage(field, pos + 2 - position, record[pos-1]);}
			ret = ret * 100 + v;
		case 3: 
			v = TO_NUM[record[pos++] & 0xFF];
			if (v < 0) {invalidMessage(field, pos + 2 - position, record[pos-1]);}
			ret = ret * 100 + v;
		case 2: 
			v = TO_NUM[record[pos++] & 0xFF];
			if (v < 0) {invalidMessage(field, pos + 2 - position, record[pos-1]);}
			ret = ret * 100 + v;
		}
		
		ret = ret * 10 + ((record[pos] & 0xF0)  >> 4);
		int signNyble = record[pos] &  0x0F;
		if (signNyble == NEGATIVE_SIGN_NYBLE) {
			ret = - ret;
		} else if (signNyble < 10) {
			throw new RecordException("Invalid sign in field: " + field.getName() + " " + signNyble
					+ " > "+ Conversion.getDecimalSB(record, position - 1, position + field.getLen() - 1));
		}
		
		return ret;
	}
		
	private void invalidMessage(IFieldDetail field, int pos, byte b) {
		throw new FieldConversionError(field, "Invalid Packed decimal at: " + pos + " in field: " + field.getName()
				+ " Byte: " + Integer.toHexString(b & 0xFF), null);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.ITypeBinaryNumeric8#setUnscaledLong(byte[], long)
	 */
	@Override
	public byte[] setUnscaledLong(
			  byte[] record,
			  int position,
			  IFieldDetail field, 
			  long value) {
	    int pos = position + field.getLen() - 2;
		int signNyble = POSITIVE_SIGNED_NYBLE;
		long oValue = value;
		if (value < 0) {
			signNyble = NEGATIVE_SIGN_NYBLE;
			value = - value;
		} else if (isPositive()) {
			signNyble = UNSIGNED_NYBLE;
		}
		
		record[pos--] = (byte) ((value % 10 << 4) + signNyble);
		value = value / 10;

		switch (field.getLen()) {
		case 9: 
			record[pos--] = TO_PD[(int) (value % 100)];
			value = value / 100;
		case 8: 
			record[pos--] = TO_PD[(int) (value % 100)];
			value = value / 100;
		case 7: 
			record[pos--] = TO_PD[(int) (value % 100)];
			value = value / 100;
		case 6: 
			record[pos--] = TO_PD[(int) (value % 100)];
			value = value / 100;
		case 5: 
			record[pos--] = TO_PD[(int) (value % 100)];
			value = value / 100;
		case 4: 
			record[pos--] = TO_PD[(int) (value % 100)];
			value = value / 100;
		case 3: 
			record[pos--] = TO_PD[(int) (value % 100)];
			value = value / 100;
		case 2: 
			record[pos--] = TO_PD[(int) (value % 100)];
			value = value / 100;
		}
		
		if (value > 0) {
			throw new RecordException("Value " + oValue + " is to big for the field: " + field.getName());
		}
		
		return record;
	}

	@Override
	public boolean isValid(IFieldDetail fldDef, String value) {
		throw new RecordException("Packed decimals are a binary format. It makes no sense to check a Text field");
	}

	@Override
	public boolean isValid(int position, IFieldDetail fldDef, byte[] line) {
		return CheckPackedDecimal.checkPackedDecimal(position, fldDef, line);
	}

	
}
