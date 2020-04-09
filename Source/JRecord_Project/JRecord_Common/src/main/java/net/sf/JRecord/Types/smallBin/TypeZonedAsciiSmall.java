package net.sf.JRecord.Types.smallBin;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Types.TypeZoned;

public class TypeZonedAsciiSmall extends TypeBaseXBinary {


	private TypeZoned typeZoned;
	
	private static final byte[] NUMERIC_BYTES = "+-.,09aijrAIJR".getBytes();
//	private static final char ch4ByteMinus = (char) (NUMERIC_BYTES[1] & 0xff);
//	private static final char ch4ByteComma = (char) (NUMERIC_BYTES[4] & 0xff);
	private static final char ch4Byte0 = (char) (NUMERIC_BYTES[4] & 0xff);
//	private static final char ch4Byte1 = (char) ((NUMERIC_BYTES[5] & 0xff) + 1);
//	private static final char ch4Byte2 = (char) ((NUMERIC_BYTES[5] & 0xff) + 2);
//	private static final char ch4Byte3 = (char) ((NUMERIC_BYTES[5] & 0xff) + 3);
//	private static final char ch4Byte4 = (char) ((NUMERIC_BYTES[5] & 0xff) + 4);
//	private static final char ch4Byte5 = (char) ((NUMERIC_BYTES[5] & 0xff) + 5);
//	private static final char ch4Byte6 = (char) ((NUMERIC_BYTES[5] & 0xff) + 6);
//	private static final char ch4Byte7 = (char) ((NUMERIC_BYTES[5] & 0xff) + 7);
//	private static final char ch4Byte8 = (char) ((NUMERIC_BYTES[5] & 0xff) + 8);
//	private static final char ch4Byte9 =(char)  (NUMERIC_BYTES[5] & 0xff);
	private static final char sign1a  = (char)  (NUMERIC_BYTES[6] & 0xff);
	private static final char sign9a  = (char)  (NUMERIC_BYTES[7] & 0xff);
	private static final char signm1a = (char)  (NUMERIC_BYTES[8] & 0xff);
	private static final char signm9a = (char)  (NUMERIC_BYTES[9] & 0xff);
	private static final char sign1b  = (char)  (NUMERIC_BYTES[10] & 0xff);
	private static final char sign9b  = (char)  (NUMERIC_BYTES[11] & 0xff);
	private static final char signm1b = (char)  (NUMERIC_BYTES[12] & 0xff);
	private static final char signm9b = (char)  (NUMERIC_BYTES[13] & 0xff);
	
	public TypeZonedAsciiSmall(boolean positive) {
		super(positive, false, false);
		typeZoned = new TypeZoned(positive);
	}

	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.smallBin.TypeBaseXBinary#setField(byte[], int, net.sf.JRecord.Common.IFieldDetail, java.lang.Object)
	 */
	@Override
	public byte[] setField(byte[] record, int position, IFieldDetail field, Object value) {
		if (value == null || value == CommonBits.NULL_VALUE ) {
			return setUnscaledLong(record, position, field, 0);
		} else if (value instanceof Number) {
			return super.setField(record, position, field, value);
		}
		return typeZoned.setField(record, position, field, value);
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.TypeNum#formatValueForRecord(net.sf.JRecord.Common.IFieldDetail, java.lang.String)
	 */
	@Override
	public String formatValueForRecord(IFieldDetail field, String val) {
		return typeZoned.formatValueForRecord(field, val);
	}


	@Override
	public long asUnscaledLong(byte[] record, int position, IFieldDetail field) {
		long val = 0;
		int en = position + field.getLen() - 2;
		int d;
		for (int i = position - 1; i < en; i++) {
			d = record[i] & 0x0F;
			if (d > 9) { 
				throw new RecordException(field.getName() + ": Invalid Zoned Decimal: "
								+ Conversion.getString(record, position, en, ""));
			}
			val = val * 10 + d;
		}
		char signChar = (char) (record[en] & 0xff);
		int sign = 1;
		int last = 0;
		
		if (signChar >= sign1a && signChar <= sign9a) {
			last = signChar - sign1a + 1;
		} else if (signChar >= sign1b && signChar <= sign9b) {
			last = signChar - sign1b + 1;
		} else if (signChar >= signm1a && signChar <= signm9a) {
			last = signChar - signm1a + 1;
			sign = -1;
		} else if (signChar >= signm1b && signChar <= signm9b) {
			last = signChar - signm1b + 1;
			sign = -1;
		} else if (signChar == Conversion.getPositive0EbcdicZoned()) {
		} else if (signChar == Conversion.getNegative0EbcdicZoned()) {
			sign = -1;
		} else if (signChar >= '0' && signChar <= '9') {
			last = signChar - '0';
		} else {
			throw new RecordException(field.getName() + ": Invalid Zoned Decimal Sign: "
					+ Conversion.getString(record, position, en+1, ""));
		}

		return sign * (val * 10 + last);
	}

	@Override
	public byte[] setUnscaledLong(byte[] record, int position, IFieldDetail field, long value) {

		int en = position + field.getLen() - 2;
		int pos = position - 1;
//		int signNyble = POSITIVE_SIGN_NYBLE;
		boolean negative = value < 0;
		
		if (negative) {
			value = - value;
		}
		
		
		long digit = value % 10;
		
		if (super.isPositive()) {
			record[en] = (byte) (digit + ch4Byte0);
		} else if (digit == 0) {
			char ch = Conversion.getPositive0EbcdicZoned();
			if (negative) {
				ch = Conversion.getNegative0EbcdicZoned();
			}
			record[en] = (byte) (ch);
		} else {
			char ch = sign1b;
			if (negative) {
				ch = signm1b;
			}
	
			record[en] = (byte) (digit + ch - 1);
		}
		for (int i = en-1; i >= pos; i--) {
			value = value / 10;
			record[i] = (byte) (value % 10 + ch4Byte0);	
		}
		return record;
	}

}
