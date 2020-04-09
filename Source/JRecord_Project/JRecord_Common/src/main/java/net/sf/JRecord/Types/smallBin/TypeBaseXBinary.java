/**
 * 
 */
package net.sf.JRecord.Types.smallBin;

import java.math.BigDecimal;
import java.math.BigInteger;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Types.TypeNum;

/**
 * 
 * Abstract class to support Short Types (can be represented as a long) i.e.
 * <ul>
 *   <li>
 * </ul>
 * 
 * @author Bruce Martin
 *
 */
public abstract class TypeBaseXBinary extends TypeNum implements ITypeBinaryExtendedNumeric {

	private static long[] pot = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
			10000000000L, 100000000000L, 1000000000000L, 10000000000000L, 100000000000000L,
			1000000000000000L, 10000000000000000L, 100000000000000000L,
			1000000000000000000L};
//	10_000_000_000L, 100_000_000_000L, 1_000_000_000_000L, 10_000_000_000_000L, 100_000_000_000_000L,
//	1_000_000_000_000_000L, 10_000_000_000_000_000L, 100_000_000_000_000_000L,
//	1_000_000_000_000_000_000L};
	
	private static BigDecimal BD_MAX_LONG = BigDecimal.valueOf(Long.MAX_VALUE);
	private static BigDecimal BD_MIN_LONG = BigDecimal.valueOf(Long.MIN_VALUE);

	/**
	 * @param positive
	 */
	public TypeBaseXBinary(boolean positive, boolean binary, boolean couldBeHexHero) {
        super(false, true, true, positive, binary, couldBeHexHero, false);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.TypeNum#setField(byte[], int, net.sf.JRecord.Common.IFieldDetail, java.lang.Object)
	 */
	@Override
	public byte[] setField(byte[] record, int position, IFieldDetail field, Object value) {
		if (value == null || value == CommonBits.NULL_VALUE) {
			return setUnscaledLong(record, position, field, 0);
		} 
		int decimal = field.getDecimal();
		if (value instanceof Number && decimal >= 0 && decimal < pot.length) {
			Number valueNum = (Number) value;
			if (value instanceof BigDecimal) {
				return setUnscaledBigDecimal(record, position, field, ((BigDecimal) value).movePointRight(decimal));
			}
			if (value instanceof BigInteger) {
				return setUnscaledBigDecimal(
						record, position, field, 
						new BigDecimal((BigInteger) value, -decimal));
			}

			double d = valueNum.doubleValue() * pot[decimal];
			if (d > Long.MAX_VALUE || d < Long.MIN_VALUE) {
				throw new RecordException("Value " + valueNum + " is to big for field: " + field.getName());
			}
			
			if (valueNum instanceof Double) {
				return setUnscaledLong(record, position, field, (long) (d >= 0? d + 0.5 : d - 0.5));
			}
			if (valueNum instanceof Float) {
				float f = valueNum.floatValue() * pot[decimal];
				return setUnscaledLong(record, position, field, (long) (f >= 0? f + 0.5 : f - 0.5));
			}
			if (decimal == 0) {
				return setUnscaledLong(record, position, field, valueNum.longValue());
			}
		}
		String valStr = value.toString();
		if ("".equals(valStr)) {
			return setUnscaledLong(record, position, field, 0);
		} 
		return setUnscaledBigDecimal(
				record, position, field,
				(new BigDecimal(valStr))
					.movePointRight(decimal));
	}

//	protected byte[] ensureCapacity(byte[] rec, int length) {
//		if (rec == null) {
//			rec = new byte[length];
//		} else if (rec.length < length) {
//			byte[] newRec = 
//		}
//	}
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.TypePackedDecimal#getField(byte[], int, net.sf.JRecord.Common.IFieldDetail)
	 */
	@Override
	public Object getField(byte[] record, int position, IFieldDetail field) {
		long unscaled = asUnscaledLong(record, position, field);
		int decimal = field.getDecimal();
		if (decimal != 0) {
			return BigDecimal.valueOf(unscaled, decimal);
		} 
		return unscaled;
	}
	
	public final byte[] setUnscaledBigDecimal(byte[] record,
			  int position,
			  IFieldDetail field, 
			  BigDecimal value) {
		
		if (value.compareTo(BD_MIN_LONG) < 0 || value.compareTo(BD_MAX_LONG) > 0) {
			throw new RecordException("Value " + value + " is to big to be stored in field: " + field.getName());
		}
		return setUnscaledLong(record, position, field, value.longValue());
		
	}

}
