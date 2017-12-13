package net.sf.JRecord.Details.fieldValue;

import java.math.BigDecimal;
import java.math.BigInteger;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.Types.smallBin.ITypeBinaryExtendedNumeric;

public class FieldValueSmallBin extends BaseFieldValueLine   {

	private static long[] pot = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
			10000000000L, 100000000000L, 1000000000000L, 10000000000000L, 100000000000000L,
			1000000000000000L, 10000000000000000L, 100000000000000000L,
			1000000000000000000L};

	private ITypeBinaryExtendedNumeric type;

	public FieldValueSmallBin(
					Line theLine,
					IFieldDetail field,
					ITypeBinaryExtendedNumeric type) {
		super(theLine, field);
		this.type = type;
	}



	@Override
	public long asLong() {
		long unscaled = type.asUnscaledLong(theLine.getData(), field.calculateActualPosition(theLine), field);
		if (field.getDecimal() == 0) {
			return unscaled;
		}
		unscaled = unscaled / pot[field.getDecimal() - 1];
		return (unscaled >= 0 ? (unscaled + 5) : (unscaled - 5)) / 10;
				
	}


	@Override
	public double asDouble() {
		return type.asUnscaledLong(theLine.getData(), field.calculateActualPosition(theLine), field) / (double) pot[field.getDecimal()];
	}

	@Override
	public float asFloat() {
		return type.asUnscaledLong(theLine.getData(), field.calculateActualPosition(theLine), field) / (float) pot[field.getDecimal()];
	}

	@Override
	public BigInteger asBigInteger() {	
		return BigInteger.valueOf(asLong());
	}

	@Override
	public BigDecimal asBigDecimal() {
		return BigDecimal.valueOf(type.asUnscaledLong(theLine.getData(), field.calculateActualPosition(theLine), field), field.getDecimal());
	}

	@Override
	public void set(AbstractFieldValue value) {
		type.setField(theLine.getData(), field.calculateActualPosition(theLine), field, value);
	}

	@Override
	public void set(Object value) {
		theLine.setField(field, value);
	}

	@Override
	public void set(long value) {
		setLong(value * pot[field.getDecimal()]);
	}



	/**
	 * @param val
	 */
	private void setLong(long val) {
		int pos = field.calculateActualPosition(theLine);
		theLine.ensureCapacity(pos + field.getLen() - 1);
		type.setUnscaledLong(theLine.getData(), pos, field, val);
	}

	@Override
	public void set(double value) {
		double d = value * pot[field.getDecimal()];
		setLong((long) (d >= 0 ? d + 0.5 : d - 0.5));
	}

	@Override
	public void set(float value) {
		float f = value * pot[field.getDecimal()];
		setLong((long) (f >= 0 ? f + 0.5 : f - 0.5));
	}

}
