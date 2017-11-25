package net.sf.JRecord.zTest.Types1;

import java.math.BigDecimal;

import junit.framework.TestCase;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeBinBigEndian;
import net.sf.JRecord.Types.TypeBinLittleEndian;
import net.sf.JRecord.Types.TypeNum;
import net.sf.JRecord.Types.TypePackedDecimal;
import net.sf.JRecord.Types.smallBin.ITypeBinaryExtendedNumeric;
import net.sf.JRecord.Types.smallBin.TypeIntBigEndian;
import net.sf.JRecord.Types.smallBin.TypeIntLittleEndian;
import net.sf.JRecord.Types.smallBin.TypePackedDecimal9;

public class TestShortNumber extends TestCase {

	FieldDetail[] flds = {
			FieldDetail.newFixedWidthField("", Type.ftPackedDecimal, 5, 1, 0, ""),
			FieldDetail.newFixedWidthField("", Type.ftPackedDecimal, 4, 2, 0, ""),
			FieldDetail.newFixedWidthField("", Type.ftPackedDecimal, 3, 3, 0, ""),
			FieldDetail.newFixedWidthField("", Type.ftPackedDecimal, 2, 4, 0, ""),
			FieldDetail.newFixedWidthField("", Type.ftPackedDecimal, 1, 5, 0, ""),
	};
	
	public void testGetSetAgainstPd1() {
		tstGetSetSmall( new TypePackedDecimal(), new TypePackedDecimal9());
	}
	
	public void testGetSetAgainstIntBe() {
		tstGetSetSmall(new TypeBinBigEndian(false), new TypeIntBigEndian(false, false));
	}
	public void testGetSetAgainstIntLe() {
		tstGetSetSmall(new TypeBinLittleEndian(false), new TypeIntLittleEndian(false, false));
	}

	
	public void testGetSetAgainstPd2() {
		tstGetSetLarge(new TypePackedDecimal(), new TypePackedDecimal9(), Type.ftPackedDecimal);
	}
	
	public void testGetSetAgainstIntBE() {
		tstGetSetLarge(new TypeBinBigEndian(false), new TypeIntBigEndian(false, false), Type.ftBinaryBigEndian);
	}

	
	public void testGetSetAgainstInt() {
		tstGetSetLarge(new TypeBinLittleEndian(false), new TypeIntLittleEndian(false, false), Type.ftBinaryBigEndian);
	}

	public void testGetAgainstPd1positive() {
		TypePackedDecimal  pd  = new TypePackedDecimal(true);
		TypePackedDecimal9 pd9 = new TypePackedDecimal9(true);
		
		byte[] rec1 = {0, 0, 0, 0, 0};
		
		FieldDetail fld0d =  FieldDetail.newFixedWidthField("", Type.ftPackedDecimal, 1, 5, 0, "");
		FieldDetail fld1d =  FieldDetail.newFixedWidthField("", Type.ftPackedDecimal, 1, 5, 1, "");

		for (int i = 0; i < 1000; i++) {
			for (int j = 1; j < flds.length; j++) {
				chkAsUnscaledLong(rec1, pd, pd9, i, j, fld0d, fld1d);
				assertEquals(0x0F, rec1[4] & 0x0F);
			}
		}
	};
	
	
	public void testGetAgainstIntBEpositive() {
		tstGetPositive(new TypeBinBigEndian(true), new TypeIntBigEndian(true, false));
	}
	
	
	public void testGetAgainstUIntBEpositive() {
		tstGetPositive(new TypeBinBigEndian(true, true), new TypeIntBigEndian(true, true));
	}

	
	public void testGetAgainstIntLEpositive() {
		tstGetPositive(new TypeBinLittleEndian(true), new TypeIntLittleEndian(true, false));
	}
	
	
	public void testGetAgainstUIntLEpositive() {
		tstGetPositive(new TypeBinLittleEndian(true, true), new TypeIntLittleEndian(true, true));
	}


	/**
	 * @param rec1
	 * @param rec2
	 * @param fld1d
	 * @param pd
	 * @param pd9
	 */
	private void tstGetSetSmall(TypeNum pd, ITypeBinaryExtendedNumeric pd9) {
		byte[] rec1 = {0, 0, 0, 0, 0};
		byte[] rec2 = {0, 0, 0, 0, 0};
		FieldDetail fld0d =  FieldDetail.newFixedWidthField("", Type.ftPackedDecimal, 1, 5, 0, "");
		FieldDetail fld1d =  FieldDetail.newFixedWidthField("", Type.ftPackedDecimal, 1, 5, 1, "");
		
		for (int i = -9; i < 10; i++) {
			for (int j = 0; j < flds.length; j++) {
				chkAsUnscaledLong(rec1, pd, pd9, i, j, fld0d, fld1d);
				chkSetUnscaledLong(rec2, pd, pd9, i, j);
			}
		}
		
		for (int i = -999; i < 1000; i++) {
			for (int j = 1; j < flds.length; j++) {
				chkAsUnscaledLong(rec1, pd, pd9, i, j, fld0d, fld1d);
				chkSetUnscaledLong(rec2, pd, pd9, i, j);
			}
		}
		for (int i = -99999; i < 100000; i++) {
			for (int j = 2; j < flds.length; j++) {
				chkAsUnscaledLong(rec1, pd, pd9, i, j, fld0d, fld1d);
				chkSetUnscaledLong(rec2, pd, pd9, i, j);
			}
		}
	
		int val = 100121;
		while (val < 999999999) {
			chkAsUnscaledLong(rec1, pd, pd9, val, 4, fld0d, fld1d);
			chkAsUnscaledLong(rec1, pd, pd9, -val, 4, fld0d, fld1d);
			chkSetUnscaledLong(rec2, pd, pd9, val, 4);
			chkSetUnscaledLong(rec2, pd, pd9, -val, 4);
			val = val * 3;
		}
	};
	

	/**
	 * @param pd
	 * @param pd9
	 */
	private void tstGetPositive(TypeNum pd, ITypeBinaryExtendedNumeric pd9) {
		byte[] rec1 = {0, 0, 0, 0, 0};
		
		FieldDetail fld0d =  FieldDetail.newFixedWidthField("", Type.ftPackedDecimal, 1, 5, 0, "");
		FieldDetail fld1d =  FieldDetail.newFixedWidthField("", Type.ftPackedDecimal, 1, 5, 1, "");

		for (int i = 0; i < 1000; i++) {
			for (int j = 1; j < flds.length; j++) {
				chkAsUnscaledLong(rec1, pd, pd9, i, j, fld0d, fld1d);
			}
		}
	};


	/**
	 * @param pd
	 * @param pd9
	 */
	private void tstGetSetLarge(TypeNum pd, ITypeBinaryExtendedNumeric pd9, int type) {
		byte[] rec1 = {0, 0, 0, 0, 0, 0, 0, 0, 0};
		byte[] rec2 = {0, 0, 0, 0, 0, 0, 0, 0, 0};
		
		FieldDetail f = FieldDetail.newFixedWidthField(
				"", type, 1, 
				type == Type.ftPackedDecimal ? 9 : 8, 0, "");
		long inc = 1313131313131313L;
		long amt = inc;
		
		chkGet(rec1, f, pd, pd9, 4);
		chkGet(rec1, f, pd, pd9, 256);
		while (amt < 100000000000000000L) {
			chkGet(rec1, f, pd, pd9, amt);
			chkGet(rec1, f, pd, pd9, -amt);
			chkSet(rec2, f, pd, pd9, amt);
			chkSet(rec2, f, pd, pd9, -amt);
			
			amt += inc;
		}
	}

	/**
	 * @param rec1
	 * @param f
	 * @param pd
	 * @param pd9
	 * @param amt
	 */
	private void chkGet(byte[] rec1, FieldDetail f, TypeNum pd, ITypeBinaryExtendedNumeric pd9, long amt) {
		pd.setField(rec1, 1, f, amt);
		long l = pd9.asUnscaledLong(rec1, 1, f);
		assertEquals(amt, l);
	}
	
	private void chkSet(byte[] rec1, FieldDetail f, TypeNum pd, ITypeBinaryExtendedNumeric pd9, long amt) {
		pd9.setUnscaledLong(rec1, 1, f, amt);
		Object l = pd.getField(rec1, 1, f);
		assertEquals(Long.toString(amt), l.toString());
	}
	
	/**
	 * @param rec1
	 * @param pd
	 * @param pd9
	 * @param value
	 * @param fieldIdx
	 */
	private void chkAsUnscaledLong(
			byte[] rec1, Type pd, ITypeBinaryExtendedNumeric pd9, int value, int fieldIdx, 
			FieldDetail fld0Decimal, FieldDetail fld1Decimal) {
		int position = 5 - fieldIdx;
		
		String message = value + ", " + position;
		FieldDetail field = flds[fieldIdx];

		pd.setField(rec1, position, field, value);
		long l = pd9.asUnscaledLong(rec1, position, field);
		assertEquals(value + ", " + fieldIdx, value, l);
		assertEquals(Long.valueOf(l), pd9.getField(rec1, position, field));
		pd.setField(rec1, 1, fld0Decimal, value);
		assertEquals(message, BigDecimal.valueOf(value, 1), pd9.getField(rec1, 1, fld1Decimal));
	}
	
	private void chkSetUnscaledLong(byte[] rec1, Type pd, ITypeBinaryExtendedNumeric pd9, int amt, int fieldIdx) {
		FieldDetail f = flds[fieldIdx];
		pd9.setUnscaledLong(rec1, 1, f, amt);
		Object l = pd.getField(rec1, 1, f);
		assertEquals(Long.toString(amt), l.toString());


	}

}
