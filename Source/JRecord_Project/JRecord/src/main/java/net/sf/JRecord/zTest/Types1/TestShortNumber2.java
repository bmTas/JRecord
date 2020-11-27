package net.sf.JRecord.zTest.Types1;

import java.math.BigDecimal;
import java.util.Random;

import junit.framework.TestCase;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeBinBigEndian;
import net.sf.JRecord.Types.TypeBinLittleEndian;
import net.sf.JRecord.Types.TypeNum;
import net.sf.JRecord.Types.TypePackedDecimal;
import net.sf.JRecord.Types.TypeZoned;
import net.sf.JRecord.Types.smallBin.ITypeBinaryExtendedNumeric;
import net.sf.JRecord.Types.smallBin.TypeIntBigEndian;
import net.sf.JRecord.Types.smallBin.TypeIntLittleEndian;
import net.sf.JRecord.Types.smallBin.TypePackedDecimal9;
import net.sf.JRecord.Types.smallBin.TypeZonedAsciiSmall;
import net.sf.JRecord.Types.smallBin.TypeZonedEbcdicSmall;

public class TestShortNumber2 extends TestCase{

	char[] positiveSign = {'{', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'};

	char[] negativeSign = {'}', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',};

	public void testPd() {
		chkNewField(new TypePackedDecimal(), new TypePackedDecimal9(), 6, 123);
	}

	public void testPdP() {
		chkNewField(new TypePackedDecimal(true), new TypePackedDecimal9(true), 6, 456);
	}

	public void testIntBE() {
		chkNewField(new TypeBinBigEndian(false), new TypeIntBigEndian(false, false), 8, 531);
	}

	public void testIntBEp1() {
		chkNewField(new TypeBinBigEndian(true), new TypeIntBigEndian(true, false), 8, 5312);
	}

	public void testIntBEp2() {
		chkNewField(new TypeBinBigEndian(true, true), new TypeIntBigEndian(true, true), 8, 5315);
	}

	public void testIntLE() {
		chkNewField(new TypeBinLittleEndian(false), new TypeIntLittleEndian(false, false), 8, 33557711);
	}

	public void testIntLEp1() {
		chkNewField(new TypeBinLittleEndian(true), new TypeIntLittleEndian(true, false), 8, 335711);
	}

	public void testIntLEp2() {
		chkNewField(new TypeBinLittleEndian(true, true), new TypeIntLittleEndian(true, true), 8, 335571);
	}

	public void testZdAscii() {
		chkNewField(new TypeZoned(),  new TypeZonedAsciiSmall(false),       2, 335571, "");
		chkNewField8(new TypeZoned(), new TypeZonedAsciiSmall(false),    1000,   3312, 16, "");
		chkNewField8(new TypeZoned(), new TypeZonedAsciiSmall(false),    1000,   1122, 16, "");
		chkNewField8(new TypeZoned(), new TypeZonedAsciiSmall(false), 1000000,    456, 16, "");
		chkNewField8(new TypeZoned(), new TypeZonedAsciiSmall(false), 1000000,    789, 16, "");
	}
	

	public void testZdAsciiPositive() {
		chkNewField(new TypeZoned(true),  new TypeZonedAsciiSmall(true),       2, 335571, "");
		chkNewField8(new TypeZoned(true), new TypeZonedAsciiSmall(true),    1000,   3312, 16, "");
		chkNewField8(new TypeZoned(true), new TypeZonedAsciiSmall(true),    1000,   1122, 16, "");
		chkNewField8(new TypeZoned(true), new TypeZonedAsciiSmall(true), 1000000,    456, 16, "");
		chkNewField8(new TypeZoned(true), new TypeZonedAsciiSmall(true), 1000000,    789, 16, "");
	}

	public void testZdEbcdic() {
		chkNewField(new TypeZoned(),  new TypeZonedEbcdicSmall(false), 2, 335571, "cp037");
		chkNewField8(new TypeZoned(), new TypeZonedEbcdicSmall(false), 1000, 3312, 16, "cp037");
		chkNewField8(new TypeZoned(), new TypeZonedEbcdicSmall(false), 1000, 1122, 16, "cp037");
		chkNewField8(new TypeZoned(), new TypeZonedEbcdicSmall(false), 1000000, 456, 16, "cp037");
		chkNewField8(new TypeZoned(), new TypeZonedEbcdicSmall(false), 1000000, 789, 16, "cp037");
	}

	public void testZdEbcdicPositive() {
		chkNewField(new TypeZoned(true),  new TypeZonedEbcdicSmall(true), 2, 335571, "cp037");
		chkNewField8(new TypeZoned(true), new TypeZonedEbcdicSmall(true), 1000, 3312, 16, "cp037");
		chkNewField8(new TypeZoned(true), new TypeZonedEbcdicSmall(true), 1000, 1122, 16, "cp037");
		chkNewField8(new TypeZoned(true), new TypeZonedEbcdicSmall(true), 1000000, 456, 16, "cp037");
		chkNewField8(new TypeZoned(true), new TypeZonedEbcdicSmall(true), 1000000, 789, 16, "cp037");
	}
	
	public void testPdLarge() {
		chkNewField8(new TypePackedDecimal(), new TypePackedDecimal9(), 1000, 1347, 9);
	}

	
	public void testIntBeLarge() {
		chkNewField8(new TypeBinBigEndian(false), new TypeIntBigEndian(false, false), 1000, 1347, 8);
	}

	
	public void testIntLeLarge() {
		chkNewField8(new TypeBinLittleEndian(false), new TypeIntLittleEndian(false, false), 1000, 1347, 8);
	}


	public void testShortZdUnsigned() {
		@SuppressWarnings("deprecation")
		FieldDetail field = FieldDetail.newFixedWidthField("", Type.ftZonedAsciiSmall, 1, 2, 0, "");
		TypeZonedAsciiSmall typeAsciiZd = new TypeZonedAsciiSmall(false);
		TypeZonedAsciiSmall typeAsciiZdP = new TypeZonedAsciiSmall(true);
		TypeZonedEbcdicSmall typeEbcdicZd = new TypeZonedEbcdicSmall(false);
		TypeZonedEbcdicSmall typeEbcdicZdP = new TypeZonedEbcdicSmall(true);
		
		for (int i = 0; i < 10; i++) {
			String str = "0" + (char) ('0' + i);
			assertEquals(i, typeAsciiZd.asUnscaledLong(Conversion.getBytes(str, ""), 1, field));
			assertEquals(i, typeAsciiZdP.asUnscaledLong(Conversion.getBytes(str, ""), 1, field));
			assertEquals(i, typeEbcdicZd.asUnscaledLong(Conversion.getBytes(str, "cp037"), 1, field));
			assertEquals(i, typeEbcdicZdP.asUnscaledLong(Conversion.getBytes(str, "cp037"), 1, field));

			assertEquals("" + i, typeAsciiZd.getField(Conversion.getBytes(str, ""), 1, field).toString());
			assertEquals("" + i, typeAsciiZdP.getField(Conversion.getBytes(str, ""), 1, field).toString());
			assertEquals("" + i, typeEbcdicZd.getField(Conversion.getBytes(str, "cp037"), 1, field).toString());
			assertEquals("" + i, typeEbcdicZdP.getField(Conversion.getBytes(str, "cp037"), 1, field).toString());
		}
		
		for (int i = 0; i < 10; i++) {
			String str = "1" + (char) ('0' + i);
			assertEquals(10 + i, typeAsciiZd.asUnscaledLong(Conversion.getBytes(str, ""), 1, field));
			assertEquals(10 + i, typeEbcdicZd.asUnscaledLong(Conversion.getBytes(str, "cp037"), 1, field));
			assertEquals(10 + i, typeAsciiZdP.asUnscaledLong(Conversion.getBytes(str, ""), 1, field));
			assertEquals(10 + i, typeEbcdicZdP.asUnscaledLong(Conversion.getBytes(str, "cp037"), 1, field));
			
			assertEquals("1" + i, typeAsciiZd.getField(Conversion.getBytes(str, ""), 1, field).toString());
			assertEquals("1" + i, typeEbcdicZd.getField(Conversion.getBytes(str, "cp037"), 1, field).toString());
			assertEquals("1" + i, typeAsciiZdP.getField(Conversion.getBytes(str, ""), 1, field).toString());
			assertEquals("1" + i, typeEbcdicZdP.getField(Conversion.getBytes(str, "cp037"), 1, field).toString());
		}
	}


	public void testShortZdUnsignedSet() {
		@SuppressWarnings("deprecation")
		FieldDetail field = FieldDetail.newFixedWidthField("", Type.ftZonedAsciiSmall, 1, 2, 0, "");
		FieldDetail fielde = FieldDetail.newFixedWidthField("", Type.ftZonedAsciiSmall, 1, 2, 0, "cp037");
		TypeZonedAsciiSmall  typeAsciiZd   = new TypeZonedAsciiSmall(false);
		TypeZonedAsciiSmall  typeAsciiZdP  = new TypeZonedAsciiSmall(true);
		TypeZonedEbcdicSmall typeEbcdicZd  = new TypeZonedEbcdicSmall(false);
		TypeZonedEbcdicSmall typeEbcdicZdP = new TypeZonedEbcdicSmall(true);
		
		for (int i = 0; i < 100; i++) {
			String strS = new StringBuilder().append(i / 10).append(positiveSign[i % 10]).toString();
			String strN = new StringBuilder().append(i / 10).append(negativeSign[i % 10]).toString();
			String strP = new StringBuilder().append(i / 10).append(i % 10).toString();
			
			String value = "" + i;

			assertEquals(strP, Conversion.toString(typeAsciiZdP.setUnscaledLong(new byte[2], 1, field, i), ""));
			assertEquals(strP, Conversion.toString(typeEbcdicZdP.setUnscaledLong(new byte[2], 1, fielde, i), "cp037"));
			assertEquals(strS, Conversion.toString(typeAsciiZd.setUnscaledLong(new byte[2], 1, field, i), ""));
			assertEquals(strS, Conversion.toString(typeEbcdicZd.setUnscaledLong(new byte[2], 1, fielde, i), "cp037"));
			if (i > 0) {
				assertEquals(strN, Conversion.toString(typeAsciiZd.setUnscaledLong(new byte[2], 1, field, -i), ""));
				assertEquals(strN, Conversion.toString(typeEbcdicZd.setUnscaledLong(new byte[2], 1, fielde, -i), "cp037"));
			}
			
			assertEquals(strP, Conversion.toString(typeAsciiZdP.setField(new byte[2], 1, field, i), ""));
			assertEquals(strP, Conversion.toString(typeEbcdicZdP.setField(new byte[2], 1, fielde, i), "cp037"));
			assertEquals(strS, Conversion.toString(typeAsciiZd.setField(new byte[2], 1, field, i), ""));
			assertEquals(strS, Conversion.toString(typeEbcdicZd.setField(new byte[2], 1, fielde, i), "cp037"));
			
			assertEquals(strP, Conversion.toString(typeAsciiZdP.setField(new byte[2], 1, field, value), ""));
			assertEquals(value, strP, Conversion.toString(typeEbcdicZdP.setField(new byte[2], 1, fielde, value), "cp037"));
			assertEquals(strS, Conversion.toString(typeAsciiZd.setField(new byte[2], 1, field, value), ""));
			assertEquals(strS, Conversion.toString(typeEbcdicZd.setField(new byte[2], 1, fielde, value), "cp037"));
			if (i > 0) {
				assertEquals(strN, Conversion.toString( typeAsciiZd.setField(new byte[2], 1, field, -i), ""));
				assertEquals(strN, Conversion.toString(typeEbcdicZd.setField(new byte[2], 1, fielde, -i), "cp037"));
				assertEquals(strN, Conversion.toString( typeAsciiZd.setField(new byte[2], 1, field, '-' + value), ""));
				assertEquals(strN, Conversion.toString(typeEbcdicZd.setField(new byte[2], 1, fielde, '-' + value), "cp037"));
			}
		}
		
	}

	public void testShortZdSigned() {
		@SuppressWarnings("deprecation")
		FieldDetail field = FieldDetail.newFixedWidthField("", Type.ftZonedAsciiSmall, 1, 2, 0, "");
		TypeZonedAsciiSmall typeAsciiZd = new TypeZonedAsciiSmall(false);
		TypeZonedEbcdicSmall typeEbcdicZd = new TypeZonedEbcdicSmall(false);
		TypeZonedAsciiSmall typeAsciiZdP = new TypeZonedAsciiSmall(true);
		String str;
		
		for (int i = 0; i < 10; i++) {
			str = "0" + positiveSign[i];
			assertEquals(i, typeAsciiZd.asUnscaledLong(Conversion.getBytes(str, ""), 1, field));
			assertEquals(i, typeAsciiZdP.asUnscaledLong(Conversion.getBytes("0" + i, ""), 1, field));
			assertEquals(i, typeEbcdicZd.asUnscaledLong(Conversion.getBytes(str, "cp037"), 1, field));

			str = "0" + negativeSign[i];
			assertEquals(-i, typeAsciiZd.asUnscaledLong(Conversion.getBytes(str, ""), 1, field));
			assertEquals(-i, typeEbcdicZd.asUnscaledLong(Conversion.getBytes(str, "cp037"), 1, field));
		}
		
		
		for (int i = 0; i < 10; i++) {
			str = "1" + positiveSign[i];
			assertEquals(10 + i, typeAsciiZd.asUnscaledLong(Conversion.getBytes(str, ""), 1, field));
			assertEquals(10 + i, typeEbcdicZd.asUnscaledLong(Conversion.getBytes(str, "cp037"), 1, field));

			str = "1" + negativeSign[i];
			assertEquals(-(10 + i), typeAsciiZd.asUnscaledLong(Conversion.getBytes(str, ""), 1, field));
			assertEquals(-(10 + i), typeEbcdicZd.asUnscaledLong(Conversion.getBytes(str, "cp037"), 1, field));
		}

	}

	/**
	 * @param typeN
	 * @param typeB
	 */
	private void chkNewField(TypeNum typeN, ITypeBinaryExtendedNumeric typeB, int max, int seed) {
		chkNewField(typeN, typeB, max, seed, "");
	}
	
	/**
	 * @param typeN
	 * @param typeB
	 */
	private void chkNewField(TypeNum typeN, ITypeBinaryExtendedNumeric typeB, int max, int seed, String charset) {
		Random r = new Random(123);	
		
		//System.out.println();
		
		byte[] record = {0, 0, 0, 0};
		FieldDetail field = FieldDetail.newFixedWidthField("", typeN.getFieldType(), 1, 4, 2, charset);
		
		for (int i = 0; i < 100; i++) {
			chkOneGetField(i + ": ", record, typeN, typeB, field, i);
		}
		
		for (int i = 0; i < 100; i++) {
			chkOneSetField(i + ": ", record, typeN, typeB, field,  ((double) i) / 100, i);
		}

		
		int n = 1;
		for (int i = 0; i < max; i++) {
			chkGetField1(r, n, typeN, typeB, charset);
			
			n = n * 10;
		}
		
		n = 1;
		for (int i = 0; i < max; i++) {
			chkGetField2(r, n, typeN, typeB, charset);
			
			n = n * 10;
		}
		
		n = 1;
		for (int i = 0; i < max; i++) {
			chkSetField1(r, n, typeN, typeB, charset);
			
			n = n * 10;
		}
		
		n = 1;
		for (int i = 0; i < max; i++) {
			chkSetField2(r, n, typeN, typeB, charset);
			
			n = n * 10;
		}
	}
	
	/**
	 * @param typeN
	 * @param typeB
	 */
	private void chkNewField8(TypeNum typeN, ITypeBinaryExtendedNumeric typeB, int start, int seed, int size) {
		chkNewField8(typeN, typeB, start, seed, size, "");
	}
	
	/**
	 * @param typeN
	 * @param typeB
	 */
	private void chkNewField8(TypeNum typeN, ITypeBinaryExtendedNumeric typeB, int start, int seed, int size, String charset) {
		Random r = new Random(seed);	
		
		//System.out.println();
		
		int n = start;
		for (int i = 0; i < 7; i++) {
			chkGetField3(r, n, typeN, typeB, size, charset);
			
			n = n * 10;
		}
		
		n = start;
		for (int i = 0; i < 7; i++) {
			chkSetField3(r, n, typeN, typeB, size, charset);
			
			n = n * 10;
		}
	}

	private void chkGetField1(Random r, int randomScale, TypeNum typeNum, ITypeBinaryExtendedNumeric typeSB, String charset) {
		byte[] record = {0, 0, 0, 0};
		FieldDetail field = FieldDetail.newFixedWidthField("", typeNum.getFieldType(), 1, 4, 2, charset);
		
		for (int i = 0; i < 500; i++) {
			Double d = r.nextDouble() * randomScale;
			long unscaledValue = (long) (d * 100);

			chkOneGetField(randomScale + ": " + i, record, typeNum, typeSB, field, unscaledValue);
		}
	}

	/**
	 * @param id
	 * @param record
	 * @param typeNum
	 * @param typeSB
	 * @param field
	 * @param unscaledValue
	 */
	private void chkOneGetField(String id, byte[] record, TypeNum typeNum, ITypeBinaryExtendedNumeric typeSB,
			FieldDetail field, long unscaledValue) {
		BigDecimal bd = BigDecimal.valueOf(unscaledValue, 2);
		BigDecimal bdm = BigDecimal.valueOf(-unscaledValue, 2);

		typeNum.setField(record, 1, field, bd);
		assertEquals(id, unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
		assertEquals(bd, typeSB.getField(record, field.getPos(), field));

		if (! (typeSB instanceof TypeNum) || ! ((TypeNum) typeSB).isPositive()) {
			typeNum.setField(record, 1, field, bdm);
			assertEquals(-unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
			assertEquals(bdm, typeSB.getField(record, field.getPos(), field));
		}
	}

	
	private void chkGetField2(Random r, int randomScale, TypeNum typeNum, ITypeBinaryExtendedNumeric typeSB, String charset) {
		byte[] record = {0, 0, 0, 0};
		FieldDetail field = FieldDetail.newFixedWidthField("", typeNum.getFieldType(), 1, 4, 0, charset);
		
		for (int i = 0; i < 500; i++) {
			Double d = r.nextDouble() * randomScale;
			long unscaledValue = (long) (d * 1);
			Long l = unscaledValue;
			Long lm = -unscaledValue;
			
			typeNum.setField(record, 1, field, l);
			assertEquals(unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
			assertEquals(l, typeSB.getField(record, field.getPos(), field));

			if (! (typeSB instanceof TypeNum) || ! ((TypeNum) typeSB).isPositive()) {
				typeNum.setField(record, 1, field, -unscaledValue);
				assertEquals(-unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
				assertEquals(lm, typeSB.getField(record, field.getPos(), field));
			}
		}
	}

	
	private void chkGetField3(Random r, int randomScale, TypeNum typeNum, ITypeBinaryExtendedNumeric typeSB, int size, String charset) {
		byte[] record = new byte[size];
		int decimal = 3;
		FieldDetail field = FieldDetail.newFixedWidthField("", typeNum.getFieldType(), 1, size, decimal, charset);
		
		for (int i = 0; i < 300; i++) {
			long unscaledValue = Math.abs(r.nextLong() / randomScale);
			BigDecimal bd = BigDecimal.valueOf(unscaledValue, decimal);
			BigDecimal bdm = BigDecimal.valueOf(-unscaledValue, decimal);

			typeNum.setField(record, 1, field, bd);
			assertEquals(randomScale + ": " + i, unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
			assertEquals(bd, typeSB.getField(record, field.getPos(), field));

			if (! (typeSB instanceof TypeNum) || ! ((TypeNum) typeSB).isPositive()) {
				typeNum.setField(record, 1, field, bdm);
				assertEquals(-unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
				assertEquals(bdm, typeSB.getField(record, field.getPos(), field));
			}
		}
	}

	
	private void chkSetField1(Random r, int randomScale, TypeNum typeNum, ITypeBinaryExtendedNumeric typeSB, String charset) {
		byte[] record = {0, 0, 0, 0};
		FieldDetail field = FieldDetail.newFixedWidthField("", typeNum.getFieldType(), 1, 4, 2, charset);
		
		for (int i = 0; i < 500; i++) {
			double d = r.nextDouble() * randomScale;
			long unscaledValue = (long) (d * 100 + 0.5);
			String id = i + " " + BigDecimal.valueOf(unscaledValue, 2) + " " + d;

			chkOneSetField(id, record, typeNum, typeSB, field, d, unscaledValue);
		}
		
		
		for (int i = 0; i < 500; i++) {
			float f = r.nextFloat() * randomScale;
			long unscaledValue = (long) (f * 100 + 0.5);
			BigDecimal bd = BigDecimal.valueOf(unscaledValue, 2);
			BigDecimal bdm = BigDecimal.valueOf(-unscaledValue, 2);
			String s = bd.toString();
			//System.out.print("\t" + bd);


			typeSB.setField(record, 1, field, f);
			assertEquals(i + " " + bd + " " + f, unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
			assertEquals(s, typeNum.getField(record, field.getPos(), field));

			typeSB.setField(record, 1, field, s);
			assertEquals(i + " ", unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
			assertEquals(s, typeNum.getField(record, field.getPos(), field));
			
			// -----------------------------
			
			if (! (typeSB instanceof TypeNum) || ! ((TypeNum) typeSB).isPositive()) {
				s = bdm.toString();
				typeSB.setField(record, 1, field, bdm);
				assertEquals(-unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
				assertEquals(s, typeNum.getField(record, field.getPos(), field));
	
				typeSB.setField(record, 1, field, -f);
				assertEquals(i + " " + bd + " " + f, -unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
				assertEquals(s, typeNum.getField(record, field.getPos(), field));
			}
		}

	}

	/**
	 * @param id
	 * @param record
	 * @param typeNum
	 * @param typeSB
	 * @param field
	 * @param d
	 * @param unscaledValue
	 */
	private void chkOneSetField(String id, byte[] record, TypeNum typeNum, ITypeBinaryExtendedNumeric typeSB,
			FieldDetail field, double d, long unscaledValue) {
		BigDecimal bd = BigDecimal.valueOf(unscaledValue, 2);
		BigDecimal bdm = BigDecimal.valueOf(-unscaledValue, 2);
		String s = bd.toString();


		typeSB.setField(record, 1, field, bd);
		//System.out.print("\t" + s + " " + new String(record));
		assertEquals(unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
		assertEquals(s, typeNum.getField(record, field.getPos(), field));

		typeSB.setField(record, 1, field, d);
		assertEquals(id, unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
		assertEquals(s, typeNum.getField(record, field.getPos(), field));

		typeSB.setField(record, 1, field, s);
		assertEquals(id, unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
		assertEquals(s, typeNum.getField(record, field.getPos(), field));
		
			// -----------------------------
		
		if (! (typeSB instanceof TypeNum) || ! ((TypeNum) typeSB).isPositive()) {
			s = bdm.toString();
			typeSB.setField(record, 1, field, bdm);
			assertEquals(-unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
			assertEquals(s, typeNum.getField(record, field.getPos(), field));

			typeSB.setField(record, 1, field, -d);
			assertEquals(id, -unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
			assertEquals(s, typeNum.getField(record, field.getPos(), field));

			typeSB.setField(record, 1, field, s);
			assertEquals(id, -unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
			assertEquals(s, typeNum.getField(record, field.getPos(), field));
		}
	}
	
	private void chkSetField2(Random r, int randomScale, TypeNum typeNum, ITypeBinaryExtendedNumeric typeSB, String charset) {
		byte[] record = {0, 0, 0, 0};
		FieldDetail field = FieldDetail.newFixedWidthField("", typeNum.getFieldType(), 1, 4, 0, charset);
		
		for (int i = 0; i < 500; i++) {
			double d = r.nextDouble() * randomScale;
			long unscaledValue = (long) (d + 0.5);
			BigDecimal bd = BigDecimal.valueOf(unscaledValue, 0);
			BigDecimal bdm = BigDecimal.valueOf(-unscaledValue, 0);
			String s = bd.toString();
			//d = unscaledValue;


			typeSB.setField(record, 1, field, bd);
			assertEquals(unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
			assertEquals(s, typeNum.getField(record, field.getPos(), field));

			typeSB.setField(record, 1, field, d);
			assertEquals(
					typeSB.getClass().getSimpleName() + " " +randomScale + ": " + i + " " + bd + " " + d,
					unscaledValue, 
					typeSB.asUnscaledLong(record, field.getPos(), field));
			assertEquals(s, typeNum.getField(record, field.getPos(), field));

			typeSB.setField(record, 1, field, s);
			assertEquals(i + " ", unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
			assertEquals(s, typeNum.getField(record, field.getPos(), field));
			
				// -----------------------------
			
			if (! (typeSB instanceof TypeNum) || ! ((TypeNum) typeSB).isPositive()) {
				s = bdm.toString();
				typeSB.setField(record, 1, field, bdm);
				assertEquals(-unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
				assertEquals(s, typeNum.getField(record, field.getPos(), field));
	
				typeSB.setField(record, 1, field, -d);
				assertEquals(i + " " + bd + " " + d, -unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
				assertEquals(s, typeNum.getField(record, field.getPos(), field));
	
				typeSB.setField(record, 1, field, s);
				assertEquals(i + " ", -unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
				assertEquals(s, typeNum.getField(record, field.getPos(), field));
			}
		}
		
		
		for (int i = 0; i < 500; i++) {
			float f = r.nextFloat() * randomScale;
			long unscaledValue = (long) (f + 0.5);
			BigDecimal bd = BigDecimal.valueOf(unscaledValue, 0);
			BigDecimal bdm = BigDecimal.valueOf(-unscaledValue, 0);
			String s = bd.toString();
			//f = unscaledValue;
			//System.out.print("\t" + bd);


			typeSB.setField(record, 1, field, f);
			assertEquals(i + " " + bd + " " + f, unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
			assertEquals(s, typeNum.getField(record, field.getPos(), field));

			typeSB.setField(record, 1, field, s);
			assertEquals(i + " ", unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
			assertEquals(s, typeNum.getField(record, field.getPos(), field));
			
			// -----------------------------
			
			if (! (typeSB instanceof TypeNum) || ! ((TypeNum) typeSB).isPositive()) {
				s = bdm.toString();
				typeSB.setField(record, 1, field, bdm);
				assertEquals(-unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
				assertEquals(s, typeNum.getField(record, field.getPos(), field));
	
				typeSB.setField(record, 1, field, -f);
				assertEquals(i + " " + bd + " " + f, -unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
				assertEquals(s, typeNum.getField(record, field.getPos(), field));
			}
		}
	}

	
	private void chkSetField3(Random r, int randomScale, TypeNum typeNum, ITypeBinaryExtendedNumeric typeSB, int size, String charset) {
		byte[] record = new byte[size];
		int decimal = 3;
		FieldDetail field = FieldDetail.newFixedWidthField("", typeNum.getFieldType(), 1, size, decimal, charset);
		
		for (int i = 0; i < 250; i++) {
			double d = r.nextDouble() * randomScale;
			long unscaledValue = Math.abs((long) (d * 100 + 0.5));
			BigDecimal bd = BigDecimal.valueOf(unscaledValue, decimal);
			BigDecimal bdm = BigDecimal.valueOf(-unscaledValue, decimal);
			String s = bd.toString();


			typeSB.setField(record, 1, field, bd);
			assertEquals(unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
			assertEquals(s, typeNum.getField(record, field.getPos(), field));

			typeSB.setField(record, 1, field, s);
			assertEquals(i + " ", unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
			assertEquals(s, typeNum.getField(record, field.getPos(), field));
			
				// -----------------------------
			
			if (! (typeSB instanceof TypeNum) || ! ((TypeNum) typeSB).isPositive()) {
				s = bdm.toString();
				typeSB.setField(record, 1, field, bdm);
				assertEquals(-unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
				assertEquals(s, typeNum.getField(record, field.getPos(), field));
	
				typeSB.setField(record, 1, field, s);
				assertEquals(i + " ", -unscaledValue, typeSB.asUnscaledLong(record, field.getPos(), field));
				assertEquals(s, typeNum.getField(record, field.getPos(), field));
			}
		}

	}
	

}
