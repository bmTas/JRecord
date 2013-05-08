package net.sf.JRecord.zTest.Types;

import java.math.BigInteger;

import junit.framework.TestCase;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;

public class TstBinType2 extends TestCase {


	private static byte[] rec1 = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,};

	byte[] rec;

	private TypeManager typeManager = new TypeManager();

	private FieldDetail
	typePd4,  typePd8,  typePd12,
	typeBE1,  typeBE4,  typeBE8,  typeBE12,
	typeBEP1,  typeBEP4,  typeBEP8,  typeBEP12,
	typePBE1, typePBE4, typePBE8, typePBE12,
	typeLE1,  typeLE4,  typeLE8,  typeLE12,
	typeLEP1,  typeLEP4,  typeLEP8,  typeLEP12,
	typePLE1, typePLE4, typePLE8, typePLE12,
	typeHex4, typeHex8, typeHex12;

	protected void setUp() throws Exception {
		super.setUp();

		typePd4			= getType(2, 4, Type.ftPackedDecimal, 0);
		typePd8			= getType(2, 8, Type.ftPackedDecimal, 0);
		typePd12		= getType(2, 12, Type.ftPackedDecimal, 0);

		typeHex4			= getType(2, 4, Type.ftHex, 0);
		typeHex8			= getType(2, 8, Type.ftHex, 0);
		typeHex12		= getType(2, 12, Type.ftHex, 0);

		typeBE1			= getType(2, 1, Type.ftBinaryBigEndian, 0);
		//typeBE2			= getType(2, 2, Type.ftBinaryBigEndian, 0);
		typeBE4			= getType(2, 4, Type.ftBinaryBigEndian, 0);
		typeBE8			= getType(2, 8, Type.ftBinaryBigEndian, 0);
		typeBE12		= getType(2, 12, Type.ftBinaryBigEndian, 0);

		typeBEP1		= getType(2, 1, Type.ftBinaryBigEndianPositive, 0);
		typeBEP4		= getType(2, 4, Type.ftBinaryBigEndianPositive, 0);
		typeBEP8		= getType(2, 8, Type.ftBinaryBigEndianPositive, 0);
		typeBEP12		= getType(2, 12, Type.ftBinaryBigEndianPositive, 0);

		typePBE1		= getType(2, 1, Type.ftPositiveBinaryBigEndian, 0);
		//typePBE2		= getType(2, 2, Type.ftPositiveBinaryBigEndian, 0);
		typePBE4		= getType(2, 4, Type.ftPositiveBinaryBigEndian, 0);
		typePBE8		= getType(2, 8, Type.ftPositiveBinaryBigEndian, 0);
		typePBE12		= getType(2, 12, Type.ftPositiveBinaryBigEndian, 0);


		typeLE1			= getType(2, 1, Type.ftBinaryInt, 0);
		//typeLE2			= getType(2, 2, Type.ftBinaryInt, 0);
		typeLE4			= getType(2, 4, Type.ftBinaryInt, 0);
		typeLE8			= getType(2, 8, Type.ftBinaryInt, 0);
		typeLE12		= getType(2, 12, Type.ftBinaryInt, 0);

		typeLEP1			= getType(2, 1, Type.ftBinaryIntPositive, 0);
		typeLEP4			= getType(2, 4, Type.ftBinaryIntPositive, 0);
		typeLEP8			= getType(2, 8, Type.ftBinaryIntPositive, 0);
		typeLEP12		= getType(2, 12, Type.ftBinaryIntPositive, 0);

		typePLE1		= getType(2, 1, Type.ftPostiveBinaryInt, 0);
		//typePLE2		= getType(2, 2, Type.ftPostiveBinaryInt, 0);
		typePLE4		= getType(2, 4, Type.ftPostiveBinaryInt, 0);
		typePLE8		= getType(2, 8, Type.ftPostiveBinaryInt, 0);
		typePLE12		= getType(2, 12, Type.ftPostiveBinaryInt, 0);
	}

	public void testBE()  throws RecordException {
		byte[] res1a = {0, 127};
		byte[] res1b = {0, -127};
		byte[] res2a = {0, 127, -1, -1, -1};
		byte[] res2b = {0, -128, 0, 0, 1};
		byte[] res3a = {0, 127, -1, -1, -1, -1, -1, -1, -1};
		byte[] res3b = {0, -128, 0, 0, 0, 0, 0, 0, 1};
		byte[] res4a = {0, 0, 0, 0, 127, -1, -1, -1, -1, -1, -1, -1, 0};
		byte[] res4b = {0, -1, -1, -1, -128, 0, 0, 0, 0, 0, 0, 1, 0};

		checkCallBE(typeBE1, "127",  res1a, "BE 1 byte");
		checkCallBE(typeBE1, "-127", res1b, "BE 1 byte");
		checkCallBE(typeBE4, "" + Integer.MAX_VALUE,  res2a, "BE 4 byte");
		checkCallBE(typeBE4, "" + (-Integer.MAX_VALUE),  res2b, "BE 4 byte");
		checkCallBE(typeBE8, "" + Long.MAX_VALUE,  res3a, "BE 8 byte");
		checkCallBE(typeBE8, "" + (-Long.MAX_VALUE),  res3b, "BE 8 byte");
		checkCallBE(typeBE12, "" + BigInteger.valueOf(Long.MAX_VALUE).multiply(BigInteger.valueOf(256)),
				res4a, "BE 12 byte");
		checkCallBE(typeBE12, "" + BigInteger.valueOf(-Long.MAX_VALUE).multiply(BigInteger.valueOf(256)),
				res4b, "BE 12 byte");

		checkCallBE(typeBEP1, "127",  res1a, "BE 1 byte");
		checkCallBE(typeBEP4, "" + Integer.MAX_VALUE,  res2a, "BE 4 byte");
		checkCallBE(typeBEP8, "" + Long.MAX_VALUE,  res3a, "BE 8 byte");
		checkCallBE(typeBEP12, "" + BigInteger.valueOf(Long.MAX_VALUE).multiply(BigInteger.valueOf(256)),
				res4a, "BE 12 byte");

	}

	public void testPBE()  throws RecordException {
		byte[] res1a = {0, -1};
		byte[] res2a = {0, -1, -1, -1, -1};
		byte[] res3a = {0, -1, -1, -1, -1, -1, -1, -1, -1};
		byte[] res4a = {0, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -2, 0};
		Long l = (long) Integer.MAX_VALUE;
		BigInteger bi = BigInteger.valueOf(Long.MAX_VALUE)
							.multiply(BigInteger.valueOf(2))
							.add(BigInteger.valueOf(1));

		checkCallBE(typePBE1, "255",  res1a, "PBE 1 byte");
		checkCallBE(typePBE4, "" + (l * 2 + 1),  res2a, "PBE 4 byte");
		checkCallBE(typePBE8, "" + bi,  res3a, "BE 8 byte");
		checkCallBE(typePBE12, "" + BigInteger.valueOf(Long.MAX_VALUE).multiply(BigInteger.valueOf(512)),
				res4a, "BE 12 byte");
	}


	public void testLE()  throws RecordException {
		byte[] res1a = {0, 127};
		byte[] res1b = {0, -127};
		byte[] res2a = {0, 127, -1, -1, -1};
		byte[] res2b = {0, -128, 0, 0, 1};
		byte[] res3a = {0, 127, -1, -1, -1, -1, -1, -1, -1};
		byte[] res3b = {0, -128, 0, 0, 0, 0, 0, 0, 1};
		byte[] res4a = {0, 0, 0, 0, 127, -1, -1, -1, -1, -1, -1, -1, 0};
		byte[] res4b = {0, -1, -1, -1, -128, 0, 0, 0, 0, 0, 0, 1, 0};

		checkCallLE(typeLE1, "127",  res1a, "LE 1 byte");
		checkCallLE(typeLE1, "-127", res1b, "LE 1 byte");
		checkCallLE(typeLE4, "" + Integer.MAX_VALUE,  res2a, "LE 4 byte");
		checkCallLE(typeLE4, "" + (-Integer.MAX_VALUE),  res2b, "LE 4 byte");
		checkCallLE(typeLE8, "" + Long.MAX_VALUE,  res3a, "LE 8 byte");
		checkCallLE(typeLE8, "" + (-Long.MAX_VALUE),  res3b, "LE 8 byte");
		checkCallLE(typeLE12, "" + BigInteger.valueOf(Long.MAX_VALUE).multiply(BigInteger.valueOf(256)),
				res4a, "LE 12 byte");
		checkCallLE(typeLE12, "" + BigInteger.valueOf(-Long.MAX_VALUE).multiply(BigInteger.valueOf(256)),
				res4b, "LE 12 byte");

		checkCallLE(typeLEP1, "127",  res1a, "LE 1 byte");
		checkCallLE(typeLEP4, "" + Integer.MAX_VALUE,  res2a, "LE 4 byte");
		checkCallLE(typeLEP8, "" + Long.MAX_VALUE,  res3a, "LE 8 byte");
		checkCallLE(typeLEP12, "" + BigInteger.valueOf(Long.MAX_VALUE).multiply(BigInteger.valueOf(256)),
				res4a, "LE 12 byte");
	}

	public void testPLE()  throws RecordException {
		byte[] res1a = {0, -1};
		byte[] res2a = {0, -1, -1, -1, -1};
		byte[] res3a = {0, -1, -1, -1, -1, -1, -1, -1, -1};
		byte[] res4a = {0, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -2, 0};
		Long l = (long) Integer.MAX_VALUE;
		BigInteger bi = BigInteger.valueOf(Long.MAX_VALUE)
							.multiply(BigInteger.valueOf(2))
							.add(BigInteger.valueOf(1));

		checkCallLE(typePLE1, "255",  res1a, "PLE 1 byte");
		checkCallLE(typePLE4, "" + (l * 2 + 1),  res2a, "PLE 4 byte");
		checkCallLE(typePLE8, "" + bi,  res3a, "LE 8 byte");
		checkCallLE(typePLE12, "" + BigInteger.valueOf(Long.MAX_VALUE).multiply(BigInteger.valueOf(512)),
				res4a, "LE 12 byte");
	}


	public void testPD()  throws RecordException {

		checkCallPD(typePd4, "9876543", "009876543c", "PD 4 byte");
		checkCallPD(typePd4,  "-9876543", "009876543d", "PD 4 byte");

		checkCallPD(typePd8, "987654321098765", "00987654321098765c", "PD 8 byte");
		checkCallPD(typePd8,  "-987654321098765", "00987654321098765d", "PD 8 byte");

		checkCallPD(typePd12, "98765432109876543210987", "0098765432109876543210987c", "PD 12 byte");
		checkCallPD(typePd12,  "-98765432109876543210987", "0098765432109876543210987d", "PD 12 byte");

		checkCallPD(typePd4, "09876543", "009876543c", "PD 4 byte");
		checkCallPD(typePd4,  "-09876543", "009876543d", "PD 4 byte");

		checkCallPD(typePd8, "00987654321098765", "00987654321098765c", "PD 8 byte");
		checkCallPD(typePd8,  "-0987654321098765", "00987654321098765d", "PD 8 byte");

		checkCallPD(typePd12, "0098765432109876543210987", "0098765432109876543210987c", "PD 12 byte");
		checkCallPD(typePd12,  "-098765432109876543210987", "0098765432109876543210987d", "PD 12 byte");
	}

	public void testHex()  throws RecordException {

		checkCallPD(typeHex4, "9876543c", "009876543c", "PD 4 byte");
		checkCallPD(typeHex4, "9876543d", "009876543d", "PD 4 byte");

		checkCallPD(typeHex8, "987654321098765c", "00987654321098765c", "PD 8 byte");
		checkCallPD(typeHex8, "987654321098765d", "00987654321098765d", "PD 8 byte");

		checkCallPD(typeHex12, "98765432109876543210987c", "0098765432109876543210987c", "PD 12 byte");
		checkCallPD(typeHex12, "98765432109876543210987d", "0098765432109876543210987d", "PD 12 byte");
	}

	private void checkCallBE(FieldDetail type, String value, byte[] result, String name)  throws RecordException  {
		rec = rec1.clone();

		setFldValue(type, value);

		for (int i = 0; i < result.length; i++) {
			assertEquals(name + " " + value + ": " + i, result[i], rec[i]);
		}
	}


	private void checkCallLE(FieldDetail type, String value, byte[] result, String name)  throws RecordException  {
		rec = rec1.clone();

		setFldValue(type, value);

		assertEquals(name + " " + value + ": 0", result[0], rec[0]);
		for (int i = 1; i < result.length; i++) {
			assertEquals(name + " " + value + ": " + i, result[result.length - i], rec[i]);
		}
	}


	private void checkCallPD(FieldDetail type, String value, String result, String name)  throws RecordException  {
		rec = rec1.clone();

		setFldValue(type, value);

//		for (int i = 0; i < rec.length; i++) {
//			System.out.print("\t" + rec[i]);
//		}
//		System.out.println();
		assertEquals(name + " " + value, result, Conversion.getDecimal(rec, 0, type.getLen() + 1));

	}



	public void testBEsize()  throws RecordException {
		Long li = ((long) Integer.MAX_VALUE) + 1;
		BigInteger bip = BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.valueOf(1));
		BigInteger bin = BigInteger.valueOf(Long.MIN_VALUE).subtract(BigInteger.valueOf(1));
		checkSizeError(typeBE1, "128",  "BE 1 byte");
		checkSizeError(typeBE1, "-129", "BE 1 byte");
		checkSizeError(typeBE4, "" + li,  "BE 4 byte");
		checkSizeError(typeBE4, "" + (-li - 2),  "BE 4 byte");
		checkSizeError(typeBE8, bip,  "BE 8 byte");
		checkSizeError(typeBE8, bin,  "BE 8 byte");
		checkSizeError(typeBE12, BigInteger.valueOf(li*2).multiply(bip),  "BE 12 byte");
		checkSizeError(typeBE12, BigInteger.valueOf(li*2).multiply(bin),  "BE 12 byte");

		checkSizeError(typeBEP1, "128",  "BE 1 byte");
		checkSizeError(typeBEP4, "" + li,  "BE 4 byte");
		checkSizeError(typeBEP8, bip,  "BE 8 byte");
		checkSizeError(typeBEP12, BigInteger.valueOf(li*2).multiply(bip),  "BE 12 byte");
	}



	public void testLEsize()  throws RecordException {
		Long li = ((long) Integer.MAX_VALUE) + 1;
		BigInteger bip = BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.valueOf(1));
		BigInteger bin = BigInteger.valueOf(Long.MIN_VALUE).subtract(BigInteger.valueOf(1));
		checkSizeError(typeLE1, "128",  "BE 1 byte");
		checkSizeError(typeLE1, "-129", "BE 1 byte");
		checkSizeError(typeLE4, "" + li,  "BE 4 byte");
		checkSizeError(typeLE4, "" + (-li - 2),  "BE 4 byte");
		checkSizeError(typeLE8, bip,  "BE 8 byte");
		checkSizeError(typeLE8, bin,  "BE 8 byte");
		checkSizeError(typeLE12, BigInteger.valueOf(li*2).multiply(bip),  "BE 12 byte");
		checkSizeError(typeLE12, BigInteger.valueOf(li*2).multiply(bin),  "BE 12 byte");

		checkSizeError(typeLEP1, "128",  "BE 1 byte");
		checkSizeError(typeLEP4, "" + li,  "BE 4 byte");
		checkSizeError(typeLEP8, bip,  "BE 8 byte");
		checkSizeError(typeLEP12, BigInteger.valueOf(li*2).multiply(bip),  "BE 12 byte");

	}


	public void testPDsize()  throws RecordException {

		checkSizeError(typePd4, "19876543", "PD 4 byte");
		checkSizeError(typePd4,  "-19876543",  "PD 4 byte");

		checkSizeError(typePd8, "1987654321098765",  "PD 8 byte");
		checkSizeError(typePd8,  "-1987654321098765",  "PD 8 byte");

		checkSizeError(typePd12, "198765432109876543210987",  "PD 12 byte");
		checkSizeError(typePd12,  "-198765432109876543210987", "PD 12 byte");
	}

    /**
     * Test setting a field with a value that is to long
     *
     * @param fld field definition to use
     * @param value value to assign to the supplied field
     * @param msg error message to use if size error is not thrown
     */
    private void checkSizeError( FieldDetail fld, Object value, String msg) {

        try {
            setFldValue(fld, value);

            System.out.println("::> " + msg + " " + value + " :: " + getFldValue(fld));
            throw new AssertionError("Size Error: " + msg + " " + value + " :: " + getFldValue(fld));
        } catch (RecordException e) {
        }
    }



    /**
     * Get field value
     * @param fld field definition
     * @return value of the field
     */
    private String getFldValue(FieldDetail fld) {
        return typeManager.getType(fld.getType()).getField(rec, fld.getPos(), fld).toString();
    }



	/**
	 * Set Fields value
	 * @param fld field definition
	 * @param val value to assign
	 * @throws RecordException any error that occurs
	 */
	private void setFldValue(FieldDetail fld, Object val) throws RecordException {
		typeManager.getType(fld.getType()).setField(rec, fld.getPos(), fld, val);
	}

	private FieldDetail getType(int pos, int len, int type, int decimal) {

		FieldDetail field = new FieldDetail("", "", type, decimal, "", -1, "");

		field.setPosLen(pos, len);

		return field;
	}

}
