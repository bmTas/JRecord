package net.sf.JRecord.zTest.Types;

import java.math.BigDecimal;
import java.util.HashSet;
import java.util.Random;

import junit.framework.TestCase;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;

public class TstTypesGeneral extends TestCase {

	public static final int[] NUMERIC_TYPES = {

	 	Type.ftNumLeftJustified,
	 	Type.ftNumRightJustified,
	 	Type.ftNumRightJustifiedPN,
	 	Type.ftNumRightJustCommaDp,
	 	Type.ftNumRightJustCommaDpPN,
	 	Type.ftNumZeroPadded,
	 	Type.ftNumZeroPaddedPN,
	 	Type.ftNumZeroPaddedPositive,
	 	Type.ftNumCommaDecimal,
	 	Type.ftNumCommaDecimalPN,
	 	Type.ftNumCommaDecimalPositive,

	 	Type.ftAssumedDecimal,
	 	Type.ftAssumedDecimalPositive,
	 	Type.ftSignSeparateLead,
	 	Type.ftSignSeparateTrail,
	 	Type.ftDecimal,
	 	Type.ftBinaryInt,
	 	Type.ftPostiveBinaryInt,
	 	Type.ftBinaryIntPositive,
	 	Type.ftFloat,
	 	Type.ftDouble,
	 	Type.ftNumAnyDecimal,
	 	Type.ftPositiveNumAnyDecimal,
	 	Type.ftBit,

	 	Type.ftPackedDecimal,
	 	Type.ftPackedDecimalPostive,
	 	Type.ftZonedNumeric,
	 	Type.ftBinaryBigEndian,
	 	Type.ftBinaryBigEndianPositive,
	 	Type.ftPositiveBinaryBigEndian,
	 	Type.ftRmComp,
	 	Type.ftRmCompPositive,

	 	Type.ftFjZonedNumeric,
	};

	public static final int[] BINARY_TYPES = {
		Type.ftHex,
		Type.ftDecimal,
		Type.ftBinaryInt,
		Type.ftPostiveBinaryInt,
	 	Type.ftBinaryIntPositive,

		Type.ftFloat,
		Type.ftDouble,

		Type.ftBit,

	 	Type.ftPackedDecimal,
	 	Type.ftPackedDecimalPostive,
	 	Type.ftBinaryBigEndian,
	 	Type.ftBinaryBigEndianPositive,
	 	Type.ftPositiveBinaryBigEndian,
	 	Type.ftRmComp,
	 	Type.ftRmCompPositive,

		Type.ftCharRestOfRecord
	};

	public static final int[] POSITIVE_TYPES = {
		Type.ftPostiveBinaryInt,
	 	Type.ftBinaryIntPositive,

		Type.ftAssumedDecimalPositive,
	 	Type.ftPackedDecimalPostive,
	 	Type.ftPositiveNumAnyDecimal,
	 	Type.ftBinaryBigEndianPositive,
	 	Type.ftPositiveBinaryBigEndian,
	 	Type.ftRmCompPositive,
	};

	private TypeManager typeManager = new TypeManager();
	private byte[] bytes = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


	public void testNumeric() {
		TypeManager m = TypeManager.getInstance();
		HashSet<Integer> numTypes = new HashSet<Integer>(NUMERIC_TYPES.length + 25);
		HashSet<Integer> binTypes = new HashSet<Integer>(BINARY_TYPES.length + 25);

		for (int i = 0; i < 200; i++) {
			if (m.getType(i).isBinary()) {
				System.out.print("  " + i);
			}
		}

		for (int typeId : NUMERIC_TYPES) {
			assertTrue("Numeric Type: " + typeId, m.getType(typeId).isNumeric());
			numTypes.add(typeId);
		}

		for (int i = 0; i < 200; i++) {
			if (! numTypes.contains(i)) {
				assertFalse("Char Type: " + i , m.getType(i).isNumeric());
			}
		}

		for (int typeId : BINARY_TYPES) {
			assertTrue("Bin Type: " + typeId, m.getType(typeId).isBinary());
			binTypes.add(typeId);
		}

		for (int i = 0; i < 200; i++) {
			if (! binTypes.contains(i)) {
				assertFalse("Type: " + i, m.getType(i).isBinary());
			}
		}
	}


	public  void testNumbers() throws RecordException {
		Random r = new Random();
		FieldDetail fd;
		int val;
		String res = "";
		boolean ok = true;

		for (int type : NUMERIC_TYPES) {
			switch (type) {
			case Type.ftBit: break;
			default:
				fd = getType(1, 8, type);
				for (int i = 0; i < 5000; i++) {
					val = r.nextInt(10000);

					try {
						setFldValue(fd, val);
						res = getFldValue(fd);
						assertEquals("Type: " + type, val, (new BigDecimal(res.trim())).intValue());
					} catch (Exception e) {
						System.out.println();
						System.out.println();
						System.out.println("Type: " + type + " Count: " + i + " Val: " + val + " result: >" + res + "<");
						System.out.println();
						System.out.println();
						e.printStackTrace();
						ok = false;
						break;
					}
				}
			}
		}

		assertTrue("Check abends ", ok);
	}


	public void testPositive() {
		TypeManager m = TypeManager.getInstance();

		for (int typeId : POSITIVE_TYPES) {
			assertTrue("Positive Type: " + typeId, m.getType(typeId).isNumeric());
		}

	}


	  /**
     * Get field value
     * @param fld field definition
     * @return value of the field
     */
    private String getFldValue(FieldDetail fld) {
        return typeManager.getType(fld.getType()).getField(bytes, fld.getPos(), fld).toString();
    }

    /**
     * Set Fields value
     * @param fld field definition
     * @param val value to assign
     * @throws RecordException any error that occurs
     */
    private void setFldValue(FieldDetail fld, Integer val) throws RecordException {
        typeManager.getType(fld.getType()).setField(bytes, fld.getPos(), fld, val);
    }

	private FieldDetail getType(int pos, int len, int type) {

		FieldDetail field = new FieldDetail("", "", type, 0, "", -1, "");

		field.setPosLen(pos, len);

		return field;
	}

}
