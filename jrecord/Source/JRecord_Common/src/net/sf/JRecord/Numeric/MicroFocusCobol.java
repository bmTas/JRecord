package net.sf.JRecord.Numeric;

import net.sf.JRecord.Types.Type;
import net.sf.cb2xml.def.BasicNumericDefinition;

public class MicroFocusCobol extends GenericNumericDefinition {

	private static final int[] BIN_SIZES_1_TO_8 = {1, 2, 3 ,4 ,5 ,6 ,7 , 8};
//	private static final int[] NO_SYNC = {1, 1, 1, 1};
	private static final int[] BIN_SIZES_1248 = {1, 2, 4, 8};
	private static final int[] BIN_SIZES_1244 = {1, 2, 4, 4};

	private final static int[] STANDARD_TYPES = {
		Type.ftFjZonedNumeric,  Type.ftBinaryBigEndian,
		Type.ftBinaryBigEndian, Type.ftFloat, Type.ftDouble, Type.ftPackedDecimal,
		Type.ftBinaryBigEndian, Type.ftBinaryInt
	};
	private final static int[] POSITIVE_TYPES = {
		Type.ftAssumedDecimalPositive,  Type.ftBinaryBigEndianPositive,
//		Type.ftFjZonedNumeric,          Type.ftBinaryBigEndianPositive,
		Type.ftBinaryBigEndianPositive, Type.ftFloat, Type.ftDouble, Type.ftPackedDecimalPostive,
		Type.ftBinaryBigEndianPositive, Type.ftBinaryIntPositive
	};

	//private static int[] COMP5_SIZES = {1,2,4,8};

	private static int[] COMP5_DIGITS = {
		BasicNumericDefinition.MAX_COMP_SIZE[0], BasicNumericDefinition.MAX_COMP_SIZE[1],
		BasicNumericDefinition.MAX_COMP_SIZE[3], BasicNumericDefinition.MAX_COMP_SIZE[7]
	};
	private static int[] COMP5_POSITIVE_SIZES = {
		BasicNumericDefinition.MAX_POSITIVE_COMP_SIZE[0], BasicNumericDefinition.MAX_POSITIVE_COMP_SIZE[1],
		BasicNumericDefinition.MAX_POSITIVE_COMP_SIZE[3], BasicNumericDefinition.MAX_POSITIVE_COMP_SIZE[7]
	};

	public MicroFocusCobol() {
		super(Convert.FMT_MICRO_FOCUS, Convert.FMT_BIG_ENDIAN, "Microfocus Cobol",
				BIN_SIZES_1_TO_8, BIN_SIZES_1248, BIN_SIZES_1244,
				STANDARD_TYPES, POSITIVE_TYPES, 4, 8);
	}

	@Override
	public int getBinarySize(String usage, int numDigits, boolean positive,
			boolean sync) {
		int ret;
		if (getBinCode(usage) == COMP_5) {
			ret = getBinSizes(numDigits, positive, BIN_SIZES_1248, COMP5_DIGITS, COMP5_POSITIVE_SIZES);
		} else {
			ret =  super.getBinarySize(usage, numDigits, positive, sync);
		}

		return ret;
	}


}
