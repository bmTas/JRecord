package net.sf.JRecord.zTest.Types1;

import junit.framework.TestCase;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;

public class TestTypeManager  extends TestCase {

	@SuppressWarnings("deprecation")
	ShortResult[] testsBin = {
			new ShortResult(Type.ftChar,            4, TypeManager.CharsetType.SINGLE_BYTE_ASCII,        Type.ftChar),
			new ShortResult(Type.ftCharNoTrim,      4, TypeManager.CharsetType.SINGLE_BYTE_EBCDIC,       Type.ftCharNoTrim),
			new ShortResult(Type.ftPackedDecimal,   9, TypeManager.CharsetType.SINGLE_BYTE_EBCDIC,       Type.ftPackedDecimalSmall),
			new ShortResult(Type.ftPackedDecimalPostive,   9, TypeManager.CharsetType.SINGLE_BYTE_EBCDIC,Type.ftPackedDecimalSmallPostive),
			new ShortResult(Type.ftBinaryBigEndian,        8, TypeManager.CharsetType.OTHER_CHARSET,     Type.ftIntBigEndianSmall),
			new ShortResult(Type.ftBinaryBigEndianPositive,8, TypeManager.CharsetType.SINGLE_BYTE_ASCII, Type.ftIntBigEndianPositive),
			new ShortResult(Type.ftPositiveBinaryBigEndian,7, TypeManager.CharsetType.SINGLE_BYTE_EBCDIC,Type.ftUIntBigEndianSmall),
			new ShortResult(Type.ftBinaryInt,         8, TypeManager.CharsetType.SINGLE_BYTE_ASCII,      Type.ftIntSmall),
			new ShortResult(Type.ftBinaryIntPositive, 8, TypeManager.CharsetType.OTHER_CHARSET,          Type.ftIntPositiveSmall),
			new ShortResult(Type.ftPostiveBinaryInt,  7, TypeManager.CharsetType.SINGLE_BYTE_EBCDIC,     Type.ftUIntSmall),
	};
	
	@SuppressWarnings("deprecation")
	ShortResult[] testsText = {
			new ShortResult(Type.ftChar, 4, TypeManager.CharsetType.SINGLE_BYTE_EBCDIC, Type.ftChar),
			new ShortResult(Type.ftChar, 4, TypeManager.CharsetType.OTHER_CHARSET, Type.ftChar),
			new ShortResult(Type.ftZonedNumeric, 17, TypeManager.CharsetType.SINGLE_BYTE_ASCII,  Type.ftZonedAsciiSmall),
			new ShortResult(Type.ftZonedNumeric, 17, TypeManager.CharsetType.SINGLE_BYTE_EBCDIC, Type.ftZonedEbcdicSmall),
//			new ShortResult(Type.ftNumZeroPaddedPositive, 17, TypeManager.CharsetType.SINGLE_BYTE_ASCII,  Type.ftZonedAsciiSmallPositive),
//			new ShortResult(Type.ftNumZeroPaddedPositive, 17, TypeManager.CharsetType.SINGLE_BYTE_EBCDIC, Type.ftZonedEbcdicSmallPositive),
			new ShortResult(Type.ftZonedNumeric, 17, TypeManager.CharsetType.OTHER_CHARSET,      Type.ftZonedNumeric),
	};
	
	public void testGetShortTypeBin() {
		TypeManager typeManager = TypeManager.getInstance();
		for (ShortResult t : testsBin) {
			assertEquals(t.expectedType, typeManager.getShortType(t.type, t.size, t.charsetType));
			assertEquals(t.type, typeManager.getShortType(t.type, t.size+1, t.charsetType));
		}
	}

	
	public void testGetShortTypeText() {
		TypeManager typeManager = TypeManager.getInstance();
		for (ShortResult t : testsText) {
			assertEquals(t.expectedType, typeManager.getShortType(t.type, t.size, t.charsetType));
			assertEquals(t.type, typeManager.getShortType(t.type, t.size+1, t.charsetType));
		}
	}
	

	public static class ShortResult {
		
		private final int type, expectedType, size;
		private final TypeManager.CharsetType charsetType;
		
		public ShortResult(int type, int size, TypeManager.CharsetType charsetType, int expectedType) {
			super();
			this.type = type;
			this.size = size;
			this.charsetType = charsetType;
			this.expectedType = expectedType;
		}
		
	}
}

