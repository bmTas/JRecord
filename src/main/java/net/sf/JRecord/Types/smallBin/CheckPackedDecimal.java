package net.sf.JRecord.Types.smallBin;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Types.TypeManager;

public class CheckPackedDecimal {

	static final int NEGATIVE_SIGN_NYBLE = 0x0D;

	static final byte[] TO_NUM
    =	{ 00, 01, 02, 03, 04, 05, 06, 07,  8,  9, -1, -1, -1, -1, -1, -1
	    , 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, -1, -1, -1, -1, -1, -1
	    , 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, -1, -1, -1, -1, -1, -1
	    , 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, -1, -1, -1, -1, -1, -1
	    , 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, -1, -1, -1, -1, -1, -1
	    , 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, -1, -1, -1, -1, -1, -1
	    , 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, -1, -1, -1, -1, -1, -1
	    , 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, -1, -1, -1, -1, -1, -1
	    , 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, -1, -1, -1, -1, -1, -1
	    , 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, -1, -1, -1, -1, -1, -1
	    , -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  
	    , -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  
	    , -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  
	    , -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  
	    , -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1  
	    , -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 }; 

	public static boolean checkPackedDecimal(
			int position,
			final IFieldDetail field,
			byte[] record) {
		int pos = position - 1;	
		if (record.length < pos + field.getLen()) {
			return false;
		}
		int len = field.getLen();
		boolean allowNegative = ! TypeManager.isPositiveNumeric(field.getType());

		
		for (int i = 0; i < len-1; i++) {
			if (TO_NUM[record[pos+i] & 0xFF] < 0) {
				return false;
			}
		}
		int signByte = record[pos+len-1] & 0xFF;
		int i = (signByte ) >> 4;
		if (i > 9) {
			return false;
		}
		int signNyble = signByte & (0x0F);
		return signNyble > 9 && (allowNegative || (signNyble != NEGATIVE_SIGN_NYBLE));
	}
}
