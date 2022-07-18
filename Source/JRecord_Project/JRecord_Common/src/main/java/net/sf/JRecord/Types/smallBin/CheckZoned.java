package net.sf.JRecord.Types.smallBin;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Types.TypeManager;

public class CheckZoned {
    public static  final byte HIGH_NYBLE = (byte) 0xf0;
//    public static final byte ZONED_POSITIVE_NYBLE_AND = (byte) 0xCF;
//    public static final byte ZONED_NEGATIVE_NYBLE_AND = (byte) 0xDF;
    public static  final byte ZONED_NEGATIVE_NYBLE_VALUE1 = (byte) 0xD0;
    public static  final byte ZONED_POSITIVE_NYBLE_VALUE1 = (byte) 0xC0;
    public static  final byte ZONED_NEGATIVE_NYBLE_VALUE2 = (byte) 0x80;
    public static  final byte ZONED_POSITIVE_NYBLE_VALUE2 = (byte) 0x90;
    
    
	
	public static boolean checkAsciiZoned(IFieldDetail fldDef, String num) {
		if (num == null || num.length() != fldDef.getLen()) { return false; }
		
		boolean allowNegative = ! TypeManager.isPositiveNumeric(fldDef.getType());

		char signChar = num.charAt(fldDef.getLen() - 1);
		if ((signChar >= '0' && signChar <= '9')
		|| (signChar >= 'P' && signChar <= 'Y' && allowNegative)
		|| (signChar >= 'A' && signChar <= 'I')
		|| (signChar >= 'p' && signChar <= 'y' && allowNegative)
		|| (signChar >= 'a' && signChar <= 'i')
		|| signChar == '@') {
			return checkNumeric(num);
		}
		return false;
	}


	public static boolean checkAsciiZoned(int pos, IFieldDetail fldDef, byte[] line ) {
		String val = Conversion.getString(line, pos-1, pos + fldDef.getLen() - 1, fldDef.getFontName());
		return checkAsciiZoned(fldDef, val);
	}


	public static boolean checkMainframeZoned(int pos, IFieldDetail fldDef, byte[] line ) {
		int len = fldDef.getLen();

		pos -= 1;
		if (line == null || line.length < pos + len) { return false; }
		if (Conversion.isEbcidic(fldDef.getFontName())) {
			byte signByte = line[pos + len - 1];
			int xx = signByte & 0xf0;
			byte signNyble = (byte) xx;
			boolean ret = true;
			switch (signNyble) {
			case ZONED_NEGATIVE_NYBLE_VALUE1:
			case ZONED_NEGATIVE_NYBLE_VALUE2:
				ret = ! TypeManager.isPositiveNumeric(fldDef.getType());
				break;
			case HIGH_NYBLE:
			case ZONED_POSITIVE_NYBLE_VALUE1:
			case ZONED_POSITIVE_NYBLE_VALUE2:
				break;
			default:
				if (signByte < 0xF0 || signByte > 0xF9) {
					return false;
				}
			}
			int digit = signByte & 0x0f;
			if (digit > 9) {
				return false;
			}
			
			for (int i = 0; i < len - 1; i++) {
				int v = line[pos+i] & 0xFF;
				if (v < 0xF0 || line[pos+i] > 0xF9) {
					return false;
				}
			}
			return ret;
		} 
		return checkMainframeZoned(fldDef, Conversion.getString(line, pos, pos+len, fldDef.getFontName()));
	}

	
	public static boolean checkMainframeZoned(IFieldDetail fldDef, String num) {	
		if (num == null
		|| num.length() != fldDef.getLen()) { return false; }
		boolean allowNegative = ! TypeManager.isPositiveNumeric(fldDef.getType());

		
		char signChar = num.charAt(fldDef.getLen() - 1);
		if ((signChar >= '0' && signChar <= '9')
		|| (signChar >= 'J' && signChar <= 'R' && allowNegative)
		|| (signChar >= 'A' && signChar <= 'I')
		|| (signChar >= 'j' && signChar <= 'r')
		|| (signChar >= 'a' && signChar <= 'i' && allowNegative)
		|| signChar == '{' 
		|| signChar == '}' && allowNegative) {
			return checkNumeric(num);
		}
		return false;
	}

	private static boolean checkNumeric(String num) {
		if (num == null) { return false;}
		for (int i = 0; i < num.length() - 1; i++) {
			if (num.charAt(i) < '0' || num.charAt(i) > '9') {
				return false;
			}
		}
		return true;
	}
}
