package net.sf.JRecord.Numeric;

import net.sf.JRecord.Types.Type;

public class CommonCode {

	public static int commonTypeChecks(
			int binaryFormat, String usage, String picture, boolean signed, boolean signSeperate,
			String signPosition) {
		int iType = 0;
		if ("true".equals(signed) ||  picture.startsWith("S")) {
			if (signSeperate) {
				if ("leading".equals(signPosition)) {
					iType = Type.ftSignSeparateLead;
				} else {
					iType = Type.ftSignSeparateTrail;
				}
			} else {
				if (binaryFormat == Convert.FMT_MAINFRAME) {
					iType = Type.ftZonedNumeric;
				} else {
					iType = Type.ftFjZonedNumeric;
				}
			}
		} else {
			iType = Type.ftAssumedDecimalPositive;
		}
		return iType;
	}


	/**
	 * Validate picture to ensure it is a number with decimal = comma
	 *
	 * @param pict picture to check
	 * @param validChar valid char to check
	 *
	 * @return wether it is a valid picture
	 */
	public static final boolean checkPicture(String pict, char validChar, char decimalChar) {


		boolean check = false;
		boolean foundDot = false;
		boolean lastChSearch =  pict.charAt(0) == validChar;
		boolean lastCh9      =  pict.charAt(0) == '9';

		char ch;

//      used for testing
//
//		if (pict.length() > 1
//				&& pict.indexOf('Z') < 0
//        		&& pict.indexOf('9') >= 0
//	      		&& pict.indexOf('V') < 0
//	      		&& pict.indexOf('.') < 0
//        		&& (! pict.startsWith("S"))
//        		&& pict.indexOf(',') > 0
//        		&& pict.indexOf(',') == pict.lastIndexOf(',')
//        		&& (pict.startsWith("-") || pict.startsWith("+") || pict.startsWith("9"))) {
//					System.out.println();
//        		}

		for (int i = 1; i < pict.length(); i++) {
			ch = pict.charAt(i);
			if (ch == '(') {
				if (!(lastChSearch || lastCh9)) {
					return false;
				}

				check = true;
			} else {
				lastChSearch = false;
				lastCh9      = false;

				if (check) {
					check = ch != ')';
				} else if (ch == validChar) {
					if (foundDot && (validChar != '9')) {
						return false;
					}
					lastChSearch = true;
				} else if (ch == decimalChar) {
						if (foundDot) {
							return false;
						}
						foundDot = true;
				} else {
					switch (ch) {
					case '9': lastCh9 = true;			break;
					default : return false;
					}
				}
			}
		}

		return true;
	}


	public static final boolean checkPictureNumeric(String pict, char decimalChar) {
		boolean foundDot = false;
		boolean check = false;
		boolean minusAllowed = pict.charAt(0) == '-';
		boolean plusAllowed = pict.charAt(0) == '+';
		char ch;
		pict = pict.toUpperCase();

		for (int i = 0; i < pict.length(); i++) {
			ch = pict.charAt(i);
			if (ch == '(') {
				check = true;
			} else if (ch == decimalChar) {
				if (foundDot) {
					return false;
				}
				foundDot = true;
			} else {
				if (check) {
					check = ch != ')';
				} else  {
					switch (ch) {
					case 'Z':
					case '9':
						minusAllowed = false;
						plusAllowed  = false;
					case ',':
					case '.':
					case 'V':
						break;
					case '+':
						if (! plusAllowed) return false;
						break;
					case '-':
						if (! minusAllowed) return false;
						break;
					case 'S':
						if (i > 0) return false;
						break;
					default : return false;
					}
				}
			}
		}

		return true;
	}

}
