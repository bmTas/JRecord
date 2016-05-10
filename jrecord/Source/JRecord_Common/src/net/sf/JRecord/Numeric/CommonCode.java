/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.Numeric;

import net.sf.JRecord.Types.Type;

public class CommonCode {

	public static int commonTypeChecks(
			int dialect, String usage, String picture, boolean signed, boolean signSeperate,
			String signPosition) {
		int iType = 0;
		if (signed ||  picture.startsWith("S")) {
			if (signSeperate) {
				boolean actualDecimal = picture.indexOf('.') >= 0;
				
				iType = Type.ftSignSeparateTrail;
				if ("leading".equals(signPosition)) {
					iType = Type.ftSignSeparateLead;
					if (actualDecimal) {
						iType = Type.ftSignSepLeadActualDecimal;
					}
				} else if (actualDecimal) {
					iType = Type.ftSignSepTrailActualDecimal;
				}
			} else {
				iType = Type.ftGnuCblZonedNumeric;
				switch (dialect) {
				case ICopybookDialects.FMT_MAINFRAME: 				iType = Type.ftZonedNumeric;		break;
				case ICopybookDialects.FMT_FUJITSU:
				case ICopybookDialects.FMT_FUJITSU_COMMA_DECIMAL:	iType = Type.ftFjZonedNumeric;		break;		
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
	public static final boolean checkPicture(String pict, char validChar, char decimalChar, char altDecimal) {


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
				} else if (ch == decimalChar || ch == altDecimal) {
						if (foundDot) {
							return false;
						}
						foundDot = true;
				} else {
					switch (ch) {
					case '9': lastCh9 = true;			break;
					case '+':
					case '-':
						if (i == pict.length() - 1) break;
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
        boolean leadingSign = minusAllowed || plusAllowed;
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
                        if (! leadingSign && i == pict.length() - 1) break;
						if (! plusAllowed) return false;
						break;
					case '-':
                        if (! leadingSign && i == pict.length() - 1) break;
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
