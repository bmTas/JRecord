/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord IOBuilder examples
 *    
 *    Sub-Project purpose: Examples of using JRecord IOBuilders
 *                        to perform IO on Cobol Data files
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
      
package net.sf.JRecord.zExamples.iob.cobol.iobuilder;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class CheckInvalid  {

	private static final String COPYBOOK
				= "       01  Rec-1.\n"
				+ "           05 numC3            pic s9(5) comp-3.\n"
				+ "           05 num9             pic 99999.\n";
	  //          + "           05 text             pic xx.\n";
	
	public static void main(String[] args) throws IOException {
		ICobolIOBuilder iob =JRecordInterface1.COBOL
						.newIOBuilder(new StringReader(COPYBOOK), "Rec-1");
		AbstractLine l = iob.newLine();
		ArrayList<Integer> list = new ArrayList<>(300);
			
		for (int i = 99980; i < 99999; i++) {
			l.getFieldValue(0, 0).set(i);
			l.getFieldValue(0, 1).set(i);
			//l.getFieldValue(0, 2).setToLowValues();
			String fl = l.getFullLine();
			
			if (! isValidString(fl)) {
				list.add(i);
			}
			
//			try {
//				ByteArrayInputStream is = new ByteArrayInputStream(fl.getBytes());
//				BufferedReader r = new BufferedReader(new InputStreamReader(is, "utf-8"));
//				r.readLine();
//				r.close();
//			} catch(Exception e) {
//				e.printStackTrace();
//				list.add(i);
//			}
		}
		
		System.out.println(isValidString("`~ :<> aA@#$%^&*()_+=-,."));
		for (int i =0; i < list.size(); i++) {
			System.out.print("\t" + list.get(i));
			if (i % 8 == 7) System.out.println();
		}
		
	}
	/**
	 * @param fl
	 */
	public static boolean isValidString(String fl) {
		for (int j = 0; j < fl.length(); j++) {
			char ch = fl.charAt(j);
			switch (Character.getType(ch)) {
			case  Character.COMBINING_SPACING_MARK:
			case  Character.CONNECTOR_PUNCTUATION:
			case  Character.CURRENCY_SYMBOL:
			case  Character.DASH_PUNCTUATION:
			case  Character.DECIMAL_DIGIT_NUMBER:
			case  Character.ENCLOSING_MARK:
			case  Character.END_PUNCTUATION:
			case  Character.FINAL_QUOTE_PUNCTUATION:
			case  Character.FORMAT:
			case  Character.INITIAL_QUOTE_PUNCTUATION:
			case  Character.LETTER_NUMBER:
			case  Character.LINE_SEPARATOR:
			case  Character.LOWERCASE_LETTER:
			case  Character.MATH_SYMBOL:
			case  Character.MODIFIER_LETTER:
			case  Character.MODIFIER_SYMBOL:
			case  Character.NON_SPACING_MARK:
			case  Character.OTHER_LETTER:
			case  Character.OTHER_NUMBER:
			case  Character.OTHER_PUNCTUATION:
			case  Character.OTHER_SYMBOL:
			case  Character.PARAGRAPH_SEPARATOR:
			case  Character.SPACE_SEPARATOR:
			case  Character.START_PUNCTUATION:
			case  Character.SURROGATE:
			case  Character.TITLECASE_LETTER:
			case  Character.UPPERCASE_LETTER:
				break;
			// Should be: Character.CONTROL, Character.UNASSIGNED, Character.PRIVATE_USE
			default:
				System.out.print('*' + " " + ch + "~" + Character.getType(ch));
				return false;
			}
		}
		return true;
	}
}
