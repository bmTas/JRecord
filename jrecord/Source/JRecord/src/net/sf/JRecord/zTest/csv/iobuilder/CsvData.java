/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
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

package net.sf.JRecord.zTest.csv.iobuilder;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Conversion;

public class CsvData {

	public static final String[][] CsvLineData = {
			 {"KEYCODE-NO", "STORE-NO", "DATE", "DEPT-NO", "QTY-SOLD", "SALE-PRICE"}, 
			 {"69694158", "20", "40118", "280", "-1", "-19.00"},
			 {"69694158", "20", "40118", "280", "1", "5.01"},
			 {"63604808", "20", "40118", "170", "1", "4.87"},
			 {"62684671", "20", "40118", "685", "1", "69.99"},
			 {"62684671", "20", "40118", "685", "-1", "-69.99"},
			 {"64634429", "20", "40118", "957", "1", "3.99"},
			 {"66624458", "20", "40118", "957", "1", "0.89"},
			 {"63674861", "20", "40118", "957", "10", "2.70"},
			 {"65674532", "20", "40118", "929", "1", "3.59"},
			 {"64614401", "59", "40118", "957", "1", "1.99"},
			 {"64614401", "59", "40118", "957", "1", "1.99"},
			 {"61664713", "59", "40118", "335", "1", "17.99"},
			 {"61664713", "59", "40118", "335", "-1", "-17.99"},
			 {"68634752", "59", "40118", "410", "1", "8.99"},			
	};
	
	public static final String[] COLS = {
		"id", "col1", "col2", "col3"
	};
	
	public static final String[] STRINGS = {
			"12345", ",2345", "1,345", "12,45", "123,5", "1234,", "'2345", 
			"1'345", "12'45", "123'5", "1234'", ",'345", ",2'45", ",23'5", 
			",234'", ",2345", "1,'45", "1,3'5", "1,34'", "'2,45", "12,'5", 
			"12,4'", "'2,45", "'234,", "1'34,", "12'4,", "1234,", "'2,45", 
			"'23,5", "'234,", ",'345", "1'345", "1'3,5", "1'34,", ",234'", 
			"1,34'", "12,4'", "123,'", "1234'", "''345", "'2'45", "'23'5",
			"'234'", "1''45", "1'3'5", "1'34'", "12''5", "12'4'", "123''",
			",,345", ",2,45", ",23,5", ",234,", "1,,45", "1,3,5", "1,34,", 
			"12,,5", "12,4,", "123,,", 
			",,'45", ",',45", "',,45", "',3,5", "',34,", "'2,4,", "'23,,", 
			"1'3,,", "12',,", "12,',", "12,,'",
			"'',45", "','45", ",''45", ",'3'5", ",'34'", ",2'4'", ",23''", 
			"1,3''", "12,''", "12','", "12'',",
	};

	
	public static byte[] asBytes(byte sep) {
		if (CommonBits.LINE_SEPARATOR.length() > 1) {
			return asBytes(sep, (byte) '\r', (byte) '\n', Conversion.DEFAULT_ASCII_CHARSET);
		} 
		return asBytes(sep, (byte) '\n', (byte) 0, Conversion.DEFAULT_ASCII_CHARSET);
		
	};
	
	
	public static byte[] asBytes(byte sep, byte eol, byte eol1, String charset) {
		int len = 0;
		for (int i = 0; i < CsvLineData.length; i++) {
			for (String s : CsvLineData[i]) {
				len += s.length() + 1;
			}
		}
		if (eol1 > 0) {
			len += CsvLineData.length;
		}
		byte[] bytes = new byte[len];
		int l = 0;
		for (int i = 0; i < CsvLineData.length; i++) {
			for (String s : CsvLineData[i]) {
				byte[] b = Conversion.getBytes(s, charset);
				System.arraycopy(b, 0, bytes, l, b.length);
				l += b.length;
				bytes[l++] = sep;
			}
			bytes[l-1] = eol;
			if (eol1 > 0) {
				bytes[l++] = eol1;
			}
		}
		
		return bytes;
	}
	
	public static String buildComplicatedCsv() {
		String eol = "\n";

		if (CommonBits.LINE_SEPARATOR.length() > 1) {
			eol = "\r\n";
		}
		StringBuilder b = new StringBuilder()
						.append(COLS[0]).append(',')
						.append(COLS[1]).append(',')
						.append(COLS[2]).append(',')
						.append(COLS[3]).append(eol);
		
		for (int i = 1; i < 4; i++) {
			for (String s : STRINGS) {
				b.append(i);
				for (int j = 1; j < 4; j++) {
					b.append(',');
					if (i == j) {
						b.append(adj4csv(s));
					} else {
						b.append(j).append(j);
					}
				}
				b.append(eol);
			}
		}
		
		return b.toString();
	}
	
	private static String adj4csv(String s) {
		if (s.indexOf(',') >= 0 || s.indexOf('\'') >= 0) {
			s = "'" +Conversion.replace(s, "'", "''") + "'";
		}
		return s;
	}
}
