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

package net.sf.JRecord.zTest.charIO;

import java.io.IOException;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.charIO.CsvCharReader;
import net.sf.JRecord.zTest.Common.TestCommonCode;
import junit.framework.TestCase;

/**
 * Testing embedded CR in first line in file
 *
 * @author Bruce Martin
 *
 */
public class TstCsvCharReader03lq1 extends TestCase {


	private static final String[] EOLS = {"\n", "\r", "\r\n"};


	private static final String[] CHARSETS = {
		"", "CP037", "UTF-8"
	};


	String[][] files01 = {
			{	"1,2,3",			"4,5,6"},
			{	"'`\\x1'`,2,3",		"4,5,6"},
			{	"1,'`\\x2'`,3",		"4,5,6"},
			{	"1,2,'`\\x3'`",		"4,5,6"},
			{	"1,2,'`3\\x'`",		"4,5,6"},
			{	"1,'`2\\x'`,3",		"4,5,6"},
			{	"'`,\\x1'`,2,3",	"4,5,6"},
			{	"1,'`,\\x2'`,3",	"4,5,6"},
			{	"1,2,'`,\\x3'`",	"4,5,6"},
	};

	String[][] files02 = {
			{	"'`'`'`\\x1'`,2,3",		"4,5,6"},
			{	"1,'`'`'`\\x2'`,3",		"4,5,6"},
			{	"'`'`'`'`'`\\x1'`,2,3",	"4,5,6"},
			{	"1,'`'`'`'`'`\\x2'`,3",	"4,5,6"},
			{	"1,'`2'`'`\\x'`,3",		"4,5,6"},
			{	"1,'`'`'`,\\x2'`,3",	"4,5,6"},
			{	"1,2,'`'`'`,\\x3'`",	"4,5,6"},
			{	"'`,'`'`\\x1'`,2,3",	"4,5,6"},
			{	"'`,\\x1'`,2,3",		"4,5,6"},
			{	"1,'`,'`'`\\x2'`,3",	"4,5,6"},
			{	"1,2,'`,'`'`\\x3'`",	"4,5,6"},
	};

	public void test01() throws IOException {

		for (int i = 0; i < EOLS.length; i++) {
			for (int j = 0; j < EOLS.length; j++) {
				for (int k = 0; k < CHARSETS.length; k++) {
					for (int l = 0; l < files01.length; l++) {
						String rest = i + ", " + j + ", " + CHARSETS[k] + ", " + l;
						tstFile("01a " + rest,
								new CsvCharReader(",", "'`", null, false),
								EOLS[i], EOLS[j], CHARSETS[k], files01[l]);
						tstFile("01b " + rest,
								new CsvCharReader(",", "'`", "'`'`", false),
								EOLS[i], EOLS[j], CHARSETS[k], files01[l]);
					}
				}
			}
		}
	}


	public void test02() throws IOException {

		for (int i = 0; i < EOLS.length; i++) {
			for (int j = 0; j < EOLS.length; j++) {
				if (i != 2 || j != 0) {
					for (int k = 0; k < CHARSETS.length; k++) {
						for (int l = 0; l < files02.length; l++) {
							String rest = i + ", " + j + ", " + CHARSETS[k] + ", " + l;
//							System.out.println();
//							System.out.println("  ---------------------------- ");
							tstFile("02) " + rest,
									new CsvCharReader(",", "'`", "'`'`", false),
									EOLS[i], EOLS[j], CHARSETS[k], files02[l]);
						}
					}
				}
			}
		}
	}
	private void tstFile(String id, CsvCharReader r, String eol1, String eol2, String charset, String[] f)
			throws IOException {
		String[] ff = new String[f.length + 5];

		for (int i = 0; i < f.length; i++) {
			ff[i] = Conversion.replace(
							Conversion.replace(new StringBuilder(f[i]), "\\x", eol1),
							"\\y",
							eol2)
					.toString();
		}
		ff[f.length] = "88,99,11";
		ff[f.length + 1] = "'`" + eol1 + "88'`,99,11";
		ff[f.length + 2] = "'`00" + eol1 + "'`,99,11";
		ff[f.length + 3] = "'`" + eol2 + "88a'`,99,11";
		ff[f.length + 4] = "'`00a" + eol2 + "'`,99,11";

		r.open(TestCommonCode.arrayToStream(ff, eol2, charset), charset);

		assertEquals("Eol tst- " + id, eol2, r.getEol());

		String line;
		int i = 0;

		while ((line = r.read()) != null) {
			assertEquals("line  " + (i) + " ~ " + id, ff[i], line);
			i += 1;
		}
	}
}
