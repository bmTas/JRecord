/**
 *
 */
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

import java.io.ByteArrayInputStream;
import java.io.IOException;

import junit.framework.TestCase;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.charIO.CsvCharReader;
import net.sf.JRecord.charIO.ICharReader;
import net.sf.JRecord.charIO.StandardCharReader;

/**
 * Purpose:<ol>
 *  <li>Test Mismatched End-of-line characters</li>
 *  <li>Volume tests
 * </ol>
 * @author Bruce Martin
 *
 */
public class TstCharReader3 extends TestCase {

	public void testStdCr() throws IOException {

		doFontCrTest("StdLine", "", new StdLine());
	}

	public void testStdCrCp037() throws IOException {

		doFontCrTest("StdLine cp037 ", "cp037", new StdLine());
	}


	public void testStdCrUtf8() throws IOException {

		doFontCrTest("StdLine utf-8 ", "utf-8", new StdLine());
	}


	public void testStdCrUtf16() throws IOException {

		doFontCrTest("StdLine utf-16 ", "utf-16", new StdLine());
	}


	public void testCsvStdCr() throws IOException {

		doCsvFontCrTest("Embedded Cr", "");
	}


	public void testCsvStdCrCp037() throws IOException {

		doCsvFontCrTest("Embedded Cr cp037", "cp037");
	}


	public void testCsvStdCrUtf8() throws IOException {

		doCsvFontCrTest("Embedded Cr utf-8", "utf-8");
	}


	public void testCsvStdCrUtf16() throws IOException {

		doCsvFontCrTest("Embedded Cr utf-16", "utf-16");
	}

	private void doFontCrTest(String id, String font, FormatLine fl) throws IOException {

		doTest(id + " \\n) ", new StandardCharReader(), fl, font, "\n", "\n");
		doTest(id + " \\r) ", new StandardCharReader(), fl, font, "\r", "\r");
		doTest(id + " \\r\\n) ", new StandardCharReader(), fl, font, "\r\n", "\r\n");

//		System.out.println(" -------------- ");
		doTest(id + " \\n) ", new CsvCharReader( ",", "", "", true), fl, font, "\n", "\n");

//		System.out.println(" -------------- ");
		doTest(id + " \\r) ", new CsvCharReader( ",", "", "", true), fl, font, "\r", "\r");
		doTest(id + " \\r\\n) ", new CsvCharReader( ",", "", "", true), fl, font, "\r\n", "\r\n");

		doTest(id + " \\r, \\r\\n) ", new StandardCharReader(), fl, font, "\r", "\r\n");
		doTest(id + " \\n, \\r\\n) ", new StandardCharReader(), fl, font, "\r", "\r\n");

		doTest(id + " \\r, \\r\\n) ", new CsvCharReader( ",", "", "", true), fl, font, "\r", "\r\n");
		doTest(id + " \\n, \\r\\n) ", new CsvCharReader( ",", "", "", true), fl, font, "\n", "\r\n");
	}

	private void doCsvFontCrTest(String id, String font) throws IOException {

		//FormatLine fl = EmbeddedCrFormater();


		doTest(id + " \\n) ", new CsvCharReader( ",", "\"", "\"\"", true), new EmbeddedCrFormater("\n"), font, "\n", "\n");
		doTest(id + " \\r) ", new CsvCharReader( ",", "\"", "\"\"", true),  new EmbeddedCrFormater("\n"), font, "\r", "\r");
		doTest(id + " \\r\\n) ", new CsvCharReader( ",", "\"", "\"\"", true),  new EmbeddedCrFormater("\n"), font, "\r\n", "\r\n");

		doTest(id + " \\n) ", new CsvCharReader( ",", "\"", "\"\"", true), new EmbeddedCrFormater("\r"), font, "\n", "\n");
		doTest(id + " \\r) ", new CsvCharReader( ",", "\"", "\"\"", true),  new EmbeddedCrFormater("\r"), font, "\r", "\r");
		doTest(id + " \\r\\n) ", new CsvCharReader( ",", "\"", "\"\"", true),  new EmbeddedCrFormater("\r"), font, "\r\n", "\r\n");

		doTest(id + " \\n) ", new CsvCharReader( ",", "\"", "\"\"", true), new EmbeddedCrFormater("\r\n"), font, "\n", "\n");
		doTest(id + " \\r) ", new CsvCharReader( ",", "\"", "\"\"", true),  new EmbeddedCrFormater("\r\n"), font, "\r", "\r");
		doTest(id + " \\r\\n) ", new CsvCharReader(",", "\"", "\"\"", true),  new EmbeddedCrFormater("\r\n"), font, "\r\n", "\r\n");

		doTest(id + " \\r, \\r\\n) ", new CsvCharReader( ",", "\"", "\"\"", true), new EmbeddedCrFormater("\r"), font, "\r", "\r\n");
		doTest(id + " \\n, \\r\\n) ", new CsvCharReader( ",", "\"", "\"\"", true),  new EmbeddedCrFormater("\n"), font, "\n", "\r\n");
	}

	private void doTest(String id, ICharReader r, FormatLine fl, String font, String eol1, String eol2)
	throws IOException {
		String b;
		int i = 0;

		r.open(getFile1(fl, font, eol1, eol2), font);

		while ((b = r.read()) != null) {
			assertEquals(id + ": " + i, fl.formatLine(i), b);
			i += 1;
		}
		r.close();
	}

	public static ByteArrayInputStream getFile1(FormatLine fl, String font, String eol1, String eol2) {
		StringBuilder b = new StringBuilder(fl.formatLine(0) + eol1);

		for (int i = 1; i < 50000; i++) {
			b.append(fl.formatLine(i) + eol2);
		}

		return new ByteArrayInputStream(Conversion.getBytes(b.toString(), font));
	}

	public static interface FormatLine {
		public String formatLine(int code);
	}

	public static class StdLine implements FormatLine {
	private static long l = Integer.MAX_VALUE;

		public String formatLine(int c) {
			long code = l + c;
			String k = "" + code;
			StringBuilder b = new StringBuilder(k);
			int n = (c % 16) + 5;

			for (int i = 5; i < n; i++) {
				b.append(',').append((120 + i) + "").append(',').append(k);
			}

			return b.toString();
		}
	}

	public static class EmbeddedCrFormater implements FormatLine {
		private static long l = Integer.MAX_VALUE;

		private final String eol;


		public EmbeddedCrFormater(String eol) {
			super();
			this.eol = eol;
		}


		public String formatLine(int c) {
			long code = l + c;
			String k = "" + code;
			StringBuilder b = new StringBuilder(k);
			int n = (c % 16) + 5;

			for (int i = 5; i < n; i++) {
				b.append(',').append('"').append((120 + i) + eol + " " + k).append('"').append(',').append(k);
			}

			return b.toString();
		}
	}
}
