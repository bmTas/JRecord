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

package net.sf.JRecord.zTest.Common;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Common.TranslateXmlChars;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.RecordEditorXmlLoader;
import net.sf.JRecord.Log.TextLog;



public class TestCommonCode {


	public static ByteArrayInputStream arrayToStream(String[] array) {
		return arrayToStream(array, "\n", "");
	}


	public static ByteArrayInputStream arrayToStream(String[] array, String eol) {
		return arrayToStream(array, eol, "");
	}


	public static ByteArrayInputStream arrayToStream(String[] array, String eol, String charset) {
		return stringToStream(arrayToString(array, eol), charset);
	}

	public static String arrayToString(String[] array, String eol) {

		StringBuilder b = new StringBuilder();

		for (int i = 0; i < array.length; i++) {
			b.append(array[i]).append(eol);
		}

		return b.toString();
	}

	public static ByteArrayInputStream stringToStream(String s) {
//		System.out.println();
//		System.out.println(s);
//		System.out.println();
		return new ByteArrayInputStream(s.getBytes());
	}


	public static ByteArrayInputStream stringToStream(String s, String charset) {
		return new ByteArrayInputStream(Conversion.getBytes(s, charset));
	}

	public static final LayoutDetail getCsvLayout(int fileStructure, String font, String delim, String quote,
			boolean embeddedCr, int style)
	throws Exception {
		return getCsvExternal(fileStructure + "", font, delim, quote, embeddedCr, style).asLayoutDetail();
	}

	public static final LayoutDetail getCsvLayout(String fileStructure, String font, String delim, String quote,
			boolean embeddedCr, int style)
	throws Exception {
		return getCsvExternal(fileStructure, font, delim, quote, embeddedCr, style).asLayoutDetail();
	}

	public static final ExternalRecord getCsvExternal(String fileStructure, String font, String delim, String quote,
			boolean embeddedCr, int style)
	throws Exception {
		String xml;
		String embeddedStr = "";
		String fontStr = "";

		if ( "<none>".equals(quote.toLowerCase())) {
			quote = "";
		}
		if (embeddedCr) {
			embeddedStr = " " + Constants.RE_XML_EMBEDDED_CR + "=\"Y\" ";
		}

		if (font != null && ! "".equals(font)) {
			fontStr = " FONTNAME=\"" + font + "\" ";
		}

		xml = "<RECORD RECORDNAME=\"Delimited\" COPYBOOK=\"\" STYLE=\"" + style + "\""
			+ "        FILESTRUCTURE=\"" + fileStructure + "\""
			+ "        DELIMITER=\"" + TranslateXmlChars.replaceXmlCharsStr(delim) + "\""
			+ "        QUOTE=\"" + TranslateXmlChars.replaceXmlCharsStr(quote) + "\""
			+          embeddedStr + fontStr
			+ "		   DESCRIPTION=\"Delimited\" RECORDTYPE=\"Delimited\" RecSep=\"default\">"
			+ "	<FIELDS>"
			+ "		<FIELD NAME=\"Dummy\" DESCRIPTION=\" \" POSITION=\"1\" TYPE=\"Char\"/>"
			+ "	</FIELDS>"
			+ "</RECORD>";

//		System.out.println("Generated Xml: " +xml);

		return RecordEditorXmlLoader.getExternalRecord(xml, "CsvNamesFirstLine");
	}


	public static LayoutDetail getLayoutFromCobolStr(
			String cobolCopybookString,
			String copyBookName,
		  	int splitCopybook,
		  	final String font,
			final int binaryFormat) throws RecordException {
		return getExternalRecordFromCobolStr(cobolCopybookString, copyBookName, splitCopybook, font, binaryFormat)
				.asLayoutDetail();
	}

	public static ExternalRecord getExternalRecordFromCobolStr(
			String cobolCopybookString,
			String copyBookName,
		  	int splitCopybook,
		  	final String font,
			final int binaryFormat) throws RecordException {
		ByteArrayInputStream in = new ByteArrayInputStream(cobolCopybookString.getBytes());
		CobolCopybookLoader conv = new CobolCopybookLoader();

		return conv.loadCopyBook(in, copyBookName, splitCopybook, 0, font, binaryFormat, 0, new TextLog());
	}
	
	
	public static byte[] loadFile(String filename) {
		File f = new File(filename);
		byte[] b = new byte[(int)f.length()];
		try {
			BufferedInputStream in = new BufferedInputStream(new FileInputStream(f));
			int n = in.read(b);
			int num = n;
			
			while (n > 0) {
				n = in.read(b, num, b.length - num);
				num += n;
			}
			in.close();
			return b;
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		return new byte[0];
	}

}
