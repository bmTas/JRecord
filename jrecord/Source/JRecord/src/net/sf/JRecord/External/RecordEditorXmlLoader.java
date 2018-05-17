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

package net.sf.JRecord.External;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.ILineFieldNames;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.base.BaseRecordEditorXmlLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.XmlLineReader;
import net.sf.JRecord.Log.AbsSSLogger;

/**
 * Class to Load a RecordLayout (Record or Line Description)
 * from an XML file (RecordEditor-XML)
 *
 * <pre>
 *   <b>Usage:</b>
 *        CopybookLoader loader = new RecordEditorXmlLoader();
 *        LayoutDetail layout = loader.loadCopyBook(copybookName, 0, 0, "", 0, 0, null).asLayoutDetail();
 * </pre>
 *
 * @author Bruce Martin
 *
 */
public class RecordEditorXmlLoader extends BaseCopybookLoader implements ICopybookLoaderStream {

	public static ExternalRecord getExternalRecord(String xml, String copyBookName) throws IOException {
		return new RecordEditorXmlLoader().loadCopyBook(new StringReader(xml), copyBookName, 0, 0, "", 0, 0, 0, null);
	}
	
//	public static ExternalRecord getExternalRecord(String xml, String name) throws Exception {
//		ByteArrayInputStream bs = new ByteArrayInputStream(xml.getBytes());
//
//		return (new RecordEditorXmlLoader()).loadCopyBook(bs, name, "", null);
//	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.ICopybookLoaderStream#loadCopyBook(java.io.InputStream, java.lang.String, int, int, java.lang.String, int, int, int, net.sf.JRecord.Log.AbsSSLogger)
	 */
	@Override
	public ExternalRecord loadCopyBook(InputStream inputStream,
			String copyBookName, int splitCopybook, int dbIdx, String font,
			int copybookFormat, int binaryFormat, int systemId, AbsSSLogger log)
			throws IOException {
		
        XmlLineReader r = new XmlLineReader(true);
		r.open(inputStream, (LayoutDetail) null);

	    return loadCopybook(r, copyBookName, font, log);
	}
	
	public ExternalRecord loadCopyBook(InputStream is, String copyBookName) throws IOException {
		XmlLineReader r = new XmlLineReader(true);
		r.open(is, (LayoutDetail) null);

		return loadCopybook(r, copyBookName, "", null);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.CopybookLoader#loadCopyBook(java.lang.String, int, int, java.lang.String, int, int, int, net.sf.JRecord.Log.AbsSSLogger)
	 */
	@Override
	public ExternalRecord loadCopyBook(String copyBookFile,
			int splitCopybookOption, int dbIdx, String font,
			int copybookFormat, int binFormat, int systemId, AbsSSLogger log)
			throws Exception {
			
		XmlLineReader reader = new XmlLineReader(true);
		reader.open(copyBookFile);

		return loadCopybook(reader, Conversion.getCopyBookId(copyBookFile), font, log);
	}
	
	@Override
	public ExternalRecord loadCopyBook(Reader reader, String copyBookName, int splitCopybook, int dbIdx, String font,
			int copybookFormat, int binaryFormat, int systemId, AbsSSLogger log) throws IOException {

        XmlLineReader r = new XmlLineReader(true);
		r.open(reader, (LayoutDetail) null);
	    return loadCopybook(r, copyBookName, font, log);
	}

	/**
	 * @param reader
	 * @param copyBookName
	 * @param font
	 * @param log
	 * @return
	 * @throws IOException
	 */
	private ExternalRecord loadCopybook(XmlLineReader reader, String copyBookName, String font, AbsSSLogger log)
			throws IOException {
		return (new BaseRecordEditorXmlLoader<ExternalRecord>(
								new XmlReader(reader),
								new ExternalRecordBuilder()
					)).loadCopyBook(copyBookName, font, log);
	}

	

	private static class XmlReader implements BaseRecordEditorXmlLoader.ILineReader {

		private final AbstractLineReader reader;
		
		public XmlReader(AbstractLineReader reader) {
			super();
			this.reader = reader;
		}

		@Override
		public ILineFieldNames read() throws IOException {
			return reader.read();
		}

		@Override
		public void close()  throws IOException {
			reader.close();
		}
		
	}
}
