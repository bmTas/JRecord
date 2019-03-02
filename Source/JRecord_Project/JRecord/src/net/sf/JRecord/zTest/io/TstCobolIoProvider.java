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

package net.sf.JRecord.zTest.io;

import junit.framework.TestCase;
import net.sf.JRecord.ByteIO.FixedLengthByteReader;
import net.sf.JRecord.ByteIO.FujitsuVbByteReader;
import net.sf.JRecord.ByteIO.VbByteReader;
import net.sf.JRecord.ByteIO.VbByteWriter;
import net.sf.JRecord.ByteIO.VbDumpByteWriter;
import net.sf.JRecord.Common.BasicFileSchema;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.IO.FixedLengthWriter;
import net.sf.JRecord.IO.LineReaderWrapper;
import net.sf.JRecord.IO.LineWriterWrapper;
import net.sf.JRecord.IO.TextLineWriter;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.zTest.Common.TstConstants;
import net.sf.cb2xml.def.Cb2xmlConstants;

/**
 * Testing the correct LineReader's and LineWriter's are returned by
 * the CobolIoProvider class
 * 
 * @author Bruce Martin
 *
 */
public class TstCobolIoProvider extends TestCase {

	private static String copybookName = TstConstants.COBOL_DIRECTORY + "DTAR020.cbl";
	private static final String filename = TstConstants.SAMPLE_DIRECTORY + "DTAR020.bin";
	private static final String outfilename = TstConstants.TEMP_DIRECTORY + "DTAR020.ttt.bin";

	/**
	 * Test GetLineReaders (standard structure
	 * 
	 * @throws Exception
	 */
	public void testGetReader1() throws Exception {
		CobolIoProvider ioProvider = CobolIoProvider.getInstance();
		
		AbstractLineReader lineReader;
		lineReader = ioProvider.getLineReader(Constants.IO_FIXED_LENGTH, ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE, copybookName, filename);
		
		System.out.println(lineReader.getClass().getName());
		System.out.println(((LineReaderWrapper) lineReader).getReader().getClass().getName());
		assertTrue(((LineReaderWrapper) lineReader).getReader() instanceof FixedLengthByteReader);
		lineReader.close();
		
		lineReader = ioProvider.getLineReader(Constants.IO_VB, ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE, copybookName, filename);

		System.out.println(lineReader.getClass().getName());
		System.out.println(((LineReaderWrapper) lineReader).getReader().getClass().getName());
		assertTrue(((LineReaderWrapper) lineReader).getReader() instanceof VbByteReader);
		lineReader.close();
		
		lineReader = ioProvider.getLineReader(Constants.IO_VB_GNU_COBOL, ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE, copybookName, filename);

		System.out.println(lineReader.getClass().getName());
		System.out.println(((LineReaderWrapper) lineReader).getReader().getClass().getName());
		assertTrue(((LineReaderWrapper) lineReader).getReader() instanceof VbByteReader);
		lineReader.close();
		
		lineReader = ioProvider.getLineReader(Constants.IO_VB_FUJITSU, ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE, copybookName, filename);

		System.out.println(lineReader.getClass().getName());
		System.out.println(((LineReaderWrapper) lineReader).getReader().getClass().getName());
		assertTrue(((LineReaderWrapper) lineReader).getReader() instanceof FujitsuVbByteReader);
		lineReader.close();
	}
	

	/**
	 * Test getLineReader where (specifying a copybook structure
	 * @throws Exception
	 */
	public void testGetReader2() throws Exception {
		CobolIoProvider ioProvider = CobolIoProvider.getInstance();
		
		AbstractLineReader lineReader;
		lineReader = ioProvider.getLineReader(Constants.IO_FIXED_LENGTH, ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE, Cb2xmlConstants.USE_STANDARD_COLUMNS, copybookName, filename);
		
		System.out.println(lineReader.getClass().getName());
		System.out.println(((LineReaderWrapper) lineReader).getReader().getClass().getName());
		assertTrue(((LineReaderWrapper) lineReader).getReader() instanceof FixedLengthByteReader);
		lineReader.close();
		
		lineReader = ioProvider.getLineReader(Constants.IO_VB, ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE, Cb2xmlConstants.USE_STANDARD_COLUMNS, copybookName, filename);

		System.out.println(lineReader.getClass().getName());
		System.out.println(((LineReaderWrapper) lineReader).getReader().getClass().getName());
		assertTrue(((LineReaderWrapper) lineReader).getReader() instanceof VbByteReader);
		lineReader.close();
		
		lineReader = ioProvider.getLineReader(Constants.IO_VB_GNU_COBOL, ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE, Cb2xmlConstants.USE_STANDARD_COLUMNS, copybookName, filename);

		System.out.println(lineReader.getClass().getName());
		System.out.println(((LineReaderWrapper) lineReader).getReader().getClass().getName());
		assertTrue(((LineReaderWrapper) lineReader).getReader() instanceof VbByteReader);
		lineReader.close();
		
		lineReader = ioProvider.getLineReader(Constants.IO_VB_FUJITSU, ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE, Cb2xmlConstants.USE_STANDARD_COLUMNS, copybookName, filename);

		System.out.println(lineReader.getClass().getName());
		System.out.println(((LineReaderWrapper) lineReader).getReader().getClass().getName());
		assertTrue(((LineReaderWrapper) lineReader).getReader() instanceof FujitsuVbByteReader);
		lineReader.close();
	}

	/**
	 * Test getLineWriter using just the FileStructure
	 * @throws Exception
	 */
	@SuppressWarnings("deprecation")
	public void testGetWriter1() throws Exception {
		CobolIoProvider ioProvider = CobolIoProvider.getInstance();
		
		AbstractLineWriter lineWriter;
		lineWriter = ioProvider.getLineWriter(Constants.IO_FIXED_LENGTH, outfilename);
		System.out.println(lineWriter.getClass().getName());
		assertTrue(lineWriter instanceof FixedLengthWriter);
		lineWriter.close();
		
		lineWriter = ioProvider.getLineWriter(Constants.IO_VB, outfilename);
		System.out.println(lineWriter.getClass().getName());
		System.out.println(((LineWriterWrapper)lineWriter).getWriter().getClass().getName());
		assertTrue(((LineWriterWrapper)lineWriter).getWriter() instanceof VbByteWriter);
		lineWriter.close();
		
		
		lineWriter = ioProvider.getLineWriter(Constants.IO_VB_DUMP, outfilename);
		System.out.println(lineWriter.getClass().getName());
		System.out.println(((LineWriterWrapper)lineWriter).getWriter().getClass().getName());
		assertTrue(((LineWriterWrapper)lineWriter).getWriter() instanceof VbDumpByteWriter);
		lineWriter.close();
		
		lineWriter = ioProvider.getLineWriter(Constants.IO_TEXT_LINE, outfilename);
		System.out.println(lineWriter.getClass().getName());
		assertTrue(lineWriter instanceof TextLineWriter);
		lineWriter.close();
		
		lineWriter = ioProvider.getLineWriter(Constants.IO_UNICODE_TEXT, outfilename);
		System.out.println(lineWriter.getClass().getName());
		assertTrue(lineWriter instanceof TextLineWriter);
		lineWriter.close();
	}
	
	/**
	 * Test getLineWriter using a "FileSchema"
	 * @throws Exception
	 */
	public void testGetWriter2() throws Exception {
		CobolIoProvider ioProvider = CobolIoProvider.getInstance();
		
		AbstractLineWriter lineWriter;
		lineWriter = ioProvider.getLineWriter(BasicFileSchema.newFixedSchema(Constants.IO_FIXED_LENGTH), outfilename);
		System.out.println(lineWriter.getClass().getName());
		assertTrue(lineWriter instanceof FixedLengthWriter);
		lineWriter.close();
		
		lineWriter = ioProvider.getLineWriter(BasicFileSchema.newFixedSchema(Constants.IO_VB), outfilename);
		System.out.println(lineWriter.getClass().getName());
		System.out.println(((LineWriterWrapper)lineWriter).getWriter().getClass().getName());
		assertTrue(((LineWriterWrapper)lineWriter).getWriter() instanceof VbByteWriter);
		lineWriter.close();
		
		
		lineWriter = ioProvider.getLineWriter(BasicFileSchema.newFixedSchema(Constants.IO_VB_DUMP), outfilename);
		System.out.println(lineWriter.getClass().getName());
		System.out.println(((LineWriterWrapper)lineWriter).getWriter().getClass().getName());
		assertTrue(((LineWriterWrapper)lineWriter).getWriter() instanceof VbDumpByteWriter);
		lineWriter.close();
		
		lineWriter = ioProvider.getLineWriter(BasicFileSchema.newFixedSchema(Constants.IO_TEXT_LINE), outfilename);
		System.out.println(lineWriter.getClass().getName());
		assertTrue(lineWriter instanceof TextLineWriter);
		lineWriter.close();
		
		lineWriter = ioProvider.getLineWriter(BasicFileSchema.newFixedSchema(Constants.IO_UNICODE_TEXT), outfilename);
		System.out.println(lineWriter.getClass().getName());
		assertTrue(lineWriter instanceof TextLineWriter);
		lineWriter.close();
	}

}
