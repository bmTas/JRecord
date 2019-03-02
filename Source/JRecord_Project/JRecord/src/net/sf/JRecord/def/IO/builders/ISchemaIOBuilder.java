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

package net.sf.JRecord.def.IO.builders;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import net.sf.JRecord.ByteIO.IByteRecordReader;
import net.sf.JRecord.ByteIO.IByteRecordWriter;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;


/**
 * This interface defines a minimal IOBuilder. It is the parent of all other IOBuilders
 * and is used to create a IOBuilder from an existing schema
 * <pre>
 *     JRecordInterface1.SCHEMA.newIOBuilder(existingSchema);
 * </pre>
 * 
 * @author Bruce Martin
 *
 */
public interface ISchemaIOBuilder extends INewLineCreator {

	/**
	 * Create new empty Line 
	 * 
	 * @return the new Line
	 * 
	 * @throws IOException
	 */
	public abstract AbstractLine newLine() throws IOException;

	/**
	 * Create line for supplied data
	 * 
	 * @param data data to be store in the line
	 * 
	 * @return new line
	 */
	public abstract AbstractLine newLine(byte[] data) throws IOException;

	/**
	 * 
	 * @return the layout or File-Schema (File Description)
	 */
	public abstract LayoutDetail getLayout() throws	IOException;

	/**
	 * Create a new LineReader for a specified file
	 * 
	 * @param filename name of the file to create the reader for
	 * @return Requested LineReader
	 *<pre>
	 *<b>Example:</b>
	 *   
     *   AbstractLineReader reader = JRecordInterface1.COBOL
     *           .newIOBuilder("file-name")
     *               .setFileOrganization(Constants.IO_FIXED_LENGTH)
     *           .<b>newReader("Data-Filename")</b>;
     *              
     *   while ((l = reader.read()) != null) { ... }
     *   reader.close()
     *</pre>
	 * 
	 * @throws FileNotFoundException
	 * @throws IOException anyIoexception that occurs
	 */
	public abstract AbstractLineReader newReader(String filename)
			throws FileNotFoundException, IOException;

	/**
	 * Create a new LineReader for a supplied input stream
	 * 
	 * @param datastream input datastream
	 * @return Requested LineReader
	 * <pre>
	 *<b>Example:</b>
     *      AbstractLineReader reader = JRecordInterface1.COBOL
     *              .newIOBuilder("file-name")
     *                  .setFileOrganization(Constants.IO_FIXED_LENGTH)
     *              .<b>newReader(dataStream)</b>;
     *              
     *      while ((l = reader.read()) != null) { ... }
     *      reader.close()
     * </pre>
	 * @throws IOException 
	 */
	public abstract AbstractLineReader newReader(InputStream datastream)
			throws IOException;

	/**
	 * Create a new LineReader for a user written IByteRecordreader.
	 * A Byte-Record-Reader can be used to Wrap an external data store e.g.
	 * Mainframe ZFile, Google-File-Store, MQ-Messages etc
	 * 
	 * @param byteReader user written Byte-Record-Reader.
	 * 
	 * @return requested LineReader
	 * 
	 * <pre>
	 *<b>Example:</b>
     *      AbstractLineReader reader = JRecordInterface1.COBOL
     *              .newIOBuilder("file-name")
     *                  .setFileOrganization(Constants.IO_FIXED_LENGTH)
     *              .<b>newReader(byteRecordReader)</b>;
     *              
     *      while ((l = reader.read()) != null) { ... }
     *      reader.close()
     * </pre>
     * 
	 * @throws IOException 
	 */
	public abstract AbstractLineReader newReader(IByteRecordReader byteReader)
			throws IOException;

	
	/**
	 * Create LineWriter for a supplied filename
	 * 
	 * @param filename output filename
	 * @return Requested LineWriter
	 * <pre>
	 *      
     *      ICobolIOBuilder ioBldr = RecordInterface1.COBOL
     *                             .newIOBuilder("CoboolCopybook)
     *                                 .setFileOrganization(Constants.IO_FIXED_LENGTH);
     *      LaytoutDetail schema = ioBldr.getLayout();
     *      AbstractLineWriter writer = ioBldr.<b>newWriter("DataFileName")</b>;
     *      Line line = new Line(schema);
     *      
     *      line.getFieldValue("fieldName").set("Field Value");
     *      ....
     *      writer.write(line);
     *      ...
     *      
     *      writer.close
     *
     *</pre>
     *
	 * 
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	public abstract AbstractLineWriter newWriter(String filename)
			throws FileNotFoundException, IOException;

	/**
	 * Create LineWriter for a supplied stream
	 * @param datastream output stream where the file is going to be written
	 * @return the Requested LineWriter
	 * 
	 * <pre>
	 *      
     *      ICobolIOBuilder ioBldr = RecordInterface1.COBOL
     *                             .newIOBuilder("CoboolCopybook)
     *                                 .setFileOrganization(Constants.IO_FIXED_LENGTH);
     *      LaytoutDetail schema = ioBldr.getLayout();
     *      AbstractLineWriter writer = ioBldr.<b>newWriter(dataStream)</b>;
     *      Line line = new Line(schema);
     *      
     *      line.getFieldValue("fieldName").set("Field Value");
     *      ....
     *      writer.write(line);
     *      ...
     *      
     *      writer.close
     *
     *</pre>
	 * 
	 * @throws IOException
	 */
	public abstract AbstractLineWriter newWriter(OutputStream datastream)
			throws IOException;

	/**
	 * Create LineWriter for a supplied Byte-Record-Writer.
	 * @param writer supplied Byte-Record-Writer.
	 * @return JRecord Line-Writer
	 * @throws IOException
	 */
	public abstract AbstractLineWriter newWriter(IByteRecordWriter writer)
			throws IOException;

}
