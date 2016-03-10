package net.sf.JRecord.def.IO.builders;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

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

}
