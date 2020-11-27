package net.sf.JRecord.zTest.Cobol.iobuilder;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.ByteIO.IByteRecordReader;
import net.sf.JRecord.ByteIO.IByteRecordWriter;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;


/**
 * Test the IByteRecordReader and IByteRecordWriter interfaces
 * 
 * @author Bruce Martin
 *
 */
public class TstByteRecordIO extends TestCase {
	String cpy
		= "          03  num-1      pic 9999.\n"
		+ "          03  PicX-2     pic x(6).\n";
	
	String[] lines = {
			"0123 abcde",
			"0456 asdfg",
			"9876 qwert",
			"9988 poiuy",
			"7890 qqwwe",
	};
	
	public void testReading() throws IOException {
		AbstractLineReader reader = JRecordInterface1.COBOL
			.newIOBuilder(new StringReader(cpy), "rec1")
				.newReader(new ByteRecReader(lines));
		
		AbstractLine line;
		int i = 0;
		while ((line = reader.read()) != null) {
			assertEquals(Integer.parseInt(lines[i].substring(0, 4)), line.getFieldValue(0, 0).asInt());
			assertEquals(lines[i].substring(4), line.getFieldValue(0, 1).asString());
			
			i += 1;
		}
		reader.close();

		assertEquals(5, i);
	}
	
	public void testWriting() throws IOException {
		ICobolIOBuilder iob = JRecordInterface1.COBOL
				.newIOBuilder(new StringReader(cpy), "rec1");
		ByteRecWriter recWriter = new ByteRecWriter();
		AbstractLineWriter writer = iob	.newWriter(recWriter);
		AbstractLine line = iob.newLine();
		
		for (String s : lines) {
			line.getFieldValue(0, 0).set(Integer.parseInt(s.substring(0, 4)));
			line.getFieldValue(0, 1).set(s.substring(4));

			writer.write(line);
		}
		writer.close();
		
		for (int i = 0; i < lines.length; i++) {
			assertEquals(lines[i], recWriter.list.get(i));
		}
		assertEquals(lines.length, recWriter.list.size());
	}
	
	private static class ByteRecReader implements IByteRecordReader {
		final String[] lines;
		int lineNo = 0;
		
		public ByteRecReader(String[] lines) {
			super();
			this.lines = lines;
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.ByteIO.IByteRecordReader#read()
		 */
		@Override
		public byte[] read() throws IOException {
			
			if (lines.length > lineNo) {
				return lines[lineNo++].getBytes();
			}
			return null;
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.ByteIO.IByteRecordReader#close()
		 */
		@Override
		public void close() throws IOException {
		}
	}
	
	private static class ByteRecWriter implements IByteRecordWriter {
		ArrayList<String> list = new ArrayList<String>();
		
		/* (non-Javadoc)
		 * @see net.sf.JRecord.ByteIO.IByteRecordWriter#write(byte[])
		 */
		@Override
		public void write(byte[] bytes) throws IOException {
			list.add(new String(bytes));
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.ByteIO.IByteRecordWriter#close()
		 */
		@Override
		public void close() throws IOException {
			
		}
		
	}
}
