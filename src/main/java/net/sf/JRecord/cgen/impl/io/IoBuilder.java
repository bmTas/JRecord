package net.sf.JRecord.cgen.impl.io;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import net.sf.JRecord.ByteIO.ByteIOProvider;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDecider;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.cgen.def.IReader;
import net.sf.JRecord.cgen.def.IWriter;
import net.sf.JRecord.cgen.defJr.IPojoConverter;
import net.sf.JRecord.def.IO.builders.IIOBuilder;
import net.sf.JRecord.def.IO.builders.Icb2xmlLoadOptions;


/**
 * This class is a IOBuilder class for an arbitrary
 * Pojo representation of a Cobol Record. It is used in 
 * Code Generated by the CodeGen sub project.
 * 
 * @author Bruce Martin
 *
 * @param <Pojo>
 */
public class IoBuilder<Pojo> {

	
	private final IPojoConverter<Pojo> pojoConverter;
	private final boolean useByteIo;
	private final IIOBuilder builder;
	private final Icb2xmlLoadOptions recordSelectionDtls;
	private final LayoutDetail schema;
	
	public <T extends IIOBuilder> IoBuilder(IPojoConverter<Pojo> pojoConverter, T builder) throws IOException {
		this.pojoConverter = pojoConverter;
		
		this.builder = builder;
		this.recordSelectionDtls = builder instanceof Icb2xmlLoadOptions ? (Icb2xmlLoadOptions) builder : null;
		this.schema = builder.getLayout();
		this.useByteIo = ByteIOProvider.getInstance().getByteReader(schema) != null;
	}

	public LayoutDetail getLayout() {
		return schema;
	}

	/**
	 * Create new Reader for a file
	 * @param filename name of file to be read
	 * @return requested Reader
	 * 
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	public IReader<Pojo> newReader(String filename) throws FileNotFoundException, IOException {
		if (useByteIo) {
			ReadFromBytes<Pojo> r =  new ReadFromBytes<Pojo>(schema, pojoConverter);
			r.open(filename);
			return r;
		}
		
		return new ReadLine<Pojo>(builder.newReader(filename), pojoConverter);
	}
	
	

	/**
	 * Create new Reader for a stream
	 * @param in Stream to be read
	 * @return Requested reader
	 * 
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	public IReader<Pojo> newReader(InputStream in) throws FileNotFoundException, IOException {
		if (useByteIo) {
			ReadFromBytes<Pojo> r =  new ReadFromBytes<Pojo>(schema, pojoConverter);
			r.open(in);
			return r;
		}
		
		return new ReadLine<Pojo>(builder.newReader(in), pojoConverter);
	}
	
	/**
	 * Create an stream (of pojo's) from the input file of Cobol data.
	 * @param filename file name to be read and be presented as a stream
	 * @return stream (of pojo's) for further processing
	 * @throws IOException any error that occurs
	 */
	public Stream<Pojo> stream(String filename) throws IOException {
		return stream(new FileInputStream(filename));
    }



	/**
	 *  Create an stream (of pojo's) from the input stream.
	 * @param in input-stream containing the Cobol-data
	 * @return stream (of pojo's) for further processing
	 * @throws IOException any error that occurs
	 */
   	public Stream<Pojo> stream(InputStream in) throws IOException {
		return StreamSupport.stream(
				Spliterators.spliteratorUnknownSize(
						new PojoIterator<Pojo>(newReader(in)), 
						Spliterator.IMMUTABLE | Spliterator.NONNULL), 
				false);
	}

	/**
	 * Create Writer for the file
	 * @param filename file to be written
	 * @return requested writer
	 * 
	 * @throws IOException
	 */
	public IWriter<Pojo> newWriter(String filename) throws IOException {
		if (useByteIo) {
			WriteAsBytes<Pojo> w =  new WriteAsBytes<Pojo>(schema, pojoConverter);
			w.open(filename);
			return w;
		}
		
		return new WriteLine<Pojo>(builder.newWriter(filename), pojoConverter, builder.newLine());
	}
	
	/**
	 * Create a writer for an Output Stream
	 * @param out output stream
	 * @return requested writer
	 * 
	 * @throws IOException
	 */
	public IWriter<Pojo> newWriter(OutputStream out) throws IOException {
		if (useByteIo) {
			WriteAsBytes<Pojo> w =  new WriteAsBytes<Pojo>(schema, pojoConverter);
			w.open(out);
			return w;
		}
		
		return new WriteLine<Pojo>(builder.newWriter(out), pojoConverter, builder.newLine());
	}
	
	/**
	 * Set / update Record Selection criteria
	 * 
	 * @param recordName Record Name
	 * @param selectionCriteria Record Selection criteria
	 * @return this IOBuilder
	 */
	public IoBuilder<Pojo> setRecordSelection(String recordName, ExternalSelection selectionCriteria) {
		checkRecordSelection("setRecordSelection");
		recordSelectionDtls.setRecordSelection(recordName, selectionCriteria);
		
		return this;
	}

	/**
	 * 
	 */
	protected void checkRecordSelection(String method) {
		if (recordSelectionDtls == null) {
			throw new RecordException("method " + method + " is not available for IoBuilder " + builder.getClass().getName());
		}
	}

	/**
	 * Set / Update the RecordDecider
	 * @param recordDecider Record decider
	 * @return this IoBuilder
	 */
	public IoBuilder<Pojo> setRecordDecider(RecordDecider recordDecider) {
		checkRecordSelection("setRecordDecider");
		recordSelectionDtls.setRecordDecider(recordDecider);
		
		return this;
	}


	/**
	 * Wrap a JRecord LineReader up as a Pojo Reader
	 * 
	 * @author Bruce Martin
	 *
	 * @param <Pojo>
	 */
	private static class ReadLine<Pojo> implements IReader<Pojo> {

		final AbstractLineReader reader;
		private final IPojoConverter<Pojo> pojoConverter;	/* Used to convert a JRecord line to a Pojo */
		
		
		public ReadLine(AbstractLineReader reader, IPojoConverter<Pojo> pojoConverter) {
			super();
			this.reader = reader;
			this.pojoConverter = pojoConverter;
		}

		@Override
		public Pojo read() throws IOException {
			AbstractLine line = reader.read();
			
			return line == null ? null : pojoConverter.toPojo(line);
		}

		@Override
		public void close() throws IOException {
			reader.close();
		}
	}
	
	
	/**
	 * This class wraps JRecord LineReader up as a Pojo Writer
	 * @author bruce
	 *
	 * @param <Pojo>
	 */
	private static class WriteLine<Pojo> implements IWriter<Pojo> {
		final AbstractLineWriter writer;
		private final IPojoConverter<Pojo> pojoConverter; /* used to convert a Pojo into a JRecord line */ 
		final AbstractLine l;
		
		
		public WriteLine(AbstractLineWriter writer, IPojoConverter<Pojo> pojoConverter, AbstractLine line) {
			super();
			this.writer = writer;
			this.pojoConverter = pojoConverter;
			this.l = line;
		}

		@Override
		public void write(Pojo pojo) throws IOException {
			pojoConverter.updateLine(l, pojo);
			writer.write(l);
		}

		@Override
		public void close() throws IOException {
			writer.close();
		}
	}
}
