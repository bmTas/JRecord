package net.sf.JRecord.cgen.impl;

import java.io.IOException;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.cgen.def.IWriter;
import net.sf.JRecord.cgen.defJr.IUpdateLine;

/**
 * This class will write a Pojo to a file. t uses a IUpdateLine class
 * to convert the Pojo into a JRecord AbstractLine then uses a AbstractLineWriter
 * to write the line to the file.
 * 
 * @author Bruce Martin
 *
 * @param <Pojo>
 */
public class PojoLineWriter<Pojo> implements IWriter<Pojo> {
	private final AbstractLineWriter lineWriter;
	//private final LineRecBarDTAR020JR wrapper = new LineRecBarDTAR020JR();
	private final AbstractLine line;
	private final IUpdateLine<Pojo> copy;

	public PojoLineWriter(AbstractLineWriter lineWriter, AbstractLine line, IUpdateLine<Pojo> updateLine) {
		super();
		this.lineWriter = lineWriter;
		this.line = line;
		this.copy = updateLine;
	}
	
	@Override
	public void write(Pojo record) throws IOException {

	    copy.updateLine(line, record);
		
		lineWriter.write(line);
	}
	
	@Override
	public void close() throws IOException {
		lineWriter.close();
	}
}
