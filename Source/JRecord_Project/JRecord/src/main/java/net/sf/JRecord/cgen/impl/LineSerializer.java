package net.sf.JRecord.cgen.impl;

import net.sf.JRecord.Common.IGetData;
import net.sf.JRecord.cgen.def.ILineToBytes;
import net.sf.JRecord.cgen.def.ISerializer;

public class LineSerializer<Line> implements ISerializer<Line> {
	
	public static <Line> LineSerializer<Line> create(ILineToBytes<Line> jrLine) {
		return new LineSerializer<Line>(jrLine);
	}
	
	private final ILineToBytes<Line> jrLine;
	
	protected LineSerializer(ILineToBytes<Line> jrLine) {
		super();
		this.jrLine = jrLine;
	}

	@Override	 
	public byte[] serialize(Line rec) {
	        
        if (rec instanceof IGetData) {
            return ((IGetData) rec).getData();
        }
        
        jrLine.set(rec);
        return jrLine.getData();
    }
}
