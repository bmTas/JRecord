package net.sf.JRecord.cgen.impl.derSer;

import net.sf.JRecord.Common.IGetData;
import net.sf.JRecord.cgen.def.ISerializer;

public class ByteLineSerializer<Line extends IGetData> implements ISerializer<Line> {
	
	@Override	 
	public byte[] serialize(Line rec) {
        return rec.getData();
    }
}
