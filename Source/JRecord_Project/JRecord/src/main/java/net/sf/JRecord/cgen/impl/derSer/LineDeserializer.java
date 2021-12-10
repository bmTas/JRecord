package net.sf.JRecord.cgen.impl.derSer;

import net.sf.JRecord.cgen.def.IDeserializer;
import net.sf.JRecord.cgen.defJr.IAsPojoSetData;

public class LineDeserializer<Line> implements IDeserializer<Line> {

	public static <Line> LineDeserializer<Line> create(IAsPojoSetData<Line> line)  {
		return new LineDeserializer<Line>(line);
	}
	private final IAsPojoSetData<Line> pojoConverter;
	
	
	protected LineDeserializer(IAsPojoSetData<Line> pojoConverter) {
		super();
		this.pojoConverter = pojoConverter;
	}


	@Override
	public Line deserialize(byte[] rec) {
		pojoConverter.setData(rec);
		return pojoConverter.asPojo();
	}

}
