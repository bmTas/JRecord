package net.sf.JRecord.cgen.impl.derSer;

import net.sf.JRecord.Details.IGetByteData;
import net.sf.JRecord.cgen.def.ISerializer;

public class GetBytesSerializer<RecType extends IGetByteData>  implements  ISerializer<RecType> {
	@Override public byte[] serialize(IGetByteData rec) {
		return rec.getData();
	}
}
