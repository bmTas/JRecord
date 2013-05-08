package net.sf.JRecord.Types;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;

public class TypeCharRestOfFixedRecord extends TypeChar {

	public TypeCharRestOfFixedRecord() {
		super(true, true, false);
	}

	/**
	 * @see net.sf.JRecord.Types.TypeChar#getFieldEnd(net.sf.JRecord.Common.FieldDetail, byte[])
	 */
	@Override
	protected int getFieldEnd(IFieldDetail currField, byte[] record) {
		return record.length;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.TypeChar#setField(byte[], int, net.sf.JRecord.Common.FieldDetail, java.lang.Object)
	 */
	@Override
	public byte[] setField(byte[] record, int position, IFieldDetail field,
			Object value) throws RecordException {
        String val  = value.toString();
		String font = field.getFontName();
		int pos = position - 1;
		int len = record.length - pos;


	    byte[] byteVal = getBytes(val, font);
		if (val.length() >= len) {
			System.arraycopy(byteVal, 0, record, pos, len);
		} else {
			System.arraycopy(byteVal, 0, record, pos, val.length());
			//padWith(record, pos + val.length(), len - val.length(), " ", font);
			padByte(record, pos + val.length(), len - val.length(), getPadByte(font));
		}
		return record;
	}


}
