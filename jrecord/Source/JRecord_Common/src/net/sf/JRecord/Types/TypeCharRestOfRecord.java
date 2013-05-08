package net.sf.JRecord.Types;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;

public class TypeCharRestOfRecord extends TypeChar {

	public TypeCharRestOfRecord() {
		super(true, true, false);
	}

//	/* (non-Javadoc)
//	 * @see net.sf.JRecord.Types.TypeChar#getField(byte[], int, net.sf.JRecord.Common.FieldDetail)
//	 */
//	@Override
//	public Object getField(byte[] record, int position, FieldDetail currField) {
//		Object o = super.getField(record, position, currField);
//		//System.out.println(" ~~ " + position + " " + currField.getLen() + " - " + o);
//		return o;
//	}

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
		if (val.length() == len) {
			System.arraycopy(byteVal, 0, record, pos, len);
		} else {
			byte[] temp = new byte[pos + val.length()];
			System.arraycopy(record, 0, temp, 0, position);
			System.arraycopy(byteVal, 0, temp, pos, val.length());
			record = temp;
		}
		return record;
	}


}
