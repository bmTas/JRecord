/**
 * 
 */
package net.sf.JRecord.Types.smallBin;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;

/**
 * Short Binary Integer (Little Endian)
 * 
 * @author Bruce Martin
 *
 */
public class TypeIntLittleEndian extends TypeBaseXBinary {

	private final boolean normal;
	/**
	 * @param positive
	 */
	public TypeIntLittleEndian(boolean positive, boolean uInt) {
		super(positive || uInt, true, true);
		this.normal = ! uInt;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.smallBin.ITypeBinaryExtendedNumeric#asUnscaledLong(byte[], int, net.sf.JRecord.Common.IFieldDetail)
	 */
	@Override
	public long asUnscaledLong(byte[] record, int position, IFieldDetail field) {
		int len = field.getLen();
	    int pos = position + len - 2;	
		if (record.length <= pos) {
			throw new RecordException("Invalid int (little endian, record is to short: " + field.getName());
		}
		
		long v = normal && (record[pos] < 0) ? -1 : 0;
		
		switch (len) {
		case 1: 
			if (normal) { return record[pos]; }
			return record[pos] & 0xFF;
		case 8: v = v << 8 | (record[pos--] & 0xFF);
		case 7: v = v << 8 | (record[pos--] & 0xFF); 
		case 6: v = v << 8 | (record[pos--] & 0xFF);
		case 5: v = v << 8 | (record[pos--] & 0xFF);
		case 4: v = v << 8 | (record[pos--] & 0xFF);
		case 3: v = v << 8 | (record[pos--] & 0xFF);
		case 2: v = (v << 8 | (record[pos--] & 0xFF)) << 8 | record[pos] & 0xFF;
		}

		return v;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Types.smallBin.ITypeBinaryExtendedNumeric#setUnscaledLong(byte[], int, net.sf.JRecord.Common.IFieldDetail, long)
	 */
	@Override
	public byte[] setUnscaledLong(byte[] record, int position, IFieldDetail field, long value) {
	    int pos = position - 1;	
		int len = field.getLen();
		if (record == null || record.length < pos + len) {
			throw new RecordException("Invalid Binary Field, record is to short: " + field.getName());
		}
		
		switch (len) {
		case 8: 
			record[pos++] = (byte) (value & 0xFF);
			value = value >> 8;
		case 7:  
			record[pos++] = (byte) (value & 0xFF);
			value = value >> 8;
		case 6:  
			record[pos++] = (byte) (value & 0xFF);
			value = value >> 8;
		case 5:  
			record[pos++] = (byte) (value & 0xFF);
			value = value >> 8;
		case 4:  
			record[pos++] = (byte) (value & 0xFF);
			value = value >> 8;
		case 3: 
			record[pos++] = (byte) (value & 0xFF);
			value = value >> 8;
		case 2:
			record[pos++] = (byte) (value & 0xFF);
			value = value >> 8;
		case 1: 
			record[pos]   = (byte) (value & 0xFF);
		}
		
		return record;
	}

}
