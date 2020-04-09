package net.sf.JRecord.Types.smallBin;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Types.Type;

/**
 * 
 * Numeric types that support un-scaled-long operations
 * 
 * Typically these are:<ul>
 * <li>Binary (comp/comp-5) fields <= 8 bytes
 * <li>Packed decimal fields <= 9 bytes
 * </ul>
 * 
 * These types can be represented as a long
 * 
 * @author Bruce Martin
 *
 */
public interface ITypeBinaryExtendedNumeric extends Type {

	/**
	 * Get the field value as an un-scaled long
	 * @param record record to be updated
	 * @param position position in the record of the field
	 * @param field Field definition
	 * @return field value as an un-scaled long
	 */
	public long asUnscaledLong(
			byte[] record,
			int position,
			IFieldDetail field);
	
	/**
	 * 
	 * @param record record to be updated
	 * @param position position in the record of the field
	 * @param field Field definition
	 * @param value new field value
	 * @return updated record
	 */
	public byte[] setUnscaledLong(byte[] record,
			  int position,
			  IFieldDetail field, 
			  long value);
}
