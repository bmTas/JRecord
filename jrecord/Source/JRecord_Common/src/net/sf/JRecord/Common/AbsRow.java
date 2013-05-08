/*
 * Created on 15/01/2005
 *
 */
package net.sf.JRecord.Common;

/**
 * This is an abstract (interface) definition of a table Row
 * (or Record). It allows access to the records fields via the Field number
 * It is used by most of the swing utilities in the RecordEditor
 */
public interface AbsRow {

	/**
	 * Get the value of a field (by field number)
	 *
	 * @param fldNum Field number
	 * @return value of the field
	 */
	Object getField(int fldNum);

}
