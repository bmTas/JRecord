/**
 *
 */
package net.sf.JRecord.Common;


/**
 * A basic Row consisting of an integer key and a String field name.
 * It is used return Key / Value pairs to the RecordEditor from JRecord.
 *
 * @author Bruce Martin
 *
 */
public class BasicKeyedField implements AbsRow {

	public int key;
	public String name;
	public Boolean valid=null;
	/**
	 * @see net.sf.JRecord.Common.AbsRow#getField(int)
	 */
	@Override
	public Object getField(int fldNum) {
		switch (fldNum) {
		case (0): return Integer.valueOf(key);
		case (1): return name;
		case (3): return valid;
		default: return null;
		}
	}

}
