package net.sf.JRecord.cgen.def;

/**
 * Get the array size for a particalar dimension
 * @author Bruce Martinm
 *
 */
public interface IArraySize {

	/**
	 * Get the number of Array index's
	 * @return number of Array index's
	 */
	public abstract int getIndexCount();

	/**
	 * get the array size for a particular dimension
	 * @param indexNumber index number
	 * @return request array size
	 */
	int getArrayLength(int indexNumber);

}