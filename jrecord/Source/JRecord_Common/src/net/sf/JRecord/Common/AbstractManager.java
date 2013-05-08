package net.sf.JRecord.Common;

/**
 * Abstract Manager - Interface of a class that manages other classes
 *
 *
 * @author Bruce Martin
 *
 */
public interface AbstractManager {


    /**
     * Get the number of entries
     * @return The number of entries
     */
    public int getNumberOfEntries();

    /**
     * Get the name of the manager
     * @return name of the manager
     */
    public String getManagerName();

	/**
	 * get key (from the index)
	 * @param idx get key for index number
	 * @return the key value for the index
	 */
	public abstract int getKey(int idx);

	/**
	 * get the name of managed class (from the inex)
	 * @param idx get name for index number
	 * @return the name for the index
	 */
	public abstract String getName(int idx);

}