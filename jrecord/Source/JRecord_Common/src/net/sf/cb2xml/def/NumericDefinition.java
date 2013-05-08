/**
 * 
 */
package net.sf.cb2xml.def;

/**
 * Provides an interface for defining the numeric characteristics
 * of version of Cobol being used. This will allow cb2xml to be used with various
 * PC Cobol's
 * 
 * 
 * @author Bruce Martin
 *
 */
public interface NumericDefinition {


    /**
     * Get the conversion Name
     * @return conversion Name
     */
    public abstract String getName();

    public abstract int getBinarySize(String usage, int numDigits, boolean positive, boolean sync);
    
	public abstract int chkStorageLength(int storageLength, String usage);

    public abstract int getSyncAt(String usage, int actualLength);
}
