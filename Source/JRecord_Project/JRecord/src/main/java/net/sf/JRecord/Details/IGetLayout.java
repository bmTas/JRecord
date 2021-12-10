package net.sf.JRecord.Details;

import java.io.IOException;

/**
 * Any class or Interface that can return LayoutDetail (or file schema)
 * 
 * @author Bruce Martin
 *
 */
public interface IGetLayout {
	/**
	 * 
	 * @return the layout or File-Schema (File Description)
	 */
	public abstract LayoutDetail getLayout() throws	IOException;

}
