package net.sf.JRecord.schema.jaxb.impl;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.schema.jaxb.IItem;
import net.sf.JRecord.schema.jaxb.interfaces.IWriteCheck;

/**
 * Class to test if the data is 
 * @author Bruce Martin
 *
 */
public class ItemByteCheck implements IWriteCheck {
	private final byte testByte;
	public ItemByteCheck(byte testByte) {
		super();
		this.testByte = testByte;
	}
	@Override
	public boolean isOkToWrite(IItem item, AbstractLine line) {

		int position = item.getPosition();
		byte[] data = line.getData(position, item.getStorageLength());
		for (int i = 0; i < data.length; i++) {
			if (data[i] != testByte) {
				return true;
			}
		}
		return false;
	}

}
