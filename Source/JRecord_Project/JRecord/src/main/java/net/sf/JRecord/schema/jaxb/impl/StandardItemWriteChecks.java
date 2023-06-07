package net.sf.JRecord.schema.jaxb.impl;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.schema.jaxb.IItem;
import net.sf.JRecord.schema.jaxb.interfaces.IWriteCheck;

public class StandardItemWriteChecks {

	public static final StandardItemWriteChecks INSTANCE = new StandardItemWriteChecks();
	
	public IWriteCheck skipLowValues() { return new ItemByteCheck((byte) 0); }
	public IWriteCheck skipHighValues() { return new ItemByteCheck((byte) -1); }
	public IWriteCheck skipSpaces() { return new SpaceCheck(); }
	
	
	private static class SpaceCheck implements IWriteCheck {

		@Override
		public boolean isOkToWrite(IItem item, AbstractLine line) {
			Object fieldValue = line.getField(item.getFieldDefinition());
			String value = fieldValue == null ? "" : fieldValue.toString();
			return "".equals(value);
		}
		
	}
}
