package net.sf.JRecord.schema.jaxb.impl;

import net.sf.JRecord.schema.jaxb.interfaces.IFormatField;

public class StandardFieldFormats {
	public static final StandardFieldFormats INSTANCE = new StandardFieldFormats();
	
	public IFormatField zeroPad = new ZeroPad();
}
