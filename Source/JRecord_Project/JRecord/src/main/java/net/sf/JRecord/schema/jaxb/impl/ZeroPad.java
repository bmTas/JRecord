package net.sf.JRecord.schema.jaxb.impl;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.schema.jaxb.IItem;
import net.sf.JRecord.schema.jaxb.interfaces.IFormatField;

/**
 * Pad field with leading Zero's
 * @author Bruce Martin
 *
 */
public class ZeroPad implements IFormatField {

	@Override
	public String format(IItem itemDef, IFieldDetail fieldDef, String value) {
		int displayLen = itemDef.getDisplayLength();
		String sign = "";
		value = value == null ? "" : value;
		
		if (value.startsWith("+") || value.startsWith("-")) {
			sign = value.substring(0, 1);
			value = value.substring(1);
		}
		StringBuilder b = new StringBuilder(displayLen);
		b.append(sign);
		for (int i = displayLen - value.length() - sign.length(); i> 0; i--) {
			b.append(0);
		}
	
		return b.append(value).toString();
	}

}
