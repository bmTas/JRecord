package net.sf.JRecord.schema.jaxb.impl;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.schema.jaxb.IItem;
import net.sf.JRecord.schema.jaxb.interfaces.IFormatField;

public class AddPlusToNumeric implements IFormatField {
	
	public static final AddPlusToNumeric INSTANCE = new AddPlusToNumeric();

	/* (non-Javadoc)
	 * @see net.sf.JRecord.schema.jaxb.interfaces.IFormatField#format(net.sf.JRecord.schema.jaxb.IItem, net.sf.JRecord.Common.IFieldDetail, java.lang.String)
	 */
	@Override
	public String format(IItem itemDef, IFieldDetail fieldDef, String value) {
		if (TypeManager.isNumeric(fieldDef.getType()) 
		&&  TypeManager.isSignLeading(fieldDef.getType()) 
		&&  value != null && value.length() > 0 
		&& (value.charAt(0) != '-') && (value.charAt(0) != '+')) {
			value = '+' + value.trim();
		}
		return value;
	}

	
}
