package net.sf.JRecord.cbl2xml.zTest.example.renameFields;

import java.util.HashMap;

import net.sf.JRecord.fieldNameConversion.IRenameField;

public class RenameFieldsMap implements IRenameField {
	private final HashMap<String, String> fieldNameMap = new HashMap<String, String>();
	
	
	public RenameFieldsMap add(String cobolName, String javaName) {
		fieldNameMap.put(cobolName.toUpperCase(), javaName);
		
		return this;
	}
	
	@Override
	public String toFieldName(String schemaFieldName) {
		String ret = fieldNameMap.get(schemaFieldName.toUpperCase());
		return ret == null ? schemaFieldName : ret ;
	}

}
