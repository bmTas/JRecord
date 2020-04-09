package net.sf.JRecord.schema.fieldRename;

import java.util.HashMap;
import java.util.Map;

import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.fieldNameConversion.IRenameField;

public abstract class AbstractFieldLookup implements IGetRecordFieldByName {

	private Map<String, Integer> recordMap;
	private LayoutDetail schema;
	protected IRenameField renameField;
	
	protected AbstractFieldLookup() {
		this(StdFieldRenameItems.LEAVE_ASIS);
	}
	
	protected AbstractFieldLookup(IRenameField renameField) {
		this.renameField = renameField;
	}

	public LayoutDetail getSchema() {
		return schema;
	}

	@Override
	public void setSchema(LayoutDetail schema) {
		this.schema = schema;

		if (schema.getRecordCount() < 2) {
			recordMap = null;
		} else {
			recordMap = new HashMap<String, Integer>(schema.getRecordCount() * 2);
			for (int i = 0; i < schema.getRecordCount(); i++) {
				recordMap.put(renameField.toFieldName(schema.getRecord(i).getRecordName()).toLowerCase(), i);
			}
		}
	}

	@Override
	public int getRecordIndex(String name) {
		int r = 1;
		if (schema.getRecordCount() > 1) {
			String lcName = name.toLowerCase();
			if (recordMap.containsKey(lcName)) {
				r = recordMap.get(lcName);
			} else {
				r = schema.getRecordIndex(name);
			}
		}
		return r;
	}
}
