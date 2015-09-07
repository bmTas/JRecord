package net.sf.JRecord.cg.schema;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.cg.common.CCode;

/**
 * Class to describe one field in the file. It is used for Code generation 
 * @author Bruce Martin
 *
 */
public class FieldDef extends JavaDetails {
	private final FieldDetail fieldDetail;

	public FieldDef(String cobolName, FieldDetail fieldDef) {
		super(cobolName);
		this.fieldDetail = fieldDef;
	}

	/**
	 * @return the fieldDef
	 */
	public final FieldDetail getFieldDetail() {
		return fieldDetail;
	}
	
	public final String getJRecordTypeId() {
		return CCode.getJRecordTypeName(fieldDetail.getType());
	}
}
