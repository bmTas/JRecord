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
	private final ArrayElement arrayDetails;
	public final String javaType;


	public FieldDef(String cobolName, FieldDetail fieldDef, ArrayElement ai) {
		super(cobolName);
		this.fieldDetail = fieldDef;
		this.arrayDetails = ai;
		this.javaType = CCode.typeToJavaType(fieldDetail.getType(), fieldDetail.getLen(), fieldDetail.getDecimal());
	}

	/**
	 * @return the fieldDef
	 */
	public final FieldDetail getFieldDetail() {
		return fieldDetail;
	}
	
	/**
	 * @return the arrayDetails
	 */
	public final ArrayElement getArrayDetails() {
		return arrayDetails;
	}
	
	public boolean isArrayItem() {
		return arrayDetails != null;
	}
	
	public boolean alwaysIncludeField() {
		return arrayDetails == null || arrayDetails.isSpecial();
	}

	public final String getJRecordTypeId() {
		return CCode.getJRecordTypeName(fieldDetail.getType());
	}

	/**
	 * @return the javaType
	 */
	public final String getJavaType() {
		return javaType;
	}

	public final String getAsType() {
		return "as" + javaType.substring(0, 1).toUpperCase() + javaType.substring(1);
	}

	public final String getFieldInitialise() {
		String ret = "0";
		if ("String".equals(javaType)) {
			ret = "\"\"";
		} else if (javaType.startsWith("Big")) {
			ret = javaType + ".ZERO";
		}
		return ret;
	}

}
