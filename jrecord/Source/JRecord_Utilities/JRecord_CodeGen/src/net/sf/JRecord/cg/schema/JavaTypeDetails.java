package net.sf.JRecord.cg.schema;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.cg.schema.classDefinitions.ClassDef;
import net.sf.JRecord.cg.schema.classDefinitions.IClassDef;
import net.sf.JRecord.cgen.support.Code2JRecordConstants;

public class JavaTypeDetails {
	public final String javaType, javaRawType;
	public final IClassDef classDef;
	
	public JavaTypeDetails(boolean isCsv, FieldDetail fieldDef) {
		this(isCsv, fieldDef.getType(), fieldDef.getCobolItem() == null ? null : fieldDef.getCobolItem().getJavaType(),fieldDef.getLen(), 
				fieldDef.getDecimal(), fieldDef.getFormat(), fieldDef.getParamater());
	}
	
	
	public JavaTypeDetails(boolean isCsv, int type, String typeName, int len, int decimal, int format, String param) {

		this.classDef = ClassDef.getClassDef(format, param);
		this.javaRawType = typeName == null || typeName.length() == 0
				? Code2JRecordConstants.typeToJavaType(isCsv, type, len, decimal)
				: typeName;
		this.javaType = classDef== null || classDef.getClassName() == null ? javaRawType : classDef.getClassName();
	}
}
