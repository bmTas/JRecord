package net.sf.JRecord.cg.nameConversion;

public interface IFieldNameConversion {
	
	String getConversionName();

	String toConstant(String b);

	String toSqlName(String str);

	String toSuffix(String str);

	String toFieldName(String b);

	String toClassName(String b);

	String toJavaId(boolean isCobol, String name);

	String cobolName2JavaName(String cobolName);

	String string2JavaId(String name);

	String toAdjCobolName(String cobolName, String copybookName);

}