package net.sf.JRecord.zTest.External.test.data;



import static org.junit.jupiter.api.Assertions.assertEquals;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.External.base.IChildRecord;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.constantNames.ConstantNames;

public class FieldTestDetails {


	public final String name;
	public final int position, length, decimal, type;
	
	public FieldTestDetails(String name, int position, int length, int decimal, int type) {
		super();
		this.name = name;
		this.position = position;
		this.length = length;
		this.decimal = decimal;
		this.type = type;
	}
	
	public void checkExternalField(String id, ExternalField field) {
		assertEquals(name, field.getName());
		id = id + ": " + name;
		assertEquals(position, field.getPos(), id);
		assertEquals(decimal, field.getDecimal());
		assertEquals(type, field.getType(), id);
	}
	
	public void checkField(FieldDetail field) {
		assertEquals(name, field.getName());
		assertEquals(position, field.getPos());
		assertEquals(decimal, field.getDecimal());
		int fieldType = field.getType();
		if (type == fieldType || TypeManager.getInstance().getShortType(type, field.getLen(), field.getFontName()) == fieldType) {
			
		} else {
			assertEquals(type, field.getType());
		}
	}
	
	
	public static void printExternalField(ExternalField field) {
		printExternalField(field, "\t");
	}
	
	public static void printRecord(ExternalRecord externalRecord) {
		System.out.println("\t{");
		for (int i = 0; i < externalRecord.getChildRecordCount(); i++) {
			IChildRecord<ExternalRecord> childRecordDtls = externalRecord.getChildRecord(i);
			ExternalRecord childRecord = childRecordDtls.getExternalRecord();
			for (int fieldNumber = 0; fieldNumber < childRecord.getNumberOfRecordFields(); fieldNumber ++ ) {
				FieldTestDetails.printExternalField(childRecord.getRecordField(fieldNumber), "\t\t");
			}
			System.out.println("\t}, {");
		}
	}

	
	public static void printExternalField(ExternalField field, String indent) {
		System.out.println(indent + "new FieldTestDetails(\"" + field.getName() + "\", "
				+ field.getPos() + ", "  + field.getLen() + ", " + field.getDecimal()
				+ ", " + ConstantNames.getTypeNames().getJRecordConstantName(field.getType()) + "),");

	}
}
