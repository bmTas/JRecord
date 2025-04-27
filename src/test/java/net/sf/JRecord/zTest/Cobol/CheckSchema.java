package net.sf.JRecord.zTest.Cobol;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.constantNames.ConstantNameConversion;
import net.sf.JRecord.constantNames.ConstantNames;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class CheckSchema {

	private final FieldDetail[][] expectedFields;
	private final String copybookName;
	
	public CheckSchema(String copybookName, FieldDetail[][] expectedFields) {
		super();
		this.copybookName = copybookName;
		this.expectedFields = expectedFields;
	}

	public void checkConversion(String copybook, String expectedXml) throws IOException, XMLStreamException, FactoryConfigurationError {
		ICobolIOBuilder iob = checkXml(copybook, expectedXml);
		LayoutDetail layout = iob.getLayout();
		
		
		//printExpectedFields(layout);
		
		if (expectedFields == null) {
			printExpectedFields(layout);
		} else {
			for (int recNum = 0; recNum < layout.getRecordCount(); recNum++) {
				RecordDetail record = layout.getRecord(recNum);
	
				for (int fieldNum = 0; fieldNum < record.getFieldCount(); fieldNum++) {
					compareFields(expectedFields[recNum][fieldNum], record.getField(fieldNum), 0);
				}
			}
		}

	}

	public void compareFields(FieldDetail expectedField, FieldDetail field, int diff) {
		assertEquals(expectedField.getName(), field.getName());
		assertEquals(expectedField.getType(), field.getType());
		assertEquals(expectedField.getPos() + diff, field.getPos());
		assertEquals(expectedField.getLen(), field.getLen());
		assertEquals(expectedField.getDecimal(), field.getDecimal());
		assertEquals(expectedField.getFontName(), field.getFontName());
	}

	//"EMPLOYEE-RECORD"
	protected ICobolIOBuilder checkXml(String copybook, String expectedXml)
			throws IOException, XMLStreamException, UnsupportedEncodingException, FactoryConfigurationError {
		ICobolIOBuilder iob = JRecordInterface1.COBOL.newIOBuilder(
				 JRecordInterface1.COBOL.newCobolCopybookReader()
				 		.setCopybookName(copybookName)
				 		.addFreeFormatCobolText(copybook))
						.setSplitCopybook(CopybookLoader.SPLIT_REDEFINE);
		StringWriter w = new StringWriter();
		JRecordInterface1.SCHEMA_XML
				.setIndentXml(true)
				.export(w, iob.getExternalRecord());
		assertEquals(expectedXml, w.toString());
		
		return iob;
	}

	public void printExpectedFields(LayoutDetail layout) {
		ConstantNameConversion typeNames = ConstantNames.getTypeNames();
		for (int recNum = 0; recNum < layout.getRecordCount(); recNum++) {
			RecordDetail record = layout.getRecord(recNum);

			System.out.println("\t}, {");
			for (int fieldNum = 0; fieldNum < record.getFieldCount(); fieldNum++) {
				FieldDetail field = record.getField(fieldNum);
				System.out.println(
						"\t\tFieldDetail.newFixedWidthField("
							+ "\"" + field.getName() + "\", "
							+ typeNames.getConstantDetails(field.getType()).getJRecordInterfaceConstant() + ", "
							//+ field.getType() + ", "
							+ field.getPos() + ", "
							+ field.getLen() + ", "
							+ field.getDecimal() + ", "
							+ "\"" + field.getFontName() + "\"),"
						);
			}
		}
		System.out.println("\t}");
	}

}
