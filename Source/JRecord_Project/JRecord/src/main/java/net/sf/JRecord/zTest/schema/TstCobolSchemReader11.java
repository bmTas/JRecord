package net.sf.JRecord.zTest.schema;

import static org.junit.Assert.*;

import java.io.IOException;
import java.io.StringReader;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.schema.ArrayElementChecks;
import net.sf.JRecord.schema.CobolSchemaDetails;
import net.sf.JRecord.schema.CobolSchemaReader;
import net.sf.JRecord.schema.IArrayItemCheck;
import net.sf.JRecord.schema.jaxb.IItem;
import net.sf.JRecord.schema.jaxb.Item;
import net.sf.JRecord.schema.jaxb.ItemRecordDtls;
import net.sf.JRecord.schema.jaxb.impl.StandardItemWriteChecks;
import net.sf.JRecord.schema.jaxb.impl.ZeroPad;
import net.sf.JRecord.schema.jaxb.interfaces.IFormatField;
import net.sf.JRecord.schema.jaxb.interfaces.IRedefineSelection;
import net.sf.JRecord.schema.jaxb.interfaces.IWriteCheck;

public class TstCobolSchemReader11 {

	static final String COBOL_REDEFINE = ""
			+ "           11  Group-Selector           pic x(01).\n" 
			+ "           11  Group-1.\n"
			+ "               15 Field-1-1             pic x(25).\n" 
			+ "           11  redefines Group-1.\n"
			+ "               15 NumField-2-1          pic s9(4). \n"
			+ "               15 NumField-2-2          pic s9(4)V99.\n"
			+ "               15 NumField-2-3          pic s9(4).\n"
			+ "           11  redefines Group-1.\n"
			+ "               15  field-30-1           pic 9(9).\n"
			 ;
	
	static final String COBOL_ARRAY = "           11  Array-Field   occurs 10  pic x(4).\n";
	static final String COBOL_GROUP = ""
			+ "           11  Group-Test.\n"
			+ "               15 G-Field-1             pic x(3).\n"
			+ "               15 G-Field-2             pic x(4).\n"
			+ "               15 G-Field-3             pic x(3)."
			
			;

	static final String COPYBOOK = "" 
			+ "      01  Redef-Test.\n"
			+        COBOL_REDEFINE
			+        COBOL_ARRAY
			+        COBOL_GROUP
			;
	static final String NESTED_COPYBOOK = "" 
			+ "      01  Redef-Test.\n"
			+ "          03 R11.\n"
			+ "             05 R12.\n"
			+ "                 07 R13.\n"
			+        COBOL_REDEFINE
			+ "          03 A11.\n"
			+ "             05 A12.\n"
			+ "                 07 A13.\n"
			+        COBOL_ARRAY
			+ "          03 T11.\n"
			+ "             05 T12.\n"
			+ "                 07 T13.\n"
			+        COBOL_GROUP
			;
	static final String DUPLICATE_FIELD_NAMES_COPYBOOK = "" 
			+ NESTED_COPYBOOK
			+ "          03 R21.\n"
			+ "             05 R12.\n"
			+ "                 07 R13.\n"
			+        COBOL_REDEFINE
			+ "          03 A11.\n"
			+ "             05 A22.\n"
			+ "                 07 A13.\n"
			+        COBOL_ARRAY
			+ "          03 T11.\n"
			+ "             05 T12.\n"
			+ "                 07 T23.\n"
			+        COBOL_GROUP
			;
	
	private IRedefineSelection redef = new IRedefineSelection() {
		@Override public List<IItem> selectRedefinedItemToWrite(List<IItem> redefinedItemGroup, AbstractLine line) {
			return redefinedItemGroup;
		}
		
	};
	private IArrayItemCheck skipSpaces = ArrayElementChecks.INSTANCE.newSkipSpaces();
	private ZeroPad zeroPad = new ZeroPad();
	private IWriteCheck skipLowValues = StandardItemWriteChecks.INSTANCE.skipLowValues();


	@Test
	public void testBasic() throws IOException {
		AnalyseSchema details = tstSimpleAssign(new SchemaReader(NESTED_COPYBOOK));
		
		assertEquals("[Redef-Test, A11, A12, A13, Array-Field]", details.arrayItem.getGroupNames().toString());
		assertEquals("[Redef-Test, R11, R12, R13, Group-1]", details.redefineItem.getGroupNames().toString());
		assertEquals("[Redef-Test, R11, R12, R13, , NumField-2-1]", details.formatItem.getGroupNames().toString());
		assertEquals("[Redef-Test, T11, T12, T13, Group-Test]", details.writeCheckItem.getGroupNames().toString());

	}

	@Test
	public void testNested() throws IOException {
		AnalyseSchema details = tstSimpleAssign(new SchemaReader(COPYBOOK));
		
		assertEquals("[Redef-Test, Array-Field]", details.arrayItem.getGroupNames().toString());
		assertEquals("[Redef-Test, Group-1]", details.redefineItem.getGroupNames().toString());
		assertEquals("[Redef-Test, , NumField-2-1]", details.formatItem.getGroupNames().toString());
		assertEquals("[Redef-Test, Group-Test]", details.writeCheckItem.getGroupNames().toString());

	}


	@Test
	public void testDuplicateNames1() throws IOException {
		SchemaReader sr = new SchemaReader(DUPLICATE_FIELD_NAMES_COPYBOOK);
		
		sr.setArrayCheck(Arrays.asList("A12", "Array-Field"), skipSpaces)
		  .setRedefineSelection(Arrays.asList("R11", "Group-1"), redef)
		  .setFormatField(Arrays.asList("R11", "NumField-2-1"), zeroPad)
		  .setWriteCheck(Arrays.asList("T13", "Group-Test"), skipLowValues)
		  ;
		
		AnalyseSchema details = checkExitAssignment(sr);
		
		assertEquals("[Redef-Test, A11, A12, A13, Array-Field]", details.arrayItem.getGroupNames().toString());
		assertEquals("[Redef-Test, R11, R12, R13, Group-1]", details.redefineItem.getGroupNames().toString());
		assertEquals("[Redef-Test, R11, R12, R13, , NumField-2-1]", details.formatItem.getGroupNames().toString());
		assertEquals("[Redef-Test, T11, T12, T13, Group-Test]", details.writeCheckItem.getGroupNames().toString());
	}


	@Test
	public void testDuplicateNames2() throws IOException {
		SchemaReader sr = new SchemaReader(DUPLICATE_FIELD_NAMES_COPYBOOK);
		
		sr.setArrayCheck(Arrays.asList("A22", "Array-Field"), skipSpaces)
		  .setRedefineSelection(Arrays.asList("R21", "Group-1"), redef)
		  .setFormatField(Arrays.asList("R21", "NumField-2-1"), zeroPad)
		  .setWriteCheck(Arrays.asList("T23", "Group-Test"), skipLowValues)
		  ;
		
		AnalyseSchema details = checkExitAssignment(sr);
		
		assertEquals("[Redef-Test, A11, A22, A13, Array-Field]", details.arrayItem.getGroupNames().toString());
		assertEquals("[Redef-Test, R21, R12, R13, Group-1]", details.redefineItem.getGroupNames().toString());
		assertEquals("[Redef-Test, R21, R12, R13, , NumField-2-1]", details.formatItem.getGroupNames().toString());
		assertEquals("[Redef-Test, T11, T12, T23, Group-Test]", details.writeCheckItem.getGroupNames().toString());
	}


	private AnalyseSchema tstSimpleAssign(SchemaReader sr) throws IOException {
		
		sr.setArrayCheck("Array-Field", skipSpaces)
		  .setRedefineSelection("Group-1", redef)
		  .setFormatField("NumField-2-1", zeroPad)
		  .setWriteCheck("Group-Test", skipLowValues)
		  ;
		
		return checkExitAssignment(sr);
	}
	

	private AnalyseSchema checkExitAssignment(SchemaReader sr) throws IOException {
		AnalyseSchema details = new AnalyseSchema( sr.getCobolSchemaDetails());
		
		//System.out.println(details.arrayItem.getGroupNames().toString()); 
		
		assertTrue(details.arrayItem.arrayValidation == skipSpaces);
		assertTrue(details.redefineItem.redefinesCheck == redef);
		IFormatField formatFieldImplementation = details.formatItem.formatFieldImplementation;
		assertTrue(formatFieldImplementation != null);
		assertTrue(formatFieldImplementation.getClass().getSimpleName(), formatFieldImplementation == zeroPad);
		assertTrue(details.writeCheckItem.writeCheck == skipLowValues);
		
		assertEquals(1, details.arrayCount);
		assertEquals(1, details.redefineCount);
		assertEquals(1, details.formatCount);
		assertEquals(1, details.writeCheckCount);
		return details;
	}
	
	private static class AnalyseSchema {
		int arrayCount = 0, redefineCount = 0, formatCount = 0, writeCheckCount = 0;
		Item arrayItem, redefineItem, formatItem, writeCheckItem;
		
		AnalyseSchema(CobolSchemaDetails schema) {
			for (ItemRecordDtls itemRecord : schema.recordItems ) {
				processItem(itemRecord.items);
			}
		}
		
		void processItem(List<Item> items) {
			for (Item item : items) {
				if (item.arrayValidation != null) {
					arrayCount += 1;
					arrayItem = item;
				}
				if (item.redefinesCheck != null) {
					redefineCount += 1;
					redefineItem = item;
				}
				if (item.formatFieldImplementation != null) {
					formatCount += 1;
					formatItem = item;
				}
				if (item.writeCheck != null) {
					writeCheckCount += 1;
					writeCheckItem = item;
				}
			
				processItem(item.getChildItems());
			}
			
			
		}
	}
	
	


	private static class SchemaReader extends CobolSchemaReader<SchemaReader> {

		private static final String REDEFINE_TST = "RedefineTst";

		protected SchemaReader(String cobolCopybook) {
			super(REDEFINE_TST, new CobolCopybookLoader());
			
			super.addCopyBook(new StringReader(cobolCopybook), REDEFINE_TST);
		}
		
	}
}
