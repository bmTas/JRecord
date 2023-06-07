package net.sf.JRecord.cbl2xml.zTest.cbl2xml.modifiers;

import static org.junit.Assert.*;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Collections;
import java.util.List;

import javax.xml.stream.XMLStreamException;

import org.junit.Test;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Option.JRecordConstantVars;
import net.sf.JRecord.cbl2xml.Cobol2Xml;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.schema.jaxb.IItem;
import net.sf.JRecord.schema.jaxb.interfaces.IRedefineSelection;

public class TstRedefineCheck {

	String copybook = ""
			+ "      01  Redefine-Test.\n"
			+ "           03  Group-Selector           pic x(01).\n"
			+ "           03  Group-1.\n"
			+ "               05 Field-1-1             pic x(25).\n"
			+ "           03  redefines Group-1.\n"
			+ "               05 NumField-2-1          pic s9(4). \n"
			+ "               05 NumField-2-2          pic s9(4)V99.\n"
			+ "               05 NumField-2-3          pic s9(4).\n"
			+ "           03  Group-3 redefines Group-1.\n"
			+ "               05  field-30-1           pic 9(9).";
	String file = ""
			+ "1asdfghjiklzxcvbnm        \n"
			+ "2000A00020{000C\n"
			+ "3000001234\n"
			+ "112345678901234567890  ";
	
	String result = ""
			+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<CobolData>\n"
			+ "    <Redefine-Test>\n"
			+ "        <Group-Selector>1</Group-Selector>\n"
			+ "        <Group-1>\n"
			+ "            <Field-1-1>asdfghjiklzxcvbnm</Field-1-1>\n"
			+ "        </Group-1>\n"
			+ "    </Redefine-Test>\n"
			+ "    <Redefine-Test>\n"
			+ "        <Group-Selector>2</Group-Selector>\n"
			+ "        <NumField-2-1>1</NumField-2-1>\n"
			+ "        <NumField-2-2>2.00</NumField-2-2>\n"
			+ "        <NumField-2-3>3</NumField-2-3>\n"
			+ "    </Redefine-Test>\n"
			+ "    <Redefine-Test>\n"
			+ "        <Group-Selector>3</Group-Selector>\n"
			+ "        <Group-3>\n"
			+ "            <field-30-1>1234</field-30-1>\n"
			+ "        </Group-3>\n"
			+ "    </Redefine-Test>\n"
			+ "    <Redefine-Test>\n"
			+ "        <Group-Selector>1</Group-Selector>\n"
			+ "        <Group-1>\n"
			+ "            <Field-1-1>12345678901234567890</Field-1-1>\n"
			+ "        </Group-1>\n"
			+ "    </Redefine-Test>\n"
			+ "</CobolData>"
		;
	
	@Test
	public void test() throws IOException, XMLStreamException {
		ICobol2Xml cbl2Xml = getCobol2Json()
				.setRedefineSelection("Group-1", new IRedefineSelection() {

					@Override
					public List<IItem> selectRedefinedItemToWrite(List<IItem> redefinedItemGroup, AbstractLine line) {
						String code = line.getFieldValue("Group-Selector").asString();
						IItem itm = redefinedItemGroup.get(2);
						if ("1".equals(code)) {
							itm = redefinedItemGroup.get(0);
						} else  if ("2".equals(code)) {
							itm = redefinedItemGroup.get(1);
						}
						
						return Collections.singletonList(itm);
					}			
				});
		
	    StringWriter w = new StringWriter();

		cbl2Xml.cobol2xml(new ByteArrayInputStream(file.getBytes()), w);
		
//		System.out.print(w.toString());
		
		assertEquals(result, w.toString());
	}

	
	
	private ICobol2Xml getCobol2Json() {
		JRecordConstantVars constants = Cobol2Xml.JR_CONSTANTS;
 	
       return Cobol2Xml.newCobol2Xml(new StringReader(copybook), "RedefineTst")

                                         // Cobol Options
                         .setFileOrganization(constants.IO_STANDARD_TEXT_FILE)
                         .setDialect(constants.FMT_MAINFRAME)               
                         .setSplitCopybook(constants.SPLIT_NONE)      
                         .setFont("")
                         .setPrettyPrint(true);

	}
}
