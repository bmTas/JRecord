package net.sf.JRecord.cbl2xml.zTest.cbl2xml.modifiers;

import static org.junit.Assert.assertEquals;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;

import javax.xml.stream.XMLStreamException;

import org.junit.Test;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Option.JRecordConstantVars;
import net.sf.JRecord.cbl2xml.Cobol2Xml;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.cbl2xml.zTest.xml2cbl.Cb2XmlCode;
import net.sf.JRecord.schema.jaxb.impl.ZeroPad;
import net.sf.JRecord.schema.jaxb.interfaces.IFormatField;

public class TstFieldFormat {
	private static final String ZERO_PAD_XML = ""
			+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<CobolData>\n"
			+ "    <Line>\n"
			+ "        <DTAR020-KCODE-STORE-KEY>\n"
			+ "            <DTAR020-KEYCODE-NO>69694158</DTAR020-KEYCODE-NO>\n"
			+ "            <DTAR020-STORE-NO>020</DTAR020-STORE-NO>\n"
			+ "        </DTAR020-KCODE-STORE-KEY>\n"
			+ "        <DTAR020-DATE>40118</DTAR020-DATE>\n"
			+ "        <DTAR020-DEPT-NO>280</DTAR020-DEPT-NO>\n"
			+ "        <DTAR020-QTY-SOLD>000000001</DTAR020-QTY-SOLD>\n"
			+ "        <DTAR020-SALE-PRICE>5.01</DTAR020-SALE-PRICE>\n"
			+ "    </Line>\n"
			+ "    <Line>\n"
			+ "        <DTAR020-KCODE-STORE-KEY>\n"
			+ "            <DTAR020-KEYCODE-NO>63604808</DTAR020-KEYCODE-NO>\n"
			+ "            <DTAR020-STORE-NO>020</DTAR020-STORE-NO>\n"
			+ "        </DTAR020-KCODE-STORE-KEY>\n"
			+ "        <DTAR020-DATE>40118</DTAR020-DATE>\n"
			+ "        <DTAR020-DEPT-NO>170</DTAR020-DEPT-NO>\n"
			+ "        <DTAR020-QTY-SOLD>000000001</DTAR020-QTY-SOLD>\n"
			+ "        <DTAR020-SALE-PRICE>4.87</DTAR020-SALE-PRICE>\n"
			+ "    </Line>\n"
			+ "    <Line>\n"
			+ "        <DTAR020-KCODE-STORE-KEY>\n"
			+ "            <DTAR020-KEYCODE-NO>68634752</DTAR020-KEYCODE-NO>\n"
			+ "            <DTAR020-STORE-NO>059</DTAR020-STORE-NO>\n"
			+ "        </DTAR020-KCODE-STORE-KEY>\n"
			+ "        <DTAR020-DATE>40118</DTAR020-DATE>\n"
			+ "        <DTAR020-DEPT-NO>410</DTAR020-DEPT-NO>\n"
			+ "        <DTAR020-QTY-SOLD>000000001</DTAR020-QTY-SOLD>\n"
			+ "        <DTAR020-SALE-PRICE>8.99</DTAR020-SALE-PRICE>\n"
			+ "    </Line>\n"
			+ "    <Line>\n"
			+ "        <DTAR020-KCODE-STORE-KEY>\n"
			+ "            <DTAR020-KEYCODE-NO>64674965</DTAR020-KEYCODE-NO>\n"
			+ "            <DTAR020-STORE-NO>166</DTAR020-STORE-NO>\n"
			+ "        </DTAR020-KCODE-STORE-KEY>\n"
			+ "        <DTAR020-DATE>40118</DTAR020-DATE>\n"
			+ "        <DTAR020-DEPT-NO>235</DTAR020-DEPT-NO>\n"
			+ "        <DTAR020-QTY-SOLD>-00000001</DTAR020-QTY-SOLD>\n"
			+ "        <DTAR020-SALE-PRICE>-19.99</DTAR020-SALE-PRICE>\n"
			+ "    </Line>\n"
			+ "</CobolData>"
			;
	
	@Test
	public void test1() throws XMLStreamException, IOException {
		ICobol2Xml jsnBldr = getCobol2JsonNormalBuilder();
		
		IFormatField zeroPad = newZeroPad();
		
	    InputStream input = new FileInputStream(Cb2XmlCode.getFullName("DTAR020_4rows.bin"));
	    StringWriter w = new StringWriter();

		try {
			jsnBldr
				.setFormatField("DTAR020-STORE-NO", zeroPad)
				.setFormatField("DTAR020-DEPT-NO", zeroPad)
				.setFormatField("DTAR020-QTY-SOLD", zeroPad)
				
				.cobol2xml(input, w);
		} catch (Throwable e) {
			// TODO Auto-generated catch block
			//e.printStackTrace();
			System.out.print(w.toString());
			throw e;
		}
		
//	    System.out.print(w.toString());
	    
	    assertEquals(ZERO_PAD_XML, w.toString());
	}

	@Test
	public void test2() throws IOException, XMLStreamException {
		ICobol2Xml jsnBldr = getCobol2JsonDropNamesBuilder();
		
		IFormatField zeroPad = newZeroPad();
		
	    InputStream input = new FileInputStream(Cb2XmlCode.getFullName("DTAR020_4rows.bin"));
	    StringWriter w = new StringWriter();

		jsnBldr
			.setFormatField("STORE-NO", zeroPad)
			.setFormatField("DEPT-NO", zeroPad)
			.setFormatField("QTY-SOLD", zeroPad)
			
			.cobol2xml(input, w)
			;
		
//	    System.out.print(w.toString());
	    String expected = Conversion.replace(ZERO_PAD_XML, "DTAR020-", "").toString();

	    assertEquals(expected, w.toString());
	}


	private IFormatField newZeroPad() {
		return new ZeroPad();
	}

	
	private ICobol2Xml getCobol2JsonDropNamesBuilder() {
	       return getCobol2JsonNormalBuilder()
	                          .setDropCopybookNameFromFields(true);
	}

	private ICobol2Xml getCobol2JsonNormalBuilder() {
		JRecordConstantVars constants = Cobol2Xml.JR_CONSTANTS;
 	
       return Cobol2Xml.newCobol2Xml(Cb2XmlCode.getFullName("cobol/DTAR020.cbl"))

                                         // Cobol Options
                         .setFileOrganization(constants.IO_FIXED_LENGTH)
                         .setDialect(constants.FMT_MAINFRAME)               
                         .setSplitCopybook(constants.SPLIT_NONE)      
                         .setFont("cp037")
                         .setPrettyPrint(true);

	}

}
