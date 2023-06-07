package net.sf.JRecord.cbl2xml.zTest.cbl2xml.modifiers;

import static org.junit.Assert.assertEquals;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.math.BigDecimal;

import javax.xml.stream.XMLStreamException;

import org.junit.Test;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Option.JRecordConstantVars;
import net.sf.JRecord.cbl2xml.Cobol2Xml;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.cbl2xml.zTest.xml2cbl.Cb2XmlCode;
import net.sf.JRecord.schema.jaxb.IItem;
import net.sf.JRecord.schema.jaxb.interfaces.IWriteCheck;

public class TstIWriteCheck {

	private static final String STANDARD_XML = ""
			+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<CobolData>\n"
			+ "    <Line>\n"
			+ "        <DTAR020-KCODE-STORE-KEY>\n"
			+ "            <DTAR020-KEYCODE-NO>69694158</DTAR020-KEYCODE-NO>\n"
			+ "            <DTAR020-STORE-NO>20</DTAR020-STORE-NO>\n"
			+ "        </DTAR020-KCODE-STORE-KEY>\n"
			+ "        <DTAR020-DATE>40118</DTAR020-DATE>\n"
			+ "        <DTAR020-DEPT-NO>280</DTAR020-DEPT-NO>\n"
			+ "        <DTAR020-QTY-SOLD>1</DTAR020-QTY-SOLD>\n"
			+ "        <DTAR020-SALE-PRICE>5.01</DTAR020-SALE-PRICE>\n"
			+ "    </Line>\n"
			+ "    <Line>\n"
			+ "        <DTAR020-KCODE-STORE-KEY>\n"
			+ "            <DTAR020-KEYCODE-NO>63604808</DTAR020-KEYCODE-NO>\n"
			+ "            <DTAR020-STORE-NO>20</DTAR020-STORE-NO>\n"
			+ "        </DTAR020-KCODE-STORE-KEY>\n"
			+ "        <DTAR020-DATE>40118</DTAR020-DATE>\n"
			+ "        <DTAR020-DEPT-NO>170</DTAR020-DEPT-NO>\n"
			+ "        <DTAR020-QTY-SOLD>1</DTAR020-QTY-SOLD>\n"
			+ "        <DTAR020-SALE-PRICE>4.87</DTAR020-SALE-PRICE>\n"
			+ "    </Line>\n"
			+ "    <Line>\n"
			+ "        <DTAR020-KCODE-STORE-KEY>\n"
			+ "            <DTAR020-KEYCODE-NO>68634752</DTAR020-KEYCODE-NO>\n"
			+ "            <DTAR020-STORE-NO>59</DTAR020-STORE-NO>\n"
			+ "        </DTAR020-KCODE-STORE-KEY>\n"
			+ "        <DTAR020-DATE>40118</DTAR020-DATE>\n"
			+ "        <DTAR020-DEPT-NO>410</DTAR020-DEPT-NO>\n"
			+ "        <DTAR020-QTY-SOLD>1</DTAR020-QTY-SOLD>\n"
			+ "        <DTAR020-SALE-PRICE>8.99</DTAR020-SALE-PRICE>\n"
			+ "    </Line>\n"
			+ "    <Line>\n"
			+ "        <DTAR020-KCODE-STORE-KEY>\n"
			+ "            <DTAR020-KEYCODE-NO>64674965</DTAR020-KEYCODE-NO>\n"
			+ "            <DTAR020-STORE-NO>166</DTAR020-STORE-NO>\n"
			+ "        </DTAR020-KCODE-STORE-KEY>\n"
			+ "        <DTAR020-DATE>40118</DTAR020-DATE>\n"
			+ "        <DTAR020-DEPT-NO>235</DTAR020-DEPT-NO>\n"
			+ "        <DTAR020-QTY-SOLD>-1</DTAR020-QTY-SOLD>\n"
			+ "        <DTAR020-SALE-PRICE>-19.99</DTAR020-SALE-PRICE>\n"
			+ "    </Line>\n"
			+ "</CobolData>";
	
	private static final String DROP_KEYCODE_STORE_XML = ""
			+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<CobolData>\n"
			+ "    <Line>\n"
			+ "        <DTAR020-DATE>40118</DTAR020-DATE>\n"
			+ "        <DTAR020-DEPT-NO>280</DTAR020-DEPT-NO>\n"
			+ "        <DTAR020-QTY-SOLD>1</DTAR020-QTY-SOLD>\n"
			+ "        <DTAR020-SALE-PRICE>5.01</DTAR020-SALE-PRICE>\n"
			+ "    </Line>\n"
			+ "    <Line>\n"
			+ "        <DTAR020-DATE>40118</DTAR020-DATE>\n"
			+ "        <DTAR020-DEPT-NO>170</DTAR020-DEPT-NO>\n"
			+ "        <DTAR020-QTY-SOLD>1</DTAR020-QTY-SOLD>\n"
			+ "        <DTAR020-SALE-PRICE>4.87</DTAR020-SALE-PRICE>\n"
			+ "    </Line>\n"
			+ "    <Line>\n"
			+ "        <DTAR020-KCODE-STORE-KEY>\n"
			+ "            <DTAR020-KEYCODE-NO>68634752</DTAR020-KEYCODE-NO>\n"
			+ "            <DTAR020-STORE-NO>59</DTAR020-STORE-NO>\n"
			+ "        </DTAR020-KCODE-STORE-KEY>\n"
			+ "        <DTAR020-DATE>40118</DTAR020-DATE>\n"
			+ "        <DTAR020-DEPT-NO>410</DTAR020-DEPT-NO>\n"
			+ "        <DTAR020-QTY-SOLD>1</DTAR020-QTY-SOLD>\n"
			+ "        <DTAR020-SALE-PRICE>8.99</DTAR020-SALE-PRICE>\n"
			+ "    </Line>\n"
			+ "    <Line>\n"
			+ "        <DTAR020-KCODE-STORE-KEY>\n"
			+ "            <DTAR020-KEYCODE-NO>64674965</DTAR020-KEYCODE-NO>\n"
			+ "            <DTAR020-STORE-NO>166</DTAR020-STORE-NO>\n"
			+ "        </DTAR020-KCODE-STORE-KEY>\n"
			+ "        <DTAR020-DATE>40118</DTAR020-DATE>\n"
			+ "        <DTAR020-DEPT-NO>235</DTAR020-DEPT-NO>\n"
			+ "        <DTAR020-QTY-SOLD>-1</DTAR020-QTY-SOLD>\n"
			+ "        <DTAR020-SALE-PRICE>-19.99</DTAR020-SALE-PRICE>\n"
			+ "    </Line>\n"
			+ "</CobolData>";
	
	private static final String DROP_KEYCODE_STORE_PRICE_XML = ""
			+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			+ "<CobolData>\n"
			+ "    <Line>\n"
			+ "        <DTAR020-DATE>40118</DTAR020-DATE>\n"
			+ "        <DTAR020-DEPT-NO>280</DTAR020-DEPT-NO>\n"
			+ "        <DTAR020-QTY-SOLD>1</DTAR020-QTY-SOLD>\n"
			+ "        <DTAR020-SALE-PRICE>5.01</DTAR020-SALE-PRICE>\n"
			+ "    </Line>\n"
			+ "    <Line>\n"
			+ "        <DTAR020-DATE>40118</DTAR020-DATE>\n"
			+ "        <DTAR020-DEPT-NO>170</DTAR020-DEPT-NO>\n"
			+ "        <DTAR020-QTY-SOLD>1</DTAR020-QTY-SOLD>\n"
			+ "        <DTAR020-SALE-PRICE>4.87</DTAR020-SALE-PRICE>\n"
			+ "    </Line>\n"
			+ "    <Line>\n"
			+ "        <DTAR020-KCODE-STORE-KEY>\n"
			+ "            <DTAR020-KEYCODE-NO>68634752</DTAR020-KEYCODE-NO>\n"
			+ "            <DTAR020-STORE-NO>59</DTAR020-STORE-NO>\n"
			+ "        </DTAR020-KCODE-STORE-KEY>\n"
			+ "        <DTAR020-DATE>40118</DTAR020-DATE>\n"
			+ "        <DTAR020-DEPT-NO>410</DTAR020-DEPT-NO>\n"
			+ "    </Line>\n"
			+ "    <Line>\n"
			+ "        <DTAR020-KCODE-STORE-KEY>\n"
			+ "            <DTAR020-KEYCODE-NO>64674965</DTAR020-KEYCODE-NO>\n"
			+ "            <DTAR020-STORE-NO>166</DTAR020-STORE-NO>\n"
			+ "        </DTAR020-KCODE-STORE-KEY>\n"
			+ "        <DTAR020-DATE>40118</DTAR020-DATE>\n"
			+ "        <DTAR020-DEPT-NO>235</DTAR020-DEPT-NO>\n"
			+ "        <DTAR020-QTY-SOLD>-1</DTAR020-QTY-SOLD>\n"
			+ "        <DTAR020-SALE-PRICE>-19.99</DTAR020-SALE-PRICE>\n"
			+ "    </Line>\n"
			+ "</CobolData>";
	
	@Test
	public void testNormal() throws IOException, XMLStreamException {
		ICobol2Xml cbl2Xml = getCobol2XmlNormalBuilder();
	    InputStream input = new FileInputStream(Cb2XmlCode.getFullName("DTAR020_4rows.bin"));
       // ByteArrayOutputStream output = new ByteArrayOutputStream();
	    StringWriter w = new StringWriter();
	    
	    cbl2Xml.cobol2xml(input, w);
	    
//	    System.out.print(w.toString());
	    
	    assertEquals(STANDARD_XML, w.toString());
	}
	
	
	@Test
	public void testNormalDropStore() throws IOException, XMLStreamException {
		ICobol2Xml cbl2Xml = getCobol2XmlNormalBuilder();
	    InputStream input = new FileInputStream(Cb2XmlCode.getFullName("DTAR020_4rows.bin"));
       // ByteArrayOutputStream output = new ByteArrayOutputStream();
	    StringWriter w = new StringWriter();
	    
	    cbl2Xml
	      .setWriteCheck("DTAR020-KCODE-STORE-KEY", newDTARKeycodeStore())
	    	.cobol2xml(input, w);
	    
//	    System.out.print(w.toString());
	    
	    assertEquals(DROP_KEYCODE_STORE_XML, w.toString());
	}

	
	@Test
	public void testNormalDropStorePrice() throws IOException, XMLStreamException {
		ICobol2Xml cbl2Xml = getCobol2XmlNormalBuilder();
	    InputStream input = new FileInputStream(Cb2XmlCode.getFullName("DTAR020_4rows.bin"));
       // ByteArrayOutputStream output = new ByteArrayOutputStream();
	    StringWriter w = new StringWriter();
	    
	    IWriteCheck priceCheck = new IWriteCheck() {
				@Override public boolean isOkToWrite(IItem item, AbstractLine line) {
					BigDecimal price = line.getFieldValue("DTAR020-SALE-PRICE").asBigDecimal();
					return ! (new BigDecimal("8.99")) .equals(price);
				}
		    };
	    
	    cbl2Xml
	        .setWriteCheck("DTAR020-KCODE-STORE-KEY", newDTARKeycodeStore())
	    	.setWriteCheck("DTAR020-QTY-SOLD", priceCheck)
	    	.setWriteCheck("DTAR020-SALE-PRICE", priceCheck)

	    	.cobol2xml(input, w);
	    
	   // System.out.print(w.toString());
	    
	    assertEquals(DROP_KEYCODE_STORE_PRICE_XML, w.toString());
	}

	private IWriteCheck newDTARKeycodeStore() {
		return new IWriteCheck() {
				@Override public boolean isOkToWrite(IItem item, AbstractLine line) {
					int store = line.getFieldValue("DTAR020-STORE-NO").asInt();
					return store != 20;
				}
	       };
	}

	
	@Test
	public void testDropCopybook() throws IOException, XMLStreamException {
		ICobol2Xml cbl2Xml = getCobol2XmlDropNamesBuilder();
	    InputStream input = new FileInputStream(Cb2XmlCode.getFullName("DTAR020_4rows.bin"));
       // ByteArrayOutputStream output = new ByteArrayOutputStream();
	    StringWriter w = new StringWriter();
	    
	    cbl2Xml.cobol2xml(input, w);
	    
//	    System.out.print(w.toString());
	    
	    String expected = Conversion.replace(STANDARD_XML, "DTAR020-", "").toString();
	    
	    assertEquals(expected, w.toString());
	}

	
	@Test
	public void testDropCopybookAndStorePrice() throws IOException, XMLStreamException {
		ICobol2Xml cbl2Xml = getCobol2XmlDropNamesBuilder();
	    InputStream input = new FileInputStream(Cb2XmlCode.getFullName("DTAR020_4rows.bin"));
       // ByteArrayOutputStream output = new ByteArrayOutputStream();
	    StringWriter w = new StringWriter();
	    
	    IWriteCheck priceCheck =  createPriceCheck();
	    
	    cbl2Xml
	    	.setWriteCheck("KCODE-STORE-KEY", CreateStoreTest())
	    	.setWriteCheck("QTY-SOLD", priceCheck)
	    	.setWriteCheck("SALE-PRICE", priceCheck)
	    	.cobol2xml(input, w);
	    
//	    System.out.print(w.toString());
	    
	    String expected = Conversion.replace(DROP_KEYCODE_STORE_PRICE_XML, "DTAR020-", "").toString();
	    
	    assertEquals(expected, w.toString());
	}

	
	@Test
	public void testDropCopybookAndStore() throws IOException, XMLStreamException {
		ICobol2Xml cbl2Xml = getCobol2XmlDropNamesBuilder();
	    InputStream input = new FileInputStream(Cb2XmlCode.getFullName("DTAR020_4rows.bin"));
       // ByteArrayOutputStream output = new ByteArrayOutputStream();
	    StringWriter w = new StringWriter();
	    
	    cbl2Xml
	    	.setWriteCheck("KCODE-STORE-KEY", CreateStoreTest())
	    	.cobol2xml(input, w);
	    
//	    System.out.print(w.toString());
	    
	    String expected = Conversion.replace(DROP_KEYCODE_STORE_XML, "DTAR020-", "").toString();
	    
	    assertEquals(expected, w.toString());
	}

	@Test
	public void testNoMainArray() throws IOException, XMLStreamException {
		ICobol2Xml cbl2Xml = getCobol2XmlNoMainArrayBuilder();
	    InputStream input = new FileInputStream(Cb2XmlCode.getFullName("DTAR020_4rows.bin"));
       // ByteArrayOutputStream output = new ByteArrayOutputStream();
	    StringWriter w = new StringWriter();
	    
	    cbl2Xml.cobol2xml(input, w);
	    
	   // System.out.print(w.toString());
	    
	    String expected = ""
	    		+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
	    		+ "<CobolData>\n"
	    		+ "    <Line>\n"
	    		+ "        <KCODE-STORE-KEY>\n"
	    		+ "            <KEYCODE-NO>69694158</KEYCODE-NO>\n"
	    		+ "            <STORE-NO>20</STORE-NO>\n"
	    		+ "        </KCODE-STORE-KEY>\n"
	    		+ "        <DATE>40118</DATE>\n"
	    		+ "        <DEPT-NO>280</DEPT-NO>\n"
	    		+ "        <QTY-SOLD>1</QTY-SOLD>\n"
	    		+ "        <SALE-PRICE>5.01</SALE-PRICE>\n"
	    		+ "    </Line>\n"
	    		+ "    <Line>\n"
	    		+ "        <KCODE-STORE-KEY>\n"
	    		+ "            <KEYCODE-NO>63604808</KEYCODE-NO>\n"
	    		+ "            <STORE-NO>20</STORE-NO>\n"
	    		+ "        </KCODE-STORE-KEY>\n"
	    		+ "        <DATE>40118</DATE>\n"
	    		+ "        <DEPT-NO>170</DEPT-NO>\n"
	    		+ "        <QTY-SOLD>1</QTY-SOLD>\n"
	    		+ "        <SALE-PRICE>4.87</SALE-PRICE>\n"
	    		+ "    </Line>\n"
	    		+ "    <Line>\n"
	    		+ "        <KCODE-STORE-KEY>\n"
	    		+ "            <KEYCODE-NO>68634752</KEYCODE-NO>\n"
	    		+ "            <STORE-NO>59</STORE-NO>\n"
	    		+ "        </KCODE-STORE-KEY>\n"
	    		+ "        <DATE>40118</DATE>\n"
	    		+ "        <DEPT-NO>410</DEPT-NO>\n"
	    		+ "        <QTY-SOLD>1</QTY-SOLD>\n"
	    		+ "        <SALE-PRICE>8.99</SALE-PRICE>\n"
	    		+ "    </Line>\n"
	    		+ "    <Line>\n"
	    		+ "        <KCODE-STORE-KEY>\n"
	    		+ "            <KEYCODE-NO>64674965</KEYCODE-NO>\n"
	    		+ "            <STORE-NO>166</STORE-NO>\n"
	    		+ "        </KCODE-STORE-KEY>\n"
	    		+ "        <DATE>40118</DATE>\n"
	    		+ "        <DEPT-NO>235</DEPT-NO>\n"
	    		+ "        <QTY-SOLD>-1</QTY-SOLD>\n"
	    		+ "        <SALE-PRICE>-19.99</SALE-PRICE>\n"
	    		+ "    </Line>\n"
	    		+ "</CobolData>";
	    
	    assertEquals(expected, w.toString());
	}
	
	@Test
	public void testDropOnStore() throws IOException, XMLStreamException {
		ICobol2Xml cbl2Xml = getCobol2XmlNoMainArrayBuilder();
	    InputStream input = new FileInputStream(Cb2XmlCode.getFullName("DTAR020_4rows.bin"));
       // ByteArrayOutputStream output = new ByteArrayOutputStream();
	    StringWriter w = new StringWriter();
	    
	    cbl2Xml
	      .setWriteCheck("KCODE-STORE-KEY", CreateStoreTest())

	    	.cobol2xml(input, w);
	    
	   // System.out.print(w.toString());
	    
	    String expected = ""
	    		+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
	    		+ "<CobolData>\n"
	    		+ "    <Line>\n"
	    		+ "        <DATE>40118</DATE>\n"
	    		+ "        <DEPT-NO>280</DEPT-NO>\n"
	    		+ "        <QTY-SOLD>1</QTY-SOLD>\n"
	    		+ "        <SALE-PRICE>5.01</SALE-PRICE>\n"
	    		+ "    </Line>\n"
	    		+ "    <Line>\n"
	    		+ "        <DATE>40118</DATE>\n"
	    		+ "        <DEPT-NO>170</DEPT-NO>\n"
	    		+ "        <QTY-SOLD>1</QTY-SOLD>\n"
	    		+ "        <SALE-PRICE>4.87</SALE-PRICE>\n"
	    		+ "    </Line>\n"
	    		+ "    <Line>\n"
	    		+ "        <KCODE-STORE-KEY>\n"
	    		+ "            <KEYCODE-NO>68634752</KEYCODE-NO>\n"
	    		+ "            <STORE-NO>59</STORE-NO>\n"
	    		+ "        </KCODE-STORE-KEY>\n"
	    		+ "        <DATE>40118</DATE>\n"
	    		+ "        <DEPT-NO>410</DEPT-NO>\n"
	    		+ "        <QTY-SOLD>1</QTY-SOLD>\n"
	    		+ "        <SALE-PRICE>8.99</SALE-PRICE>\n"
	    		+ "    </Line>\n"
	    		+ "    <Line>\n"
	    		+ "        <KCODE-STORE-KEY>\n"
	    		+ "            <KEYCODE-NO>64674965</KEYCODE-NO>\n"
	    		+ "            <STORE-NO>166</STORE-NO>\n"
	    		+ "        </KCODE-STORE-KEY>\n"
	    		+ "        <DATE>40118</DATE>\n"
	    		+ "        <DEPT-NO>235</DEPT-NO>\n"
	    		+ "        <QTY-SOLD>-1</QTY-SOLD>\n"
	    		+ "        <SALE-PRICE>-19.99</SALE-PRICE>\n"
	    		+ "    </Line>\n"
	    		+ "</CobolData>";
	    
	    assertEquals(expected, w.toString());
	}


	private IWriteCheck CreateStoreTest() {
		IWriteCheck storeCheck = new IWriteCheck() {
				@Override public boolean isOkToWrite(IItem item, AbstractLine line) {
					int store = line.getFieldValue("STORE-NO").asInt();
					return store != 20;
				}
	       };
		return storeCheck;
	}

	@Test
	public void testDropOnStoreAndPrice() throws IOException, XMLStreamException {
		ICobol2Xml cbl2Xml = getCobol2XmlNoMainArrayBuilder();
	    InputStream input = new FileInputStream(Cb2XmlCode.getFullName("DTAR020_4rows.bin"));
       // ByteArrayOutputStream output = new ByteArrayOutputStream();
	    StringWriter w = new StringWriter();
	    
	    IWriteCheck priceCheck =  createPriceCheck();
	    
	    cbl2Xml
	    	.setWriteCheck("KCODE-STORE-KEY", CreateStoreTest())
	    	.setWriteCheck("QTY-SOLD", priceCheck)
	    	.setWriteCheck("SALE-PRICE", priceCheck)

	    	.cobol2xml(input, w);
	    
	   // System.out.print(w.toString());
	    
	    String expected = ""
	    		+ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
	    		+ "<CobolData>\n"
	    		+ "    <Line>\n"
	    		+ "        <DATE>40118</DATE>\n"
	    		+ "        <DEPT-NO>280</DEPT-NO>\n"
	    		+ "        <QTY-SOLD>1</QTY-SOLD>\n"
	    		+ "        <SALE-PRICE>5.01</SALE-PRICE>\n"
	    		+ "    </Line>\n"
	    		+ "    <Line>\n"
	    		+ "        <DATE>40118</DATE>\n"
	    		+ "        <DEPT-NO>170</DEPT-NO>\n"
	    		+ "        <QTY-SOLD>1</QTY-SOLD>\n"
	    		+ "        <SALE-PRICE>4.87</SALE-PRICE>\n"
	    		+ "    </Line>\n"
	    		+ "    <Line>\n"
	    		+ "        <KCODE-STORE-KEY>\n"
	    		+ "            <KEYCODE-NO>68634752</KEYCODE-NO>\n"
	    		+ "            <STORE-NO>59</STORE-NO>\n"
	    		+ "        </KCODE-STORE-KEY>\n"
	    		+ "        <DATE>40118</DATE>\n"
	    		+ "        <DEPT-NO>410</DEPT-NO>\n"
	    		+ "    </Line>\n"
	    		+ "    <Line>\n"
	    		+ "        <KCODE-STORE-KEY>\n"
	    		+ "            <KEYCODE-NO>64674965</KEYCODE-NO>\n"
	    		+ "            <STORE-NO>166</STORE-NO>\n"
	    		+ "        </KCODE-STORE-KEY>\n"
	    		+ "        <DATE>40118</DATE>\n"
	    		+ "        <DEPT-NO>235</DEPT-NO>\n"
	    		+ "        <QTY-SOLD>-1</QTY-SOLD>\n"
	    		+ "        <SALE-PRICE>-19.99</SALE-PRICE>\n"
	    		+ "    </Line>\n"
	    		+ "</CobolData>";
	    
	    assertEquals(expected, w.toString());
	}


	private IWriteCheck createPriceCheck() {
		return new IWriteCheck() {
			@Override public boolean isOkToWrite(IItem item, AbstractLine line) {
				BigDecimal price = line.getFieldValue("SALE-PRICE").asBigDecimal();
				return ! (new BigDecimal("8.99")) .equals(price);
			}
	    };
	}


	private ICobol2Xml getCobol2XmlNoMainArrayBuilder() {
       return getCobol2XmlNormalBuilder()
                          .setDropCopybookNameFromFields(true);
	}
	
	private ICobol2Xml getCobol2XmlDropNamesBuilder() {
       return getCobol2XmlNormalBuilder()
                          .setDropCopybookNameFromFields(true);
	}
	
	private ICobol2Xml getCobol2XmlNormalBuilder() {
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

