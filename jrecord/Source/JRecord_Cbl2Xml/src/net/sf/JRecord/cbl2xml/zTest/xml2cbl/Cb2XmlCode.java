package net.sf.JRecord.cbl2xml.zTest.xml2cbl;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.net.URL;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import net.sf.cb2xml.util.XmlUtils;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;


/**
 * Common Code to compare Xml Documents
 * @author Bruce Martin
 *
 */
public class Cb2XmlCode {

	public static String toString(String[] lines) {
		StringBuilder b = new StringBuilder();
		String sep = "";
		
		for (String s : lines) {
			b.append(sep).append(s);
			sep = "\n";
		}
		
		return b.toString();
	}
	
	public static InputStream toStream(String[] lines) {
		return toStream(toString(lines));
	}
	
	public static InputStream toStream(String s) {
		return new ByteArrayInputStream(s.getBytes());
	}
	
	public static boolean compare(byte[] b1, byte[] b2) {
		int num = Math.min(b1.length, b2.length);
		
		for (int i = 0;i < num; i++) {
			if (b1[i] != b2[i]) {
				return false;
			}
		}
		return true;
	}
	
	public static void compare(String id, String fileName, byte[] data) throws IOException, XMLStreamException, FactoryConfigurationError {
		String s1 = loadFile(fileName, "\r\n", false);
		String s2 = new String(data);
		if (s1.equals(s2) || isEqivalent(fileName, data)) {
			return;
		} else {
			System.out.println("Lengths= " + s2.length() + ", " + s1.length());
			org.junit.Assert.assertEquals(id, s1, s2);
		}
	}
		
	private static boolean isEqivalent( String fileName, byte[] data) throws FileNotFoundException, XMLStreamException, FactoryConfigurationError {
		
		String spaces = "                                                                                                        ";
		XMLStreamReader parser1 = XMLInputFactory.newInstance().createXMLStreamReader(new FileInputStream(fileName));
		XMLStreamReader parser2 = XMLInputFactory.newInstance().createXMLStreamReader(new ByteArrayInputStream(data));
		int type1 = -1, type2;

		StringBuilder b1 = new StringBuilder();
		StringBuilder b2 = new StringBuilder();
		
		spaces = spaces + spaces + spaces;
		type1 = parser1.next();
		type2 = parser2.next();
		while (parser1.hasNext()) {
			if (type1 != type2) {
				if (type1 == XMLStreamConstants.CHARACTERS && type2 == XMLStreamConstants.END_ELEMENT) {
					b1.append(parser1.getText());
					type1 = parser1.next();
				} else if (type2 == XMLStreamConstants.CHARACTERS && type1 == XMLStreamConstants.END_ELEMENT) {
					b2.append(parser2.getText());
					type2 = parser2.next();
				} else {
					return false;
				}
			} else {
				switch (type1) {
	            case XMLStreamConstants.START_ELEMENT:             	
	            	if (! parser1.getName().toString().equals(parser2.getName().toString())) {
	            		return false;
	            	}
	            	
	            	b1.setLength(0);
	            	b2.setLength(0);
	            	break;
	            case XMLStreamConstants.END_ELEMENT:
	            	String ss1 = b1.toString();
	            	String ss2 = b2.toString();
	            	
	            	if (ss1.trim().equals(ss2.trim())) {
	            	} else {
	            		try {
	            			BigDecimal bd1 = new BigDecimal(ss1.trim());
	            			BigDecimal bd2 = new BigDecimal(ss2.trim());
	            			if (! bd1.equals(bd2)) {
	            				return false;
	            			}
	            		} catch (Exception e) {
	            			return false;
	            		}
	            	}
	            	break;
	            case XMLStreamConstants.CHARACTERS:
	
	            	b1.append(parser1.getText());
	               	b2.append(parser2.getText());
	
	            	break;
//            case (XMLStreamConstants.START_DOCUMENT) :
//            break;
//            case (XMLStreamConstants.COMMENT) :
//            break;
//            case (XMLStreamConstants.DTD) :
//            	break;
//            case (XMLStreamConstants.ENTITY_REFERENCE) :
//            	break;
//            case (XMLStreamConstants.CDATA) :
//              break;
//            case (XMLStreamConstants.END_DOCUMENT): 
//            	break;
            	default:
				}

				type1 = parser1.next();
				type2 = parser2.next();
			}
		}
		
		parser1.close();
		parser2.close();
		return true;
	}

	public static void compare(String id, Document doc, String fileName) throws IOException, SAXException, ParserConfigurationException {
		compare(id, doc, fileToDom(fileName));
	}
	
	public static void compare(String id, Document doc,Document doc2) {
		String s1 = XmlUtils.domToString(doc).toString();
		String s2 = XmlUtils.domToString(doc2).toString();
		if (s1.equals(s2) || compareDocs(id, doc, doc2)) {
		} else {
			System.out.println("Lengths= " + s2.length() + ", " + s1.length());
			org.junit.Assert.assertEquals(id, s1, s2);
		}
	}
	
	
	public static void compare(String id, String fileName, Document doc) throws IOException, SAXException, ParserConfigurationException {
		compare(id, fileToDom(fileName), doc);
	}
	
	public static boolean compareDocs(String id, Document doc1, Document doc2) {	
		System.out.println("---------------- " + id);
		System.out.println(XmlUtils.domToString(doc2).toString());
		System.out.println();
		return compare(doc1.getDocumentElement(), doc2.getDocumentElement());
	}
	
	
	private static boolean compare(Element element1, Element element2) {
        NodeList nodeList1 = element1.getChildNodes();
        NodeList nodeList2 = element2.getChildNodes();
        boolean ret = true;
       //level += 1;

        if (nodeList1.getLength() == nodeList2.getLength()) {
	        for (int i = 0; ret && i < nodeList1.getLength(); i++) {
	        	ret = checkNode(nodeList1.item(i), nodeList2.item(i));
	        }
  //      } else if (nodeList1.getLength() < nodeList2.getLength()) {
        } else {
        	System.out.println(":: Node Length >>" + element1.getNodeName() + " " + nodeList1.getLength()  + " " + nodeList2.getLength());
        	return false;
        }
        

        return ret;
	}
	
	private static boolean checkNode(org.w3c.dom.Node node1, org.w3c.dom.Node node2) {
        if (node1.getNodeType() == org.w3c.dom.Node.ELEMENT_NODE 
        &&  node2.getNodeType() == org.w3c.dom.Node.ELEMENT_NODE) {
            Element childElement1 = (Element) node1;
            Element childElement2 = (Element) node2;
            NamedNodeMap attributes = childElement1.getAttributes();
            //System.out.println();
            
            for (int i = 0; i < attributes.getLength(); i++) {
            	Node item = attributes.item(i);
            	
            	if (item instanceof Attr) {
            		Attr attr = (Attr) item;
					String value1 = attr.getValue();
					if (childElement2.hasAttribute(attr.getName())){
	            		String value2 = childElement2.getAttribute(attr.getName());
	            		System.out.print("\t" +attr.getName() + " ~ " + value1 + "==" + value2);
						if (value1 == null) {
							if (value2 != null) {
								System.out.println();
					        	System.out.println(":: Attribute >>" + attr.getName() + " ~ " + value1 + "==" + value2);
								return false;
							}
	            		} else if (! value1.equals(value2)) {
							System.out.println();
				        	System.out.println(":: Attribute >>" + attr.getName() + " ~ " + value1 + "==" + value2);
				        	return false;
	            		}
					}
            	}
            }
            
            return compare(childElement1, childElement2);
        } else if (node1.getNodeType() == org.w3c.dom.Node.TEXT_NODE 
        	   &&  node2.getNodeType()  == org.w3c.dom.Node.TEXT_NODE) {
        	String s1 = node1.getNodeValue();
        	String s2 = node2.getNodeValue();
        	if ((s1 == null && s2 != null)
        	|| ! s1.trim().equals(s2.trim())) {
        		return false;
        	}
        } else if (node1.getNodeType() != node2.getNodeType()) {
        	return false;
        }
        return true;
	}
	
    public static Document fileToDom(String fileName)
	throws IOException, SAXException, ParserConfigurationException {

        DocumentBuilderFactory factory
           		= DocumentBuilderFactory.newInstance();
        return factory.newDocumentBuilder().parse(new File(fileName));
    }
    
	public static String loadFile(String fileName, String recordSep, boolean addToEnd) throws IOException {
		StringBuilder b = new StringBuilder();
		String sep = "";
		BufferedReader r = new BufferedReader(new FileReader(fileName));
		String s;
		while ((s = r.readLine()) != null) {
			b.append(sep).append(s);
			sep = recordSep;
		}
		r.close();
		
//		if (b.charAt(b.length() - 1) == '\n') {
//			b.setLength(b.length() - 1);
//		}
		if (addToEnd) {
			b.append(sep);
		}
		return b.toString();
	}


    public static String getFullName(String filename) {
    	URL resource = Cb2XmlCode.class.getResource(filename);
    	if (resource == null) {
    		System.out.println(" --> Can not find: " + filename);
    	}
		return resource.getFile();
    }
}
