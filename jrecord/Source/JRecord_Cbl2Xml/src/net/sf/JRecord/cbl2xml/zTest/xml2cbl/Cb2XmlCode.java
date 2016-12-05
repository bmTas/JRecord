/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Cbl2Xml
 *    
 *    Sub-Project purpose: Convert Cobol Data files to / from Xml
 *
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */

package net.sf.JRecord.cbl2xml.zTest.xml2cbl;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Common.Conversion;
import net.sf.cb2xml.util.XmlUtils;

import org.junit.Assert;
import org.junit.internal.ArrayComparisonFailure;
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

	public static final boolean IS_MAC, IS_NIX, IS_WINDOWS;
	
	static {
		boolean isNix = false, isMac=false, isWin = true;
		try {
			String s = System.getProperty("os.name").toLowerCase();
			if (s != null) {
				isNix = (s.indexOf("nix") >= 0 || s.indexOf("nux") >= 0);
				isMac = s.indexOf("mac") >= 0;
				isWin = s.indexOf("win") >= 0;
			}
		} catch (Exception e) {
		}

		IS_MAC = isMac;
		IS_NIX = isNix;
		IS_WINDOWS = isWin;
	}

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
	
	public static void compare(String id, boolean isBinary, byte[] xml2data, byte[] expected) throws ArrayComparisonFailure {
		if (isBinary) {
			if (expected.length == xml2data.length - 2) {
				for (int k = 0; k < expected.length; k++) {
					Assert.assertEquals(id + ", " + k, expected[k], xml2data[k] );
				}
			} else {
				compare(id, new String(expected), new String(xml2data));
				//Assert.assertArrayEquals(id, expected, xml2data);
			} 
		} else {
			compare(id, new String(expected), new String(xml2data));
		}
	}

	public static void compare(String id, String e, String a) {
		if (e.length() != a.length()) {
			if (Cb2XmlCode.IS_NIX) {
				e = Conversion.replace(e, "\r\n", "\n").toString();
			}
		}
		a = fix(a, e);
		e = fix(e, a);
		if (a.length() == e.length() + 2 && (a.endsWith(" \n") || a.endsWith("\r\n"))){
			a = a.substring(0, a.length() - 2);
		}
		if (! e.equals(a)) {
//			System.out.println(e.length() + " " + a.length());
			Assert.assertEquals(id, e, a);
		}
	}
	
	private static String fix(String s1, String s2) {
		int s1Len = s1.length() - 1;
		if (s1.length() == s2.length() + 1 && s1.charAt(s1Len) == '\n') {
			s1 = s1.substring(0, s1Len);
		}
		return s1;
	}
	
	public static void compare(String id, String fileName, byte[] data) throws IOException, XMLStreamException, FactoryConfigurationError {
		String s1 = loadFile(fileName, "\r\n", false);
		compareXmlStr(id, s1, data);
	}
	
	public static String addPlusToNumeric(String s, String[] skip) {
		StringBuilder b = new StringBuilder((s.length() * 12) / 10);
		StringBuilder w = new StringBuilder();
		
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			b.append(c);
			if (c == '>' && i+1 < s.length() && s.charAt(i+1) != '<') {
				boolean num = true, match = false;
				char ch;
				for (int j = i + 1; num && ((ch = s.charAt(j)) != '<') && j < s.length(); j++) {
					num = (ch >= '0' && ch <= '9') || ch == '.';
				}
				
				if (num) {				
					String check = w.toString();			
					for (int j = 0; (! match) && j < skip.length; j++) {
						match = check.equalsIgnoreCase(skip[j]);
					}
					
					if ( ! match) {
						b.append('+');
					}
				}
			} else if (c == '<') {
				w.delete(0, w.length());
			} else {
				w.append(c);
			}
		}
		
		return b.toString();
	}

	/**
	 * @param id
	 * @param xmlStr
	 * @param data
	 * @throws FileNotFoundException
	 * @throws XMLStreamException
	 * @throws FactoryConfigurationError
	 */
	public static void compareXmlStr(String id, String xmlStr, byte[] data) throws FileNotFoundException, XMLStreamException,
			FactoryConfigurationError {
		String s2 = new String(data);
		System.out.println(id);
		//System.out.println(xmlStr);
		System.out.println(new String(data));
		if (xmlStr.equals(s2)) { // || isEquivalent(new StringReader(xmlStr), data)) {
			return;
		} else {
			System.out.println("Lengths= " + s2.length() + ", " + xmlStr.length());
			org.junit.Assert.assertEquals(id, xmlStr, s2);
		}
	}
		

//	/**
//	 * @param data
//	 * @param stream
//	 * @return
//	 * @throws XMLStreamException
//	 * @throws FactoryConfigurationError
//	 */
//	private static boolean isEquivalent(Reader stream, byte[] data)
//			throws XMLStreamException, FactoryConfigurationError {
//		
//		XMLStreamReader parser1 = XMLInputFactory.newInstance().createXMLStreamReader(stream);
//		XMLStreamReader parser2 = XMLInputFactory.newInstance().createXMLStreamReader(new ByteArrayInputStream(data));
//		String spaces = "                                                                                                        ";
//		int type1 = -1, type2;
//
//		StringBuilder b1 = new StringBuilder();
//		StringBuilder b2 = new StringBuilder();
//		
//		spaces = spaces + spaces + spaces;
//		type1 = parser1.next();
//		type2 = parser2.next();
//		while (parser1.hasNext()) {
//			if (type1 != type2) {
//				if (type1 == XMLStreamConstants.CHARACTERS && type2 == XMLStreamConstants.END_ELEMENT) {
//					b1.append(parser1.getText());
//					type1 = parser1.next();
//				} else if (type2 == XMLStreamConstants.CHARACTERS && type1 == XMLStreamConstants.END_ELEMENT) {
//					b2.append(parser2.getText());
//					type2 = parser2.next();
//				} else {
//					return false;
//				}
//			} else {
//				switch (type1) {
//	            case XMLStreamConstants.START_ELEMENT:             	
//	            	if (! parser1.getName().toString().equals(parser2.getName().toString())) {
//	            		return false;
//	            	}
//	            	
//	            	b1.setLength(0);
//	            	b2.setLength(0);
//	            	break;
//	            case XMLStreamConstants.END_ELEMENT:
//	            	String ss1 = b1.toString();
//	            	String ss2 = b2.toString();
//	            	
//	            	if (ss1.trim().equals(ss2.trim())) {
//	            	} else {
//	            		try {
//	            			BigDecimal bd1 = new BigDecimal(ss1.trim());
//	            			BigDecimal bd2 = new BigDecimal(ss2.trim());
//	            			if (! bd1.equals(bd2)) {
//	            				return false;
//	            			}
//	            		} catch (Exception e) {
//	            			return false;
//	            		}
//	            	}
//	            	break;
//	            case XMLStreamConstants.CHARACTERS:
//	
//	            	b1.append(parser1.getText());
//	               	b2.append(parser2.getText());
//	
//	            	break;
////            case (XMLStreamConstants.START_DOCUMENT) :
////            break;
////            case (XMLStreamConstants.COMMENT) :
////            break;
////            case (XMLStreamConstants.DTD) :
////            	break;
////            case (XMLStreamConstants.ENTITY_REFERENCE) :
////            	break;
////            case (XMLStreamConstants.CDATA) :
////              break;
////            case (XMLStreamConstants.END_DOCUMENT): 
////            	break;
//            	default:
//				}
//
//				type1 = parser1.next();
//				type2 = parser2.next();
//			}
//		}
//		
//		parser1.close();
//		parser2.close();
//		return true;
//	}

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
    
	public static String loadFile(String fileName, boolean addToEnd) throws IOException {
		String recordSep = "\r\n";
		if (IS_NIX) {
			recordSep = "\n";
		}
		
		return loadFile(fileName, recordSep, addToEnd);
		
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