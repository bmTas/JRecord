/*
 * @Author Bruce Martin
 * Created on 21/04/2007
 *
 * Purpose:
 */
/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
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

package net.sf.JRecord.IO;

import java.io.IOException;
import java.io.OutputStream;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.XmlConstants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;

/**
 * This class writes an AbstractLine (must be an XmlLine) to the File
 * as one XML element
 *
 * @author Bruce Martin
 *
 */
public class XmlLineWriter extends AbstractLineWriter {

    private static final int ATTR_PREFIX_LENGTH   = XmlConstants.ATTRIBUTE_PREFIX.length();
    private static final int FOLLOWING_TEXT_INDEX = XmlConstants.FOLLOWING_TEXT_INDEX;

    private XMLStreamWriter writer;
    private OutputStream os;


	/**
     * @see net.sf.JRecord.IO.AbstractLineWriter#open(java.io.OutputStream)
     */
    public void open(OutputStream outputStream) throws IOException {

    	XMLOutputFactory f ;

    	os = outputStream;
//    	try {
    		 f = XMLOutputFactory.newInstance();
//    	} catch (Exception e) {
//    		 Object o =  XMLOutputFactory.newInstance("javax.xml.stream.XMLOutputFactory",
//					  this.getClass().getClassLoader());
//    		 f = (XMLOutputFactory) o;
//		}

        try {
            writer = f.createXMLStreamWriter(outputStream);
        } catch (XMLStreamException e) {
            throw new IOException("Error allocating XML Writer: " + e.getMessage());
        }

    }

    /**
     * @see net.sf.JRecord.IO.AbstractLineWriter#write(net.sf.JRecord.Details.AbstractLine)
     */
    public void write(AbstractLine line) throws IOException {
        //String name = toString(line.getField(line.getPreferredLayoutIdx(), 0));
        String name = toString(line.getLayout().getRecord(line.getPreferredLayoutIdx()).getRecordName());

        try {
            if (XmlConstants.XML_START_DOCUMENT.equals(name)) {
                write_100_StartDocument(line);
            } else if (XmlConstants.XML_DTD.equals(name)) {
                writer.writeDTD(line.getFieldValue(XmlConstants.XML_TEXT).asString());
            } else if (XmlConstants.XML_COMMENT.equals(name)) {
                writer.writeComment(fixComment(line.getFieldValue(XmlConstants.XML_TEXT).asString()));
            } else if (XmlConstants.XML_CDATA.equals(name)) {
                writer.writeCData(line.getFieldValue(XmlConstants.XML_TEXT).asString());
            } else if (XmlConstants.XML_REFERENCE.equals(name)) {
                writer.writeEntityRef(line.getFieldValue(XmlConstants.XML_TEXT).asString());
            } else if (name.startsWith("/")) {
                writer.writeEndElement();
            } else {
                write_200_Element(line);
            }
        } catch (XMLStreamException e) {
        	e.printStackTrace();
            throw new IOException("Error allocating XML Writer: " + e.getMessage());
        }
    }

    /**
     * Write Start of Document
     * @param line line to be written
     * @throws XMLStreamException any error that occurs
     */
    private void write_100_StartDocument(AbstractLine line) throws XMLStreamException {
        String encoding = line.getFieldValue(XmlConstants.ENCODING).asString();
        String version  = line.getFieldValue(XmlConstants.VERSION).asString();

        if ("".equals(encoding) && (version == null || "".equals(version))) {
            writer.writeStartDocument();
        } else if ("".equals(encoding)) {
            writer.writeStartDocument(version);
        } else {
        	if (version == null || "".equals(version)) {
        		version = "1.0";
        	}
            writer.writeStartDocument(encoding, version);
        }

        writeFollowingText(line);
    }

    /**
     * Write DTD
     * @param line line to be written
     * @throws XMLStreamException any error that occurs
     */
    @SuppressWarnings("deprecation")
	private void write_200_Element(AbstractLine line) throws XMLStreamException {
        int idx = line.getPreferredLayoutIdx();
        LayoutDetail layout = line.getLayout();
        RecordDetail rec = layout.getRecord(idx);
        int fieldCount   = rec.getFieldCount();
        //String name = toString(line.getField(idx, 0));
		String name = toString(line.getLayout().getRecord(line.getPreferredLayoutIdx()).getRecordName());
        String prefix = line.getFieldValue(XmlConstants.PREFIX).asString();
        String namespace = line.getFieldValue(XmlConstants.NAMESPACE).asString();
        boolean end = "true".equals(line.getFieldValue(XmlConstants.END_ELEMENT).asString().toLowerCase());
        String attrName;
        Object value;

        if (end) {
            if ("".equals(prefix) && "".equals(namespace)) {
                writer.writeEmptyElement(name);
            } else if ("".equals(prefix)) {
                writer.writeEmptyElement(namespace, name);
            } else {
                writer.writeEmptyElement(prefix, name, namespace);
            }
        } else {
            if ("".equals(prefix) && "".equals(namespace)) {
                writer.writeStartElement(name);
            } else if ("".equals(prefix)) {
                writer.writeStartElement(namespace, name);
            } else {
                writer.writeStartElement(prefix, name, namespace);
            }
        }

        for (int i = FOLLOWING_TEXT_INDEX + 3; i < fieldCount; i++) {
            attrName = rec.getField(i).getName();
            value = line.getField(idx, i);

            if (value != null && ! "".equals(value) && attrName != null) {
                if (attrName.startsWith(XmlConstants.ATTRIBUTE_PREFIX)) {
                	attrName = attrName.substring(ATTR_PREFIX_LENGTH);
            	}
                writer.writeAttribute(attrName, value.toString());
            }
        }

        writeFollowingText(line);
    }


    /**
     * Write Following Text
     * @param line line to be written
     * @throws XMLStreamException any error
     */
    private void writeFollowingText(AbstractLine line) throws XMLStreamException {
        int idx = line.getPreferredLayoutIdx();
        @SuppressWarnings("deprecation")
		String followingText = toString(line.getField(idx, FOLLOWING_TEXT_INDEX));

        if (! "".equals(followingText)) {
            writer.writeCharacters(followingText);
        }
    }


    private String fixComment(Object o) {
    	StringBuilder comment = new StringBuilder(toString(o));
    	int l;

    	replace(comment, "--", "==");

    	l = comment.length() - 1;
    	while (" ".equals(comment.substring(l, l + 1))) {
    		l -= 1;
    	}
//    	System.out.print("fix -- " + comment + "< " + l + " >> >" + (comment.substring(l, l + 1))
//    			+ "< --> ");
    	if ("-".equals(comment.substring(l, l + 1))) {
    		comment.replace(l, l + 1, "=");
    	}
//    	System.out.println(comment + "<--");

    	return comment.toString();
    }

    /**
     * Replaces on string with another in a String bugffer
     *
     * @param in String buffer to be updated
     * @param from seqarch string
     * @param to replacement string
     */
    private static void replace(StringBuilder in, String from, String to) {
        Conversion.replace(in, from, to);
    }


    /**
     * Convert object to String
     * @param o object to be converted
     * @return String equivalent
     */
    private String toString(Object o) {
        String s = "";
        if (o != null) {
            s = o.toString();
        }
        return s;
    }


    /**
     * @see net.sf.JRecord.IO.AbstractLineWriter#close()
     */
    public void close() throws IOException {

        try {
            writer.writeEndDocument();
            writer.close();
            os.close();
        } catch (XMLStreamException e) {
        	e.printStackTrace();
            throw new IOException("Error closing XML Writer: " + e.getMessage());
        }
    }

}
