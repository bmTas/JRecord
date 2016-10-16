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

package net.sf.JRecord.External.base;

import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalGroupSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.ExternalRecordSelection.StreamLine;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.detailsSelection.RecordSel;

/**
 * Write a RecordLayout (Record or Line Description) to a RecordEditor-XML file.
 *
 * @author Bruce Martin
 *
 */
public class RecordEditorXmlWriter implements CopybookWriter {

	private static final String STANDARD_FONT = "UTF-8";


	@Override
	public String writeCopyBook(String directory, BaseExternalRecord<?> copybook,
			AbsSSLogger log) throws Exception {
		String fileName;
		directory = ExternalConversion.fixDirectory(directory);

		fileName = directory + ExternalConversion.fixFileName(copybook.getRecordName()) + Constants.XML_EXTENSION;
		writeCopyBook(new FileOutputStream(fileName),
				copybook, log);
		return fileName;
	}

	@Override
	public void writeCopyBook(OutputStream outStream, BaseExternalRecord<?> copybook,
			AbsSSLogger log) throws Exception {
   	XMLOutputFactory f ;
   	XMLStreamWriter writer;

//    	try {
    		 f = XMLOutputFactory.newInstance();
//    	} catch (Exception e) {
//    		// For Java 5 (before the addition of XMLOutputFactory to standard java
//    		 Object o =  XMLOutputFactory.newInstance("javax.xml.stream.XMLOutputFactory",
//					  this.getClass().getClassLoader());
//    		 f = (XMLOutputFactory) o;
//		}

       writer = f.createXMLStreamWriter(new OutputStreamWriter(outStream, STANDARD_FONT));

       writer.writeStartDocument(STANDARD_FONT, "1.0");

       writeRecord(writer, null, copybook, hasGroupDetails(copybook));

       writer.writeEndDocument();
       writer.close();
       outStream.close();
	}

	private boolean hasGroupDetails(BaseExternalRecord<?> copybook) {
		for (int i = copybook.getNumberOfRecords() - 1; i >= 0; i--) {
			if (hasGroupDetails(copybook.getRecord(i))) {
				return true;
			}
		}
		
		for (int i = copybook.getNumberOfRecordFields() - 1; i >= 0; i--) {
			String group = copybook.getRecordField(i).getGroup();
			if (group != null && group.length() > 0) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Write a Record to the XML file
	 * @param writer where to write the XML
	 * @param copybook copybook to be written
	 * @throws XMLStreamException any error that occurs
	 */
	@SuppressWarnings("deprecation")
	private void writeRecord(XMLStreamWriter writer, BaseExternalRecord<?> master, BaseExternalRecord<?> copybook, boolean hasGroup)
			throws XMLStreamException {
	   int i;
	   boolean toPrint = true;
	   String s;
	   String name = copybook.getCopyBook();

	   writer.writeStartElement(Constants.RE_XML_RECORD);

       writer.writeAttribute(Constants.RE_XML_RECORDNAME,  copybook.getRecordName());
	   writer.writeAttribute(Constants.RE_XML_COPYBOOK, name);
	   writeAttr(writer, Constants.RE_XML_DELIMITER, encodeDelim(copybook.getDelimiter()));
	   writeAttr(writer, Constants.RE_XML_DESCRIPTION, copybook.getDescription());
	   writeAttr(writer, Constants.RE_XML_FONTNAME, copybook.getFontName());
       writer.writeAttribute(Constants.RE_XML_FILESTRUCTURE,
    		   ExternalConversion.getFileStructureAsString(0, copybook.getFileStructure()));
       writer.writeAttribute(Constants.RE_XML_STYLE,
    		   ExternalConversion.getRecordStyleAsString(0, copybook.getRecordStyle()));
       writer.writeAttribute(Constants.RE_XML_RECORDTYPE,
    		   ExternalConversion.getRecordTypeAsString(0, copybook.getRecordType()));
       writer.writeAttribute(Constants.RE_XML_LISTCHAR, copybook.getListChar());
       if (copybook.isEmbeddedCr()) {
    	   writer.writeAttribute(Constants.RE_XML_EMBEDDED_CR, "Y");
       }
       if (copybook.isInitToSpaces()) {
    	   writer.writeAttribute(Constants.RE_XML_INIT_SPACES, "Y");
       }

       if (master != null && copybook.getParentRecord() >= 0) {
    	   try {
    		   writer.writeAttribute(Constants.RE_XML_PARENT,master.getRecord(copybook.getParentRecord()).getRecordName());
    	   } catch (Exception e) {
    	   }
       }
       writer.writeAttribute(Constants.RE_XML_QUOTE, copybook.getQuote());
       writer.writeAttribute(Constants.RE_XML_RECORDSEP, copybook.getRecSepList());
       s = copybook.getSystemName();
       if (s != null && ! "".equals(s)) {
    	   writer.writeAttribute(Constants.RE_XML_SYSTEMNAME, s);
       }
       ExternalSelection xSel = StreamLine.getExternalStreamLine().streamLine(copybook.getRecordSelection());
       if (copybook.isDefaultRecord()) {
    	   if (xSel == null) {
        	   writeAttr(writer, Constants.RE_XML_TESTFIELD, "");
        	   writeAttr(writer, Constants.RE_XML_TESTVALUE, "*");
        	   toPrint = false;
           }
       } else if (xSel == null) {
    	   toPrint = false;
       } else if (xSel instanceof ExternalFieldSelection) {
    	   ExternalFieldSelection eFld = (ExternalFieldSelection) xSel;
    	   if ("=".equals(eFld.getOperator()) || "eq".equalsIgnoreCase(eFld.getOperator())) {
	    	   writeAttr(writer, Constants.RE_XML_TESTFIELD, eFld.getFieldName());
	    	   writeAttr(writer, Constants.RE_XML_TESTVALUE, eFld.getRawFieldValue());
	    	   toPrint = false;
    	   }
       } else if (xSel instanceof ExternalGroupSelection) {
    	   toPrint = ((ExternalGroupSelection<?>) xSel).getSize() > 0;
       }
       writeAttr(writer, Constants.RE_XML_LINE_NO_FIELD_NAME, copybook.getLineNumberOfFieldNames(), 0);
       int recordLength = copybook.getRecordLength();
       int maxLength = getRecLength(copybook);
         
       
       if (recordLength > 0 && recordLength != maxLength) {
    	   writeAttr(writer, Constants.RE_XML_RECORDLENTH, recordLength, 0);
       }

       if (copybook.getNumberOfRecords() > 0) {
    	   writer.writeStartElement(Constants.RE_XML_RECORDS);

    	   for (i = 0; i < copybook.getNumberOfRecords(); i++) {
    		   writeRecord(writer, copybook, copybook.getRecord(i), hasGroup);
    	   }
    	   writer.writeEndElement();
       }

       if (toPrint) {
    	   writer.writeStartElement(Constants.RE_XML_TST_FIELDS);
    	   if (copybook.isDefaultRecord()) {
    		   writer.writeAttribute(Constants.RE_XML_DEFAULTREC, "Y");
    	   }

    	   writeSelection(writer, xSel);
    	   writer.writeEndElement();
       }

       if (copybook.getNumberOfRecordFields() > 0) {
    	   String lastGroupName = null;
    	   writer.writeStartElement(Constants.RE_XML_FIELDS);

    	   for (i = 0; i < copybook.getNumberOfRecordFields(); i++) {
    		   lastGroupName = writeField(writer, copybook.getRecordField(i), hasGroup, lastGroupName);
    	   }
    	   writer.writeEndElement();
       }

       writer.writeEndElement();
 	}

	private int getRecLength(BaseExternalRecord<?> r) {
		int ret = 0;
		if (r.getNumberOfRecordFields() > 0) {
			ExternalField recordField = r.getRecordField(r.getNumberOfRecordFields() - 1);
			ret = recordField.getPos() + recordField.getLen() - 1;
		}
     
        for (int i = 0; i < r.getNumberOfRecords(); i++) {
		   ret = Math.max(ret, getRecLength(r. getRecord(i)));
	    }
        return ret;
	}
	
	private String encodeDelim(String delim) {
		if ("\t".equals(delim)) {
			delim = "<TAB>";
		}
		
		return delim;
	}

	private void writeSelection(XMLStreamWriter writer, ExternalSelection sel)
	throws XMLStreamException {

	   if (sel == null) return;

       switch (sel.getType()) {
       case RecordSel.TYPE_ATOM:
    	   writeTstField(writer, (ExternalFieldSelection) sel);
    	   break;
       case RecordSel.TYPE_AND:
    	   writeSelectionGroup(writer, (ExternalGroupSelection<?>) sel, Constants.RE_XML_AND_FIELDS);
    	   break;
       case RecordSel.TYPE_OR:
    	   writeSelectionGroup(writer, (ExternalGroupSelection<?>) sel, Constants.RE_XML_OR_FIELDS);
    	   break;
       }
	}

	private void writeSelectionGroup(XMLStreamWriter writer, ExternalGroupSelection<?> g, String s)
			throws XMLStreamException {

		writer.writeStartElement(s);

	    for (int i = 0; i < g.size(); i++) {
	    	writeSelection(writer, g.get(i));
    	}

	    writer.writeEndElement();
	}

	/**
	 * Write one field
	 * @param writer XML Stream Writer
	 * @param fld field to write
	 * @throws XMLStreamException any error
	 */
	private String writeField(XMLStreamWriter writer, ExternalField fld, boolean hasGroups, String lastGroup)
	throws XMLStreamException {
		writer.writeEmptyElement(Constants.RE_XML_FIELD);
	    writer.writeAttribute(Constants.RE_XML_NAME, fld.getName());
	    writeAttr(writer, Constants.RE_XML_DESCRIPTION, fld.getDescription());
	    writer.writeAttribute(Constants.RE_XML_POS, Integer.toString(fld.getPos()));
	    if (fld.getLen() > 0) {
	    	writer.writeAttribute(Constants.RE_XML_LENGTH, Integer.toString(fld.getLen()));
	    }

	    if (fld.getDecimal() > 0 ) {
	    	writer.writeAttribute(Constants.RE_XML_DECIMAL, Integer.toString(fld.getDecimal()));
	    }

	    writer.writeAttribute(Constants.RE_XML_TYPE, ExternalConversion.getTypeAsString(ExternalConversion.USE_DEFAULT_DB, fld.getType()));
	    writeAttr(writer, Constants.RE_XML_CELLFORMAT, ExternalConversion.getFormatAsString(0, fld.getCellFormat()));
	    writeAttr(writer, Constants.RE_XML_PARAMETER, fld.getParameter());
	    writeAttr(writer, Constants.RE_XML_DEFAULT, fld.getDefault());
	    writeAttr(writer, Constants.RE_XML_COBOLNAME, fld.getCobolName());
	    //writer.writeAttribute(Constants.RE_XML_SUBKEY, Integer.toString(fld.getSubKey()));
	    if (hasGroups) {
	    	String g = fld.getGroup();
	    	if (lastGroup == null || ! lastGroup.equalsIgnoreCase(g)) {
	    		writeAttr(writer, Constants.RE_XML_GROUP_NAMES, fld.getGroup());
	    	}
	    	lastGroup = g;
	    }
	    return lastGroup;
	}


	private void writeTstField(XMLStreamWriter writer, ExternalFieldSelection fld)
	throws XMLStreamException {
		writer.writeEmptyElement(Constants.RE_XML_TST_FIELD);
		writeAttr(writer, Constants.RE_XML_NAME, fld.getFieldName());
	    writeAttr(writer, Constants.RE_XML_VALUE, fld.getRawFieldValue());
	}

	private void writeAttr(XMLStreamWriter writer, String attr, String value)
	throws XMLStreamException {
		if (value != null && ! "".equals(value)) {
			 writer.writeAttribute(attr, value);
		}
	}


	private void writeAttr(XMLStreamWriter writer, String attr, int value, int min)
	throws XMLStreamException {
		if (value > min) {
			writer.writeAttribute(attr, Integer.toString(value));
		}
	}

	/**
	 * fix nulls
	 * @param s input string
	 * @return
	 */
/*	private String fixString(String s) {
		String ret = s;
		if (ret == null) {
			ret = "";
		}

		return ret;
	}*/
}
