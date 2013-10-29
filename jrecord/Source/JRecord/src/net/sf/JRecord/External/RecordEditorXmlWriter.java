package net.sf.JRecord.External;

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
	public String writeCopyBook(String directory, ExternalRecord copybook,
			AbsSSLogger log) throws Exception {
		String fileName;
		directory = ExternalConversion.fixDirectory(directory);

		fileName = directory + ExternalConversion.fixFileName(copybook.getRecordName()) + Constants.XML_EXTENSION;
		writeCopyBook(new FileOutputStream(fileName),
				copybook, log);
		return fileName;
	}

	@Override
	public void writeCopyBook(OutputStream outStream, ExternalRecord copybook,
			AbsSSLogger log) throws Exception {
   	XMLOutputFactory f ;
   	XMLStreamWriter writer;

    	try {
    		 f = XMLOutputFactory.newInstance();
    	} catch (Exception e) {
    		// For Java 5 (before the addition of XMLOutputFactory to standard java
    		 Object o =  XMLOutputFactory.newInstance("javax.xml.stream.XMLOutputFactory",
					  this.getClass().getClassLoader());
    		 f = (XMLOutputFactory) o;
		}

       writer = f.createXMLStreamWriter(new OutputStreamWriter(outStream, STANDARD_FONT));

       writer.writeStartDocument(STANDARD_FONT, "1.0");

       writeRecord(writer, null, copybook);

       writer.writeEndDocument();
       writer.close();
	}


	/**
	 * Write a Record to the XML file
	 * @param writer where to write the XML
	 * @param copybook copybook to be written
	 * @throws XMLStreamException any error that occurs
	 */
	private void writeRecord(XMLStreamWriter writer, ExternalRecord master, ExternalRecord copybook) throws XMLStreamException {
	   int i;
	   boolean toPrint = true;
	   String name = copybook.getCopyBook();

	   writer.writeStartElement(Constants.RE_XML_RECORD);

       writer.writeAttribute(Constants.RE_XML_RECORDNAME,  copybook.getRecordName());
	   writer.writeAttribute(Constants.RE_XML_COPYBOOK, name);
	   writeAttr(writer, Constants.RE_XML_DELIMITER, copybook.getDelimiter());
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

       if (master != null && copybook.getParentRecord() >= 0) {
    	   try {
    		   writer.writeAttribute(Constants.RE_XML_PARENT,master.getRecord(copybook.getParentRecord()).getRecordName());
    	   } catch (Exception e) {
    	   }
       }
       writer.writeAttribute(Constants.RE_XML_QUOTE, copybook.getQuote());
       writer.writeAttribute(Constants.RE_XML_RECORDSEP, copybook.getRecSepList());
       //writer.writeAttribute(Constants.RE_XML_SYSTEMNAME, "");
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
	    	   writeAttr(writer, Constants.RE_XML_TESTVALUE, eFld.getFieldValue());
	    	   toPrint = false;
    	   }
       } else if (xSel instanceof ExternalGroupSelection) {
    	   toPrint = ((ExternalGroupSelection) xSel).getSize() > 0;
       }
       writeAttr(writer, Constants.RE_XML_LINE_NO_FIELD_NAME, copybook.getLineNumberOfFieldNames(), 0);

       if (copybook.getNumberOfRecords() > 0) {
    	   writer.writeStartElement(Constants.RE_XML_RECORDS);

    	   for (i = 0; i < copybook.getNumberOfRecords(); i++) {
    		   writeRecord(writer, copybook, copybook.getRecord(i));
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
    	   writer.writeStartElement(Constants.RE_XML_FIELDS);

    	   for (i = 0; i < copybook.getNumberOfRecordFields(); i++) {
    		   writeField(writer, copybook.getRecordField(i));
    	   }
    	   writer.writeEndElement();
       }

       writer.writeEndElement();
	}


	private void writeSelection(XMLStreamWriter writer, ExternalSelection sel)
	throws XMLStreamException {

	   if (sel == null) return;

       switch (sel.getType()) {
       case RecordSel.TYPE_ATOM:
    	   writeTstField(writer, (ExternalFieldSelection) sel);
    	   break;
       case RecordSel.TYPE_AND:
    	   writeGroup(writer, (ExternalGroupSelection) sel, Constants.RE_XML_AND_FIELDS);
    	   break;
       case RecordSel.TYPE_OR:
    	   writeGroup(writer, (ExternalGroupSelection) sel, Constants.RE_XML_OR_FIELDS);
    	   break;
       }
	}

	private void writeGroup(XMLStreamWriter writer, ExternalGroupSelection g, String s)
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
	private void writeField(XMLStreamWriter writer, ExternalField fld)
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
	}


	private void writeTstField(XMLStreamWriter writer, ExternalFieldSelection fld)
	throws XMLStreamException {
		writer.writeEmptyElement(Constants.RE_XML_TST_FIELD);
		writeAttr(writer, Constants.RE_XML_NAME, fld.getFieldName());
	    writeAttr(writer, Constants.RE_XML_VALUE, fld.getFieldValue());
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
