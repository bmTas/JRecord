package net.sf.JRecord.cbl2xml.impl;

import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.XMLStreamWriter;

import org.w3c.dom.Document;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.Cb2xmlDocument;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.ICobolCopybookLoader;
import net.sf.JRecord.External.XmlCopybookLoader;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.builders.CblIOBuilderMultiSchema;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Option.IRecordPositionOption;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.cbl2xml.def.Icb2xml2Xml;
import net.sf.JRecord.cbl2xml.jaxb.Condition;
import net.sf.JRecord.cbl2xml.jaxb.Copybook;
import net.sf.JRecord.cbl2xml.jaxb.Item;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;


/**
 * Purpose: Convert Cobol-Data-Files <---> Xml files
 *  
 * @author Bruce Martin
 *
 */
public class Cobol2GroupXml implements ICobol2Xml {
	
	private static final String STANDARD_FONT = "UTF-8"; 
	private String xmlMainElement = MAIN_XML_TAG;

	private final ICobolIOBuilder ioBuilder;
	
	private ExternalRecord externalRecord = null;
	private LayoutDetail schema;
	private ISchemaIOBuilder iob;
	private Copybook copybook = null;
	private ItemUpdater itemDtls = null;
	private boolean dropCopybook = false;
//	private int splitOption = CopybookLoader.SPLIT_NONE ;

	
	
	private Cobol2GroupXml(String copybookFilename, ICobolCopybookLoader loader) {
		loader.setSaveCb2xmlDocument(true);
		ioBuilder = new CblIOBuilderMultiSchema(copybookFilename, loader, ICopybookDialects.FMT_MAINFRAME);
	}

	private Cobol2GroupXml(InputStream is, String copybookname, ICobolCopybookLoader loader) throws IOException {
		loader.setSaveCb2xmlDocument(true);
		ioBuilder = new CblIOBuilderMultiSchema(is, copybookname, loader, ICopybookDialects.FMT_MAINFRAME);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2xml.def.ICobol2Xml#setFileOrganization(int)
	 */
	@Override
	public ICobol2Xml setFileOrganization(int fileOrganization) {
		ioBuilder.setFileOrganization(fileOrganization);
		reset();
		return this;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2xml.def.ICobol2Xml#setSplitCopybook(int)
	 */
	@Override
	public ICobol2Xml setSplitCopybook(int splitCopybook) {
		if (splitCopybook == CopybookLoader.SPLIT_REDEFINE ) {
			throw new RecordException("Split = SPLIT_REDEFINE is not supported !!!");
		}
		ioBuilder.setSplitCopybook(splitCopybook);
		reset();
//		this.splitOption = splitCopybook;
		return this;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2xml.def.ICobol2Xml#setDialect(int)
	 */
	@Override
	public ICobol2Xml setDialect(int dialect) {
		ioBuilder.setDialect(dialect);
		reset();
		return this;
	}

	/**
	 * @param dropCopybookNameFromFields
	 * @return
	 * @see net.sf.JRecord.def.IO.builders.ICobolIOBuilder#setDropCopybookNameFromFields(boolean)
	 */
	@Override
	public ICobol2Xml setDropCopybookNameFromFields(
			boolean dropCopybookNameFromFields) {
		ioBuilder.setDropCopybookNameFromFields(dropCopybookNameFromFields);
		dropCopybook = dropCopybookNameFromFields;
		return this;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2xml.def.ICobol2Xml#setFont(java.lang.String)
	 */
	@Override
	public ICobol2Xml setFont(String font) {
		ioBuilder.setFont(font);
		reset();
		return this;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2xml.def.ICobol2Xml#setRecordSelection(java.lang.String, net.sf.JRecord.ExternalRecordSelection.ExternalSelection)
	 */
	@Override
	public ICobol2Xml setRecordSelection(String recordName,
			ExternalSelection selectionCriteria) {
		ioBuilder.setRecordSelection(recordName, selectionCriteria);
		reset();
		return this;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2xml.def.ICobol2Xml#setCopybookFileFormat(int)
	 */
	@Override
	public ICobol2Xml setCopybookFileFormat(int copybookFileFormat) {
		ioBuilder.setCopybookFileFormat(copybookFileFormat);
		reset();
		return this;
	}


	/**
	 * @param recordName
	 * @param positionOption
	 * @return
	 * @see net.sf.JRecord.def.IO.builders.ICobolIOBuilder#setRecordPositionCode(java.lang.String, net.sf.JRecord.Option.IRecordPositionOption)
	 */
	public ICobol2Xml setRecordPositionCode(String recordName,
			IRecordPositionOption positionOption) {
		ioBuilder.setRecordPositionCode(recordName, positionOption);
		reset();
		return this;
	}
 
	/**
	 * @param xmlMainElement the xmlMainElement to set
	 */
	@Override
	public final ICobol2Xml setXmlMainElement(String xmlMainElement) {
		this.xmlMainElement = xmlMainElement;
		reset();
		return this;
	}
	
	

//	/* (non-Javadoc)
//	 * @see net.sf.JRecord.cbl2xml.def.ICobol2Xml#setLog(net.sf.JRecord.Log.AbsSSLogger)
//	 */
//	@Override
//	public ICobol2Xml setLog(AbsSSLogger log) {
//		// TODO Auto-generated method stub
//		return this;
//	}
//
//	/* (non-Javadoc)
//	 * @see net.sf.JRecord.cbl2xml.def.ICobol2Xml#setDropCopybookNameFromFields(boolean)
//	 */
//	@Override
//	public ICobol2Xml setDropCopybookNameFromFields(
//			boolean dropCopybookNameFromFields) {
//		ioBuilder.setDropCopybookNameFromFields(dropCopybookNameFromFields);
//		return this;
//	}

	
	private void reset() {
		externalRecord = null;
		copybook = null;
		
	}

	@Override
	public void cobol2xml(String cobolFileName, String xmlFileName) throws RecordException, IOException, JAXBException, XMLStreamException {
		cobol2xml(new FileInputStream(cobolFileName), new BufferedOutputStream(new FileOutputStream(xmlFileName), 0x4000));
	}
	
	@Override
	public void cobol2xml(InputStream cobolStream, OutputStream xmlStream) throws IOException, JAXBException, XMLStreamException {
		doInit();
		
        AbstractLineReader r = iob.newReader(cobolStream);
        LayoutDetail schema =  iob.getLayout();
        AbstractLine l;
       	XMLOutputFactory f = XMLOutputFactory.newInstance();
       	XMLStreamWriter writer = f.createXMLStreamWriter(new OutputStreamWriter(xmlStream, STANDARD_FONT));
        List<Item> items = copybook.getItem();

        writer.writeStartDocument(STANDARD_FONT, "1.0"); 
        writer.writeStartElement(xmlMainElement);
        
 		if (items.size() == 1) {
 			Item item = items.get(0);
	        while ((l = r.read()) != null) {
	        	writeItem(writer, l, item, new IntStack());
	        }
        } else if (schema.getRecordCount() > 1) {
        	int lineNo = 0;
        	while ((l = r.read()) != null) {
        		int recordIdx = l.getPreferredLayoutIdx();
        		lineNo += 1;
				if (recordIdx < 0) {
					throw new RecordException("Unknow Record Type for line number: " + lineNo + " " + l.getFullLine());
				}
				writeItem(writer, l, items.get(recordIdx), new IntStack());
        	}
        } else {
	        while ((l = r.read()) != null) {
	        	writer.writeStartElement("Line");
	        	writeItems(writer, l, items, new IntStack());
		        writer.writeEndElement();
	        }
       }
        writer.writeEndElement();


        writer.writeEndDocument();
        writer.close();
        xmlStream.close();
        r.close();
	}
	
	private void doInit() throws RecordException, IOException, JAXBException {
		if (copybook == null) {
			synchronized (this) {
				if (copybook == null) {
					externalRecord = ioBuilder.getExternalRecord();
					schema = externalRecord.asLayoutDetail();
					iob = JRecordInterface1.SCHEMA.newIOBuilder(schema);
					
					List<Cb2xmlDocument> cb2xmlDocuments = externalRecord.getCb2xmlDocuments();
					
					if (cb2xmlDocuments.size() != 1) {
						throw new RuntimeException("Expecting 1 cb2xml document but got: " + cb2xmlDocuments.size());
					} 
					
			        JAXBContext jc = JAXBContext.newInstance(Condition.class, Copybook.class, Item.class);
			        
			        Unmarshaller unmarshaller = jc.createUnmarshaller();
			        JAXBElement<Copybook> jaxbCopybook = unmarshaller.unmarshal(((Document) cb2xmlDocuments.get(0).cb2xmlDocument), Copybook.class);
			        copybook = jaxbCopybook.getValue();
			        itemDtls = new ItemUpdater(copybook, schema, dropCopybook, schema.getLayoutName());
				}
			}
		}
	}

	
	private void writeItem(XMLStreamWriter writer, AbstractLine l, Item item, IntStack indexs) throws XMLStreamException {

		String name = item.getName();
		if (name == null || item.getName().length() == 0 || "filler".equalsIgnoreCase(item.getName())) {
			if (item.itemType == Item.TYPE_GROUP) {
				if (item.getOccurs() != null && item.getOccurs() > 1) {
					writeArray(writer, l, item, "filler", indexs);
				} else {
					writeItems(writer, l, item.getItem(), indexs);
				}
			}
		} else if (item.getOccurs() != null && item.getOccurs() > 1) {
			writeArray(writer, l, item, item.nameToUse, indexs);
		} else {
			writer.writeStartElement(item.nameToUse);
			if (item.itemType == Item.TYPE_GROUP) {
				writeItems(writer, l, item.getItem(), indexs);
			} else if (indexs.size == 0) {
				writer.writeCharacters(l.getFieldValue(item.fieldDef).asString());
			} else {
				writer.writeCharacters(l.getFieldValue(item.arrayDef.getField(indexs.toArray())).asString());
			}
			writer.writeEndElement();
		}
	}

	/**
	 * @param writer
	 * @param l
	 * @param item
	 * @param indexs
	 * @throws XMLStreamException
	 */
	private void writeArray(XMLStreamWriter writer, AbstractLine l, Item item, String name,
			IntStack indexs) throws XMLStreamException {
		int num = item.getOccurs();
		String dependingOn = item.getDependingOn();
		if (dependingOn != null && dependingOn.length() > 0) {
			try {
				num = l.getFieldValue(dependingOn).asInt();
			} catch (Exception e) {
			}
		}
		
		indexs.add(0);
		int[] indexArray = indexs.toArray();
		for (int i = 0; i < num; i++) {
			writer.writeStartElement(name);
			if (item.itemType == Item.TYPE_GROUP) {
				writeItems(writer, l, item.getItem(), indexs.set(i));
			} else {
				indexArray[indexArray.length - 1] = i;
				writer.writeCharacters(l.getFieldValue(item.arrayDef.getField(indexArray)).asString());
			}
			writer.writeEndElement();
		}
		indexs.remove();
	}
	
	private void writeItems(XMLStreamWriter writer, AbstractLine l, List<Item> items, IntStack indexs) throws XMLStreamException {
		for (Item item : items) {
			writeItem(writer, l, item, indexs);
		}
	}
	
	@Override
	public void xml2Cobol(String xmlFileName, String cobolFileName) 
	throws RecordException, IOException, JAXBException, XMLStreamException {
		xml2Cobol(new FileInputStream(xmlFileName), new BufferedOutputStream(new FileOutputStream(cobolFileName), 0x4000));
	}

	@Override
	public void xml2Cobol(InputStream xmlStream, OutputStream cobolStream) 
	throws RecordException, IOException, JAXBException, XMLStreamException{
		//XMLInputFactory f = XMLInputFactory.newInstance();
		doInit();
		if (! schema.getDuplicateFieldNames().isEmpty()) {
			throw new RuntimeException("Duplicate names are not supported for Xml --> Cobol");
		}
//		String spaces = "                                                                                                  ";
		String lastName = "", name;
		int lvl = 0;
		int lastType, type = -1;
		XMLStreamReader parser = XMLInputFactory.newInstance().createXMLStreamReader(xmlStream);
		AbstractLine l = null;
		AbstractLineWriter w = iob.newWriter(cobolStream);		
		StringBuilder b = new StringBuilder();
		Map<String, Item> arrayItems = itemDtls.getArrayItems();
		IntStack arrayDtls = new IntStack();
		IntStack levelNames = new IntStack();



		
		while (parser.hasNext()) {
			lastType = type;
			type = parser.next();
			switch (type) {
            case XMLStreamConstants.START_ELEMENT:
            	lvl += 1;
            	name = parser.getName().toString();
            	if (lvl == 2) {
            		if (l != null) {
            			//System.out.println();
            			//System.out.println(l.getFullLine());
            			w.write(l);
            		}
            		l = iob.newLine();
            		if (schema.getRecordCount() > 1) {
            			int recIdx = schema.getRecordIndex(name);
            			if (recIdx >= 0) {
            				l.setWriteLayout(recIdx);
            			}
            		}
            	}
            	//System.out.println();
            	//System.out.print(spaces.substring(spaces.length() - 2 * lvl +1) + parser.getName() + " >");
            	if (name != null && arrayItems.containsKey(name.toUpperCase())) {
            		if (name.equalsIgnoreCase(levelNames.getLastName())) {
            			arrayDtls.inc();
            		} else {
            			arrayDtls.add(0, name);
            		}
            	}
            	
            	lastName = name;
            	levelNames.add(0, name);
            	b.setLength(0);
            	
            	
            	break;
            case XMLStreamConstants.END_ELEMENT:
            	String name2 = parser.getName().toString();
            	
				//System.out.print(b + "< " + name2 );
				
				if (lastName.equals(name2)) {
	        		AbstractFieldValue fieldValue;
	        		if (arrayDtls.size == 0) {
	        			fieldValue = l.getFieldValue(lastName);
	        		} else {
	        			fieldValue = l.getFieldValue(arrayDtls.toArrayIndex(lastName));
	        		}
		        	if (lastType == XMLStreamConstants.START_ELEMENT) {
						fieldValue.set(CommonBits.NULL_VALUE);
		        	} else {
		        		String txt = b.toString();
		        		if (fieldValue.isNumeric()) {
		        			txt = txt.trim();
		        		}
						fieldValue.set(txt);
		        	}
		        	b.setLength(0); 	
				}
				
				if (name2 != null && name2.equalsIgnoreCase(arrayDtls.getName())) {
					arrayDtls.remove();
				}
				levelNames.remove();
            	lvl -= 1;
            	break;
            case XMLStreamConstants.CHARACTERS:
            	String text = parser.getText();
            	
            	b.append(text);
            	
 //           	System.out.print(text.trim());
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
		}
		
		if (l != null) {
			w.write(l);
		}
		parser.close();
		xmlStream.close();
		w.close();
		cobolStream.close();
	}
	
	private static class IntStack {
		private int[] stack = new int[100];
		private String[] names = new String[100];
		private int size = 0;
		
		public IntStack add(int item) {
			stack[size++] = item;
			return this;
		}
		
		public IntStack add(int item, String name) {
			stack[size] = item;
			names[size++] = name;
			return this;
		}
		
		public IntStack set(int item) {
			stack[size - 1] = item;
			return this;
		}
		
		public IntStack inc() {
			size += 1;
			stack[size - 1] += 1;
			return this;
		}

		public String getName() {
			if (size <= 0) return "";
			
			return names[size-1];
		}

		public String getLastName() {
			if (size < 0) return "";
			
			return names[size];
		}

//		public int get(int idx) {
//			return stack[idx];
//		}
		
		public void remove() {
			names[size+1] = null;
			size -= 1;
		}
		
		public int[] toArray() {
			int[] ret = new int[size];
			System.arraycopy(stack, 0, ret, 0, size);
			return ret;
		}

		
		public String toArrayIndex(String name) {
			StringBuilder b = new StringBuilder(name);
			String sep = " (";
			
			for (int i = 0; i < size; i++) {
				b.append(sep).append(stack[i]);
				sep = ", ";
			}
			
			return b.append(')').toString();
		}
//		/**
//		 * @return the size
//		 */
//		public final int getSize() {
//			return size;
//		}
		
	}

	
	public static ICobol2Xml newCobol2Xml(String cobolCopybook) {
		return new Cobol2GroupXml(cobolCopybook, new CobolCopybookLoader());
	}
	
	
	public static ICobol2Xml newCobol2Xml(InputStream cobolCopybook, String copybookName) throws IOException {
		return new Cobol2GroupXml(cobolCopybook, copybookName, new CobolCopybookLoader());
	}

	
	public static Icb2xml2Xml newCb2Xml2Xml(String cobolCopybook) {
		return new Cobol2GroupXml(cobolCopybook, new XmlCopybookLoader());
	}
	
	
	public static Icb2xml2Xml newCb2Xml2Xml(InputStream cobolCopybook, String copybookName) throws IOException {
		return new Cobol2GroupXml(cobolCopybook, copybookName, new XmlCopybookLoader());
	}

}
