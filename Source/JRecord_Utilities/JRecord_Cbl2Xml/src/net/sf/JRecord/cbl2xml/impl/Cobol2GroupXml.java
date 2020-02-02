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

package net.sf.JRecord.cbl2xml.impl;

import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.XMLStreamWriter;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.fieldValue.IFieldValue;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.ICopybookLoaderCobol;
import net.sf.JRecord.External.XmlCopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.cbl2xml.def.Icb2xml2Xml;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;
import net.sf.JRecord.schema.CobolSchemaDetails;
import net.sf.JRecord.schema.CobolSchemaReader;
import net.sf.JRecord.schema.IArrayItemCheck;
import net.sf.JRecord.schema.ISchemaInformation;
import net.sf.JRecord.schema.fieldRename.IGetRecordFieldByName;
import net.sf.JRecord.schema.jaxb.IItem;
import net.sf.JRecord.schema.jaxb.Item;
import net.sf.JRecord.schema.jaxb.ItemRecordDtls;
//import net.sf.JRecord.schema.jaxb.Item;
import net.sf.JRecord.schema.jaxb.LineItemHelper;
import net.sf.JRecord.schema.jaxb.impl.DoNothingFormat;
import net.sf.JRecord.schema.jaxb.interfaces.IFormatField;


/**
 * Purpose: Convert Cobol-Data-Files <---> Xml files
 *  
 * @author Bruce Martin
 *
 */
public class Cobol2GroupXml extends CobolSchemaReader<ICobol2Xml> implements ICobol2Xml, ISchemaIOBuilder {
	
	private static final String STANDARD_FONT = "UTF-8"; 
	private String xmlMainElement = MAIN_XML_TAG;
	private CobolSchemaDetails cobolSchemaDetails = null;


	private LayoutDetail schema;

	private ISchemaInformation itemDtls = null;
	
	
	private XMLOutputFactory xmlOutputFactory = null;
	private XMLInputFactory  xmlInputFactory = null;
	
	private boolean skipValidation;
	
	private IFormatField formatField = DoNothingFormat.INSTANCE;


	private IGetRecordFieldByName fieldNameLookup = null;

	
	private Cobol2GroupXml(String copybookFilename, ICopybookLoaderCobol loader) {
		super(Conversion.getCopyBookId(copybookFilename), loader);
		loader.setSaveCb2xmlDocument(true);
		super.addCopyBook(copybookFilename);
		//ioBuilder = new CblIOBuilderMultiSchema(copybookFilename, loader, ICopybookDialects.FMT_MAINFRAME);
	}

	private Cobol2GroupXml(InputStream is, String copybookname, ICopybookLoaderCobol loader) {
		super(copybookname, loader);
		loader.setSaveCb2xmlDocument(true);
		
		super.addCopyBook(is, copybookname);
		//ioBuilder = new CblIOBuilderMultiSchema(is, copybookname, loader, ICopybookDialects.FMT_MAINFRAME);
	}

	private Cobol2GroupXml(Reader copybookReader, String copybookname, ICopybookLoaderCobol loader) {
		super(copybookname, loader);
		loader.setSaveCb2xmlDocument(true);
		
		super.addCopyBook(copybookReader, copybookname);
		//ioBuilder = new CblIOBuilderMultiSchema(is, copybookname, loader, ICopybookDialects.FMT_MAINFRAME);
	}


 
	/**
	 * @param xmlMainElement the xmlMainElement to set
	 */
	@Override
	public final ICobol2Xml setXmlMainElement(String xmlMainElement) {
		this.xmlMainElement = xmlMainElement;
		clearLayout();
		return this;
	}
	
	
	
	/**
//	 * @see net.sf.JRecord.cbl2xml.def.ICobol2Xml#setTagFormat(int)
//	 */
//	@Override
//	public ICobol2Xml setTagFormat(int tagFormat) {
//		this.tagFormat = tagFormat;
//		copybook = null;
//	
//		return this;
//	}
	


//	/* 
//	 * @see net.sf.JRecord.IO.builders.CblIOBuilderMultiSchemaBase#setSplitCopybook(int)
//	 */
//	@Override
//	public ICobol2Xml setSplitCopybook(int splitCopybook) {
//		if (splitCopybook == CopybookLoader.SPLIT_REDEFINE ) {
//			throw new RecordException("Split on redefines is not supported !!!");
//		}
//		return super.setSplitCopybook(splitCopybook);
//	}

	/**
	 * @param xmlInputFactory the xmlInputFactory to set
	 */
	@Override
	public final ICobol2Xml setXmlInputFactory(XMLInputFactory xmlInputFactory) {
		this.xmlInputFactory = xmlInputFactory;
		return this;
	}

	/**
	 * @param xmlOutputFactory the xmlOutputFactory to set
	 */
	@Override
	public final ICobol2Xml setXmlOutputFactory(XMLOutputFactory xmlOutputFactory) {
		this.xmlOutputFactory = xmlOutputFactory;
		return this;
	}


	@Override
	public final ICobol2Xml setFieldNameLookup(IGetRecordFieldByName fieldNameLookup) {
		this.fieldNameLookup = fieldNameLookup;
		
		return this;
	}

	/**
	 * @param formatField the formatField to set
	 */
	@Override
	public final ICobol2Xml setFormatField(IFormatField formatField) {
		this.formatField = formatField;
		return this;
	}

	@Override
	public void cobol2xml(String cobolFileName, String xmlFileName) throws RecordException, IOException, XMLStreamException {
		cobol2xml(new FileInputStream(cobolFileName), new BufferedOutputStream(new FileOutputStream(xmlFileName), 0x4000));
	}
	
	@Override
	public void cobol2xml(InputStream cobolStream, OutputStream xmlStream) throws IOException,  XMLStreamException {
		doInit();
		
        AbstractLineReader r = cobolSchemaDetails.ioBuilder.newReader(cobolStream);
        AbstractLine l;
       	XMLOutputFactory f = xmlOutputFactory ; //=  XMLOutputFactory.newInstance();
        if (f == null) {
        	f = XMLOutputFactory.newInstance();
        }
       	XMLStreamWriter writer = f.createXMLStreamWriter(new OutputStreamWriter(xmlStream, STANDARD_FONT));
       // List<? extends IItem> items = cobolSchemaDetails.cobolCopybook.getCobolItems(); 
       	List<ItemRecordDtls> recordItems = cobolSchemaDetails.recordItems;
        LineItemHelper lineHelper = new LineItemHelper(schema);
        
        writer.writeStartDocument(STANDARD_FONT, "1.0"); 
        writer.writeStartElement(xmlMainElement);
        
 		if (recordItems.size() == 1) {
 			List<Item> items = recordItems.get(0).items;
 			//String itemName = "Line";
 			if (items.size() == 1 && items.get(0).itemType == Item.TYPE_GROUP) {
		        while ((l = r.read()) != null) {
		        	writeItems(writer, lineHelper.setLine(l), items, new IntStack());
		        }
 			} else { 			
		        while ((l = r.read()) != null) {
		        	writer.writeStartElement("Line");
		        	writeItems(writer, lineHelper.setLine(l), items, new IntStack());
			        writer.writeEndElement();
		        }
 			}
        } else if (schema.hasTreeStructure()) {
           	ReadManager rm = new ReadManager(r, schema);
           	rm.read();
        	while (rm.line != null) {
        		writeItemInTree(writer, rm, recordItems);
        	}
        } else {
        	int lineNo = 0;
        	while ((l = r.read()) != null) {
        		int recordIdx = l.getPreferredLayoutIdx();
        		lineNo += 1;
				if (recordIdx < 0) {
					throw new RecordException("Unknow Record Type for line number: " + lineNo + " " + l.getFullLine());
				}
				ItemRecordDtls itemRecordDtls = recordItems.get(recordIdx);
				switch (itemRecordDtls.items.size()) {
				case 0: break;
				case 1:
					writeItem(writer, lineHelper.setLine(l), itemRecordDtls.items.get(0), new IntStack());
					break;
				default:
					writer.writeStartElement(itemRecordDtls.record.getRecordName());
					writeItems(writer, lineHelper, itemRecordDtls.items, new IntStack());
					writer.writeEndElement();
				}
	      	}
        }

        writer.writeEndElement();


        writer.writeEndDocument();
        writer.close();
        xmlStream.close();
        r.close();
	}
	
	private void doInit() throws IOException {
		cobolSchemaDetails = super.getCobolSchemaDetails();
		
		schema = cobolSchemaDetails.schema;
		itemDtls = cobolSchemaDetails.copybookInformation;
		
        skipValidation = ! itemDtls.isRedefinedBinaryField();
	}

	private void writeItemInTree(XMLStreamWriter writer, ReadManager rm, List<ItemRecordDtls> recordItems) 
	throws XMLStreamException, IOException {


		if (rm.recordIdx < 0) {
			throw new RecordException("Unknow Record Type for line number: " + rm.lineNumber + " " + rm.line.getFullLine());
		}

  		int recIdx = rm.recordIdx;
		List<Item> items = recordItems.get(recIdx).items;
		//IItem item = items;
		
		switch (items.size()) {
		case 0: break;
		case 1:
			writeOneItemInTree(writer, rm, recordItems, recIdx, items.get(0));
			break;
		default:
			writer.writeStartElement(recordItems.get(recIdx).record.getRecordName());
			for (Item item : items) {
				writeOneItemInTree(writer, rm, recordItems, recIdx, item);
			}
			writer.writeEndElement();
		} 
	}

	/**
	 * @param writer
	 * @param rm
	 * @param recordItems
	 * @param recIdx
	 * @throws XMLStreamException
	 * @throws IOException
	 */
	private void writeOneItemInTree(XMLStreamWriter writer, ReadManager rm, List<ItemRecordDtls> recordItems,
			int recIdx, IItem item ) throws XMLStreamException, IOException {
		writer.writeStartElement(item.getNameToUse());
		writeAnItem(writer, rm.lineItemHelper, item, new IntStack());
		
		rm.read();
		while (rm.line != null
		&& rm.recordIdx >= 0
		&& rm.schema.getRecord(rm.recordIdx).getParentRecordIndex() == recIdx) {
			writeItemInTree(writer, rm, recordItems);
		}
		
		writer.writeEndElement();
	}
	private void writeItem(XMLStreamWriter writer, LineItemHelper l, IItem item, IntStack indexs) throws XMLStreamException {

		String name = item.getName();
		if (name == null || item.getName().length() == 0 || "filler".equalsIgnoreCase(item.getName())) {
			if (item.getItemType() == IItem.TYPE_GROUP) {
				if (item.getOccurs() != null && item.getOccurs() > 1) {
					writeArray(writer, l, item, "filler", indexs);
				} else {
					writeItems(writer, l, item.getChildItems(), indexs);
				}
			}
		} else if (item.getOccurs() != null && item.getOccurs() > 1) {
			writeArray(writer, l, item, item.getNameToUse(), indexs);
		} else {
			writer.writeStartElement(item.getNameToUse());
			writeAnItem(writer, l, item, indexs);
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
	private void writeAnItem(XMLStreamWriter writer, LineItemHelper l, IItem item,
			IntStack indexs) throws XMLStreamException {
		if (item.getItemType() == IItem.TYPE_GROUP) {
			writeItems(writer, l, item.getChildItems(), indexs);
		} else if (indexs.size == 0) {
			writeText(writer, item, l.getFieldValue(item, null));
		} else {
			writeText(writer, item, l.getFieldValue(item, indexs.toArray()));
		}
	}

	/**
	 * @param writer
	 * @param item
	 * @param s
	 * @throws XMLStreamException
	 */
	public void writeText(XMLStreamWriter writer, IItem item, IFieldValue fieldValue)
			throws XMLStreamException {
		String s = formatField.format(item, fieldValue.getFieldDetail(), fieldValue.asString());
		int len;
		if (s != null && (len = s.length() - 1) >= 0) {
			if (s.charAt(len) == 0) {
				while( len > 0 && s.charAt(len-1) == 0) {
					len -= 1;
				}
				
				s = s.substring(0, len);
			}
			
			if (skipValidation || (! item.isFieldRedefined()) || isValidString(s)) {
				writer.writeCharacters(s);
			}
		}
	}

	/**
	 * @param fl
	 */
	private static boolean isValidString(String fl) {
		for (int j = 0; j < fl.length(); j++) {
			char ch = fl.charAt(j);
			switch (Character.getType(ch)) {
			case  Character.COMBINING_SPACING_MARK:
			case  Character.CONNECTOR_PUNCTUATION:
			case  Character.CURRENCY_SYMBOL:
			case  Character.DASH_PUNCTUATION:
			case  Character.DECIMAL_DIGIT_NUMBER:
			case  Character.ENCLOSING_MARK:
			case  Character.END_PUNCTUATION:
			case  Character.FINAL_QUOTE_PUNCTUATION:
			case  Character.FORMAT:
			case  Character.INITIAL_QUOTE_PUNCTUATION:
			case  Character.LETTER_NUMBER:
			case  Character.LINE_SEPARATOR:
			case  Character.LOWERCASE_LETTER:
			case  Character.MATH_SYMBOL:
			case  Character.MODIFIER_LETTER:
			case  Character.MODIFIER_SYMBOL:
			case  Character.NON_SPACING_MARK:
			case  Character.OTHER_LETTER:
			case  Character.OTHER_NUMBER:
			case  Character.OTHER_PUNCTUATION:
			case  Character.OTHER_SYMBOL:
			case  Character.PARAGRAPH_SEPARATOR:
			case  Character.SPACE_SEPARATOR:
			case  Character.START_PUNCTUATION:
			case  Character.SURROGATE:
			case  Character.TITLECASE_LETTER:
			case  Character.UPPERCASE_LETTER:
				break;
			// Should be: Character.CONTROL, Character.UNASSIGNED, Character.PRIVATE_USE
			default:
				//System.out.print('*' + " " + ch + "~" + Character.getType(ch));
				return false;
			}
		}
		return true;
	}

	/**
	 * @param writer
	 * @param l
	 * @param item
	 * @param indexs
	 * @throws XMLStreamException
	 */
	private void writeArray(XMLStreamWriter writer, LineItemHelper l, IItem item, String name,
			IntStack indexs) throws XMLStreamException {
		int num = l.getArrayCount(item, indexs.toArray());
//		String dependingOn = item.getDependingOn();
//		IOccursDependingDetails fieldLookup = item.getDependingOnDetails();
//
//		if (fieldLookup.isDependingOnArray()) {
//			try {
//				num = fieldLookup.getValue(l);
//			} catch (Exception e) {
//			}
//		} else if (item.getArrayValidation() != null) {
//			num = item.getArrayValidation().getCount(l, item, indexs.toArray(), num);
//		}
		 
		indexs.add(0);
		int[] indexArray = indexs.toArray();
		int id;
		for (int i = 0; i < num; i++) {
			if (item.getArrayValidation() != null
			&& (id = l.checkArrayIndex(item, indexArray, i)) != IArrayItemCheck.R_PROCESS ) {
				if (id == IArrayItemCheck.R_STOP) {
					break;
				}
			} else { 
				writer.writeStartElement(name);
				if (item.getItemType() == IItem.TYPE_GROUP) {	
					writeItems(writer, l, item.getChildItems(), indexs.set(i));
				} else {
					indexArray[indexArray.length - 1] = i;

					writer.writeCharacters(l.getFieldValue(item, indexArray).asString());
				}
				writer.writeEndElement();
			}
		}
		indexs.remove();
	}
	
	private void writeItems(XMLStreamWriter writer, LineItemHelper l, List<? extends IItem> items, IntStack indexs) throws XMLStreamException {
		for (IItem item : items) {
			writeItem(writer, l, item, indexs);
		}
	}
	
	@Override
	public void xml2Cobol(String xmlFileName, String cobolFileName) 
	throws RecordException, IOException, XMLStreamException {
		xml2Cobol(new FileInputStream(xmlFileName), new BufferedOutputStream(new FileOutputStream(cobolFileName), 0x4000));
	}

	@Override
	public void xml2Cobol(InputStream xmlStream, OutputStream cobolStream) 
	throws RecordException, IOException, XMLStreamException{
		//XMLInputFactory f = XMLInputFactory.newInstance();
		doInit();
		if (itemDtls.getDuplicateFieldsStatus() == ISchemaInformation.D_DUPLICATES) {
			throw new RuntimeException("Duplicate names are not supported for Xml --> Cobol");
		}
//		String spaces = "                                                                                                  ";
		String lastName = "", name;
		int lvl = 0;
		int lastType, type = -1;
		XMLStreamReader parser = (xmlInputFactory==null
										? XMLInputFactory.newInstance()
										: xmlInputFactory
								 ) 				.createXMLStreamReader(xmlStream);
		AbstractLine l = null;
		AbstractLineWriter w = cobolSchemaDetails.ioBuilder.newWriter(cobolStream);		
		StringBuilder b = new StringBuilder();
		Map<String, ? extends IItem> arrayItems = itemDtls.getArrayItems();
		IntStack arrayDtls = new IntStack();
		IntStack levelNames = new IntStack();
		Map<String, Integer> recordHierarchyMap = itemDtls.getRecordHierarchyMap();
		int maxHierarchLvl = itemDtls.getMaxRecordHierarchyLevel();
		String recordName = "";
		Integer lookupLvl;
		boolean lastWasArray = false;
		ArrayList<String> tagNames = new ArrayList<String>();
		
		if (fieldNameLookup == null) {
			fieldNameLookup = itemDtls.getFieldLookup();
		}
		fieldNameLookup.setSchema(schema);
		
		while (parser.hasNext()) {
			lastType = type;
			type = parser.next();
			switch (type) {
            case XMLStreamConstants.START_ELEMENT:
            	lvl += 1;
            	name = parser.getName().toString();
            	if (lvl == 2 
                || (     maxHierarchLvl >= lvl - 3
                   &&	(lookupLvl = recordHierarchyMap.get(name.toUpperCase())) != null
             	   &&    lookupLvl >= lvl - 3 
             		)) {
            		if (l != null) {
//           			System.out.println();
//            			System.out.println(l.getFullLine());
            			w.write(l);
            		}
        			recordName = name;

            		l = cobolSchemaDetails.ioBuilder.newLine();
            		if (schema.getRecordCount() > 1) {
            			int recIdx = fieldNameLookup.getRecordIndex(name); // schema.getRecordIndex(name);
            			if (recIdx >= 0) {
            				l.setWriteLayout(recIdx);
            			}
            		}
//            	} else if ((lookupLvl = recordHierarchyMap.get(name.toUpperCase())) != null) {
//            		System.out.println("## " + name + " " + lookupLvl + " " + lvl );
//            	} else if (lvl <= 4 && name.indexOf("Record") >= 0) {
//            		System.out.println("** " + name + " " + lvl );
            	}
//            	System.out.println();
//           	System.out.print(spaces.substring(spaces.length() - 2 * lvl +1) + parser.getName() + " >");
            	String ucName;
            	if (name != null && arrayItems.containsKey((ucName = name.toUpperCase()))) {
            		if (name.equalsIgnoreCase(levelNames.getLastName())) {
            			arrayDtls.inc();
            		} else {
            			arrayDtls.add(0, name, arrayItems.get(ucName));
            		}
            	}
            	
            	lastName = name;
            	levelNames.add(0, name, null);
            	tagNames.add(name);
            	b.setLength(0);
            	
            	
            	break;
            case XMLStreamConstants.END_ELEMENT:
            	String name2 = parser.getName().toString();
    			int[] indexes = arrayDtls.toArray();
            	
//				System.out.print(b + "< " + name2 );
				
				if (lastName.equals(name2)) {
	        		IFieldDetail f;
	        		//String n = lastName;
	        		
	        		f = fieldNameLookup.getField(recordName, tagNames, indexes);
	        		
	        		if (f == null) {
	        			if (b.length() > 0) {
	        				//f = fieldLookup.getField(recordName, tagNames, indexes);
	        				throw new RuntimeException("Field: " + lastName + " does not exist, can not assign '" + b.toString() + "'");
	        			}
	        		} else {
		        		AbstractFieldValue fieldValue = l.getFieldValue(f);
			        	if (lastType == XMLStreamConstants.START_ELEMENT) {
							fieldValue.set(CommonBits.NULL_VALUE);
			        	} else {
			        		String txt = b.toString();
			        		if (fieldValue.isNumeric()) {
			        			txt = txt.trim();
			        		}
							fieldValue.set(txt);
			        	}
	        		}
		        	b.setLength(0);
		        	tagNames.remove(tagNames.size() - 1);
				}
				
				if (lastWasArray) {
					IItem item = arrayDtls.getLastItem();
					if (item != null && item.getArrayValidation() != null && arrayDtls.size >= 0) {
						item.getArrayValidation().updateForCount(l, item, indexes, arrayDtls.stack[arrayDtls.size]+1);
					}					
				}
				lastWasArray = false;
				if (name2 != null && name2.equalsIgnoreCase(arrayDtls.getName())) {
					arrayDtls.remove();
					lastWasArray = true;
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
	
	
	/**
	 * Class to keep track of Cobol Group Levels
	 * (in particular Groups that are arrays) 
	 * @author Bruce Martin
	 *
	 */
	private static class IntStack {
		private int[] stack = new int[100];
		private String[] names = new String[100];
		private IItem[] items = new IItem[100];
		private int size = 0;
		
		public IntStack add(int item) {
			stack[size++] = item;
			return this;
		}
		
		public IntStack add(int pos, String name, IItem item) {
			stack[size] = pos;
			items[size] = item;
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

		public IItem getLastItem() {
			if (size < 0) return null;
			return items[size];
		}
		
		public void remove() {
			names[size+1] = null;
			size -= 1;
		}
		
		public int[] toArray() {
			int[] ret = new int[size];
			System.arraycopy(stack, 0, ret, 0, size);
			return ret;
		}	
	}

	
	public static ICobol2Xml newCobol2Xml(String cobolCopybook) {
		return new Cobol2GroupXml(cobolCopybook, new CobolCopybookLoader());
	}
	
	
	public static ICobol2Xml newCobol2Xml(InputStream cobolCopybook, String copybookName) {
		return new Cobol2GroupXml(cobolCopybook, copybookName, new CobolCopybookLoader());
	}
	
	public static ICobol2Xml newCobol2Xml(Reader cobolCopybookReader, String copybookName) {
		return new Cobol2GroupXml(cobolCopybookReader, copybookName, new CobolCopybookLoader());
	}

	
	public static Icb2xml2Xml newCb2Xml2Xml(String cobolCopybook) {
		return new Cobol2GroupXml(cobolCopybook, new XmlCopybookLoader());
	}
	
	
	public static Icb2xml2Xml newCb2Xml2Xml(InputStream cobolCopybook, String copybookName) {
		return new Cobol2GroupXml(cobolCopybook, copybookName, new XmlCopybookLoader());
	}

	
	private static class ReadManager {
		final AbstractLineReader reader;
		final LayoutDetail schema;
		AbstractLine line;
		int recordIdx, lineNumber = 0;
		final LineItemHelper lineItemHelper;
		ReadManager(AbstractLineReader r, LayoutDetail schema) {
			super();
			this.reader = r;
			this.schema = schema;
			lineItemHelper = new LineItemHelper(schema);
		}
		
		void read() throws IOException {
			line = reader.read();
			if (line != null) {
				lineNumber += 1;
				recordIdx = line.getPreferredLayoutIdx();
				lineItemHelper.setLine(line);
			}
		}
	}
}
