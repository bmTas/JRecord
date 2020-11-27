package net.sf.cobolToJson.impl;

import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.math.BigDecimal;
import java.util.List;



import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.ICopybookLoaderCobol;
import net.sf.JRecord.External.XmlCopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;
import net.sf.JRecord.schema.CobolSchemaDetails;
import net.sf.JRecord.schema.CobolSchemaReader;
import net.sf.JRecord.schema.IArrayItemCheck;
import net.sf.JRecord.schema.ISchemaInformation;
import net.sf.JRecord.schema.jaxb.IItem;
import net.sf.JRecord.schema.jaxb.Item;
import net.sf.JRecord.schema.jaxb.ItemRecordDtls;
import net.sf.JRecord.schema.jaxb.LineItemHelper;
//import net.sf.JRecord.schema.jaxb.Item;
import net.sf.cobolToJson.def.ICobol2Json;
import net.sf.cobolToJson.def.Icb2xml2Json;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;


/**
 * Purpose: Convert Cobol-Data-Files <---> Xml files
 *  
 * @author Bruce Martin
 *
 */
public class Cobol2JsonImp extends CobolSchemaReader<ICobol2Json> implements ICobol2Json {

	
//	private ISchemaIOBuilder iob;
//	private Copybook copybook = null;
	private ISchemaInformation itemDtls = null;
	
	private CobolSchemaDetails cobolSchemaDetails = null;

	private boolean skipValidation, prettyPrint = true;

	
//	private HashMap<String, IArrayItemCheck> arrayChecks = new HashMap<String, IArrayItemCheck>();
//	private boolean dropCopybook = false;
//	private int splitOption = CopybookLoader.SPLIT_NONE ;

	
	
	private Cobol2JsonImp(String copybookFilename, ICopybookLoaderCobol loader) {
		super(Conversion.getCopyBookId(copybookFilename), loader);
		loader.setSaveCb2xmlDocument(true);
		super.addCopyBook(copybookFilename);
		//ioBuilder = new CblIOBuilderMultiSchema(copybookFilename, loader, ICopybookDialects.FMT_MAINFRAME);
	}

	private Cobol2JsonImp(InputStream is, String copybookname, ICopybookLoaderCobol loader) {
		super(copybookname, loader);
		loader.setSaveCb2xmlDocument(true);
		
		super.addCopyBook(is, copybookname);
		//ioBuilder = new CblIOBuilderMultiSchema(is, copybookname, loader, ICopybookDialects.FMT_MAINFRAME);
	}

	private Cobol2JsonImp(Reader copybookReader, String copybookname, ICopybookLoaderCobol loader) {
		super(copybookname, loader);
		loader.setSaveCb2xmlDocument(true);
		
		super.addCopyBook(copybookReader, copybookname);
	}


	
	@Override
	public void cobol2json(String cobolFileName, String jsonFileName) throws IOException  {
		cobol2json(new FileInputStream(cobolFileName), new BufferedOutputStream(new FileOutputStream(jsonFileName), 0x4000));
	}
	
	@Override
	public void cobol2json(InputStream cobolStream, OutputStream jsonStream) throws IOException {
		doInit();
		
		ISchemaIOBuilder iob = cobolSchemaDetails.ioBuilder;
        AbstractLineReader r = iob.newReader(cobolStream);
        LayoutDetail schema =  iob.getLayout();
        AbstractLine l;
        LineItemHelper lineItemHelper = new LineItemHelper(schema);
       	JsonGenerator writer = new JsonFactory().createGenerator(jsonStream);
        //List<? extends IItem> items = cobolSchemaDetails.cobolCopybook.getCobolItems();
       	List<ItemRecordDtls> recordItems = cobolSchemaDetails.recordItems;

        if (prettyPrint) {
        	writer.setPrettyPrinter(new DefaultPrettyPrinter());
        }
        
 		try {
	    	writer.writeStartObject();
			if (recordItems.size() == 1) {
				writer.writeArrayFieldStart(itemDtls.updateName(schema.getRecord(0).getRecordName()));
			    while ((l = r.read()) != null) {
			    	writer.writeStartObject();
			    	writeItems(writer, lineItemHelper.setLine(l), recordItems.get(0).items, new IntStack());
			        writer.writeEndObject();
			    }
			} else if (schema.hasTreeStructure()) {
			   	ReadManager rm = new ReadManager(r, schema);
				String rootRecordName = super.getRootRecord();
			   	rm.read();
			   	
			   	int rootIdx = -1;
				if (rootRecordName == null) {
					for (int i =0; i < schema.getRecordCount(); i++) {
						if (schema.getRecord(i).getParentRecordIndex() < 0) {
							rootRecordName = schema.getRecord(i).getRecordName();
							rootIdx = i;
							break;
						}
					}
				} else {
					rootIdx = schema.getRecordIndex(rootRecordName);
				}
					
				if (rootRecordName == null) {
				   	//writer.writeStartArray();
					writer.writeArrayFieldStart(cobolSchemaDetails.copybookInformation.updateName(cobolSchemaDetails.schema.getLayoutName()));
					while (rm.line != null) {
						writer.writeStartObject();
						writeItemInTree(writer, rm, recordItems);
						writer.writeEndObject();
					}
				} else {
					if (rootIdx < 0) { throw new RecordException("Root Record: " + rootRecordName + " does not exist" ); }
					RecordDetail rec = schema.getRecord(rootIdx);
					if (rec.getParentRecordIndex() >= 0) {new RecordException("Root Record: " + rootRecordName + " has a parent record ???" ); }
					
					
					for (int i =0; i < schema.getRecordCount(); i++) {
						if (i != rootIdx && schema.getRecord(i).getParentRecordIndex() < 0) {
							throw new RecordException("Schema has more than one root: " + schema.getRecord(i).getRecordName());
						}
					}
					if (rm.recordIdx != rootIdx) {
						if (rm.recordIdx < 0) {
							throw new RecordException("Can not determine the record type for the first record");
						}
						throw new RecordException("Invalid First record, it should be: " + rec.getRecordName() 
								+ " and not " + schema.getRecord(rm.recordIdx).getRecordName());
					}
					writer.writeArrayFieldStart(itemDtls.updateName(schema.getRecord(rm.recordIdx).getRecordName()));
				    while (rm.line != null) {
						if (rm.recordIdx < 0) {
							throw new RecordException("Can not determine the record type for the " + rm.lineNumber + " record");
						}
						
						ItemRecordDtls recordDtls = recordItems.get(rm.recordIdx);

						switch (recordDtls.items.size()) {
						case 0: break;
						case 1:
							writeSingleTreeItemRecord(writer, rm, recordItems, rm.recordIdx, recordDtls.items.get(0));
							break;
						default:
							writeMultiTreeItemRecords(writer, rm, recordItems, rm.recordIdx, recordDtls);
						}
				    }
				}		
			} else {
				int lineNo = 0;
				//writer.writeStartArray();
				writer.writeArrayFieldStart(cobolSchemaDetails.copybookInformation.updateName(cobolSchemaDetails.schema.getLayoutName()));
				while ((l = r.read()) != null) {
					int recordIdx = l.getPreferredLayoutIdx();
					lineNo += 1;
					if (recordIdx < 0) {
						throw new RecordException("Unknow Record Type for line number: " + lineNo + " " + l.getFullLine());
					}
					writer.writeStartObject();
					//writer.writeObjectFieldStart(fieldName);
					writeItems(writer, lineItemHelper.setLine(l), recordItems.get(recordIdx).items, new IntStack());
					//writer.writeEndObject();
					writer.writeEndObject();
				}
			}

			writer.writeEndArray();
			writer.writeEndObject();
		} finally {
	        try {
				writer.flush();
				writer.close();
				jsonStream.close();
				r.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
 		
	}
	
	private void doInit() throws IOException {
		cobolSchemaDetails = super.getCobolSchemaDetails();

		itemDtls = cobolSchemaDetails.copybookInformation;
		
        skipValidation = ! itemDtls.isRedefinedBinaryField();
	}

	/**
	 * @param prettyPrint the prettyPrint to set
	 */
	@Override
	public final void setPrettyPrint(boolean prettyPrint) {
		this.prettyPrint = prettyPrint;
	}

	private void writeItemInTree(JsonGenerator writer, ReadManager rm, List<ItemRecordDtls> recordItems) 
	throws IOException {


		if (rm.recordIdx < 0) {
			throw new RecordException("Unknow Record Type for line number: " + rm.lineNumber + " " + rm.line.getFullLine());
		}

  		int recIdx = rm.recordIdx;
  		ItemRecordDtls recordDtls = recordItems.get(recIdx);
  		
  		switch (recordDtls.items.size()) {
  		case 0: break;
  		case 1:
			writeSingleTreeItemRecord(writer, rm, recordItems, recIdx, recordDtls.items.get(0));
			break;
		default:
			writeMultiTreeItemRecords(writer, rm, recordItems, recIdx, recordDtls);
  		}
	}
	
	/**
	 * @param writer
	 * @param rm
	 * @param recordItems
	 * @param recIdx
	 * @param recordDtls
	 * @throws IOException
	 */
	private void writeMultiTreeItemRecords(JsonGenerator writer, ReadManager rm, List<ItemRecordDtls> recordItems,
			int recIdx, ItemRecordDtls recordDtls) throws IOException {
		writer.writeStartObject();
		for (Item itm : recordDtls.items) {
			writeSingleTreeItemRecord(writer, rm, recordItems, recIdx, itm);
		}	
		writer.writeEndObject();
	}


	/**
	 * @param writer
	 * @param rm
	 * @param recordItems
	 * @param recIdx
	 * @param item
	 * @throws IOException
	 */
	private void writeSingleTreeItemRecord(JsonGenerator writer, ReadManager rm, List<ItemRecordDtls> recordItems,
			int recIdx, IItem item) throws IOException {
		
		writer.writeObjectFieldStart(item.getNameToUse());
		writeTreeItemRecord(writer, rm, recordItems, recIdx, item);
		writer.writeEndObject();
	}

	/**
	 * @param writer
	 * @param rm
	 * @param items
	 * @param recIdx
	 * @param item
	 * @throws IOException
	 */
	public void writeTreeItemRecord(JsonGenerator writer, ReadManager rm,
			List<ItemRecordDtls> recordItems, int recIdx, IItem item) throws IOException {
		
		if (item.getItemType() == IItem.TYPE_GROUP) {
			writeAnItem(writer, null, rm.lineItemHelper, item, new IntStack());
			readChildren(writer, rm, recordItems, recIdx);
		} else {
			writer.writeStartObject();
			writeAnItem(writer, null, rm.lineItemHelper, item, new IntStack());
			readChildren(writer, rm, recordItems, recIdx);
			writer.writeEndObject();
		}
	}

	/**
	 * @param writer
	 * @param rm
	 * @param items
	 * @param recIdx
	 * @throws IOException
	 */
	public void readChildren(JsonGenerator writer, ReadManager rm,
			List<ItemRecordDtls> recordItems, int recIdx) throws IOException {
		rm.read();
		int lastIdx = -121;
		boolean first = true;
		while (rm.line != null
		&& rm.recordIdx >= 0
		&& rm.schema.getRecord(rm.recordIdx).getParentRecordIndex() == recIdx) {
			ItemRecordDtls recordDtls = recordItems.get(rm.recordIdx);
			RecordDetail record = recordDtls.record;
			
			switch (recordDtls.items.size()) {
			case 0: break;
			case 1:
				Item item = recordDtls.items.get(0);
				if (first) {
					writer.writeArrayFieldStart(item.getNameToUse());
				}  else if (lastIdx != rm.recordIdx) {
					writer.writeEndArray();
					writer.writeArrayFieldStart(item.getNameToUse());
				}
				lastIdx = rm.recordIdx;

				writeSingleTreeItemRecord(writer, rm, recordItems, rm.recordIdx, item);
			default:
				if (first) {
					writer.writeArrayFieldStart(record.getRecordName());
				}  else if (lastIdx != rm.recordIdx) {
					writer.writeEndArray();
					writer.writeArrayFieldStart(record.getRecordName());
				}
				lastIdx = rm.recordIdx;

				writeMultiTreeItemRecords(writer, rm, recordItems, recIdx, recordDtls);
			}
			
			first = false;
		}
		
		if (lastIdx >= 0) {
			writer.writeEndArray();
		}
	}
	
	private void writeItem(JsonGenerator writer, LineItemHelper l, IItem item, IntStack indexs) throws IOException {

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
			writeAnItem(writer, item.getNameToUse(), l, item, indexs);
		}
	}

	/**
	 * @param writer
	 * @param l
	 * @param item
	 * @param indexs
	 * @throws IOException 
	 */
	private void writeAnItem(JsonGenerator writer, String fieldname, LineItemHelper l, IItem item,
			IntStack indexs) throws IOException {
		if (item.getItemType() == IItem.TYPE_GROUP) {
			if (fieldname == null) {
				writeItems(writer, l, item.getChildItems(), indexs);
			} else {
				writer.writeObjectFieldStart(fieldname);
				writeItems(writer, l, item.getChildItems(), indexs);
				writer.writeEndObject();
			}
		} else if (indexs.size == 0) {
			//writer.writeCharacters(l.getFieldValue(item.fieldDef).asString());
			writeField(writer, fieldname, l.getFieldValue(item, null), item);
		} else {
			//writer.writeCharacters(l.getFieldValue(item.arrayDef.getField(indexs.toArray())).asString());
			writeField(writer, fieldname, l.getFieldValue(item, indexs.toArray()), item);
		}
	}

	/**
	 * @param writer
	 * @param l
	 * @param item
	 * @param indexs
	 * @throws IOException 
	 */
	private void writeArray(JsonGenerator writer, LineItemHelper l, IItem item, String name,
			IntStack indexs) throws IOException {
		int num = l.getArrayCount(item, indexs.toArray());
//		String dependingOn = item.getDependingOn();
//		IOccursDependingDetails fieldLookup = item.getDependingOnDetails();
//
//		int[] indexArray = indexs.toArray();
//		if (fieldLookup.isDependingOnArray()) {
//			try {
//				num = fieldLookup.getValue(l);
//			} catch (Exception e) { }
//		} else if (item.getArrayValidation() != null) {
//			num = item.getArrayValidation().getCount(l, item, indexArray, num);
//		}
		
		indexs.add(0);
		int[] indexArray = indexs.toArray();
		int id;
		
		boolean writeArrayStart = true;
		for (int i = 0; i < num; i++) {
			if (item.getArrayValidation() != null
			&& (id = l.checkArrayIndex(item, indexArray, i)) != IArrayItemCheck.R_PROCESS ) {
				if (id == IArrayItemCheck.R_STOP) {
					break;
				}
			} else { 
				if (writeArrayStart) {
					writer.writeArrayFieldStart(name);
					writeArrayStart = false;
				}
				if (item.getItemType() == IItem.TYPE_GROUP) {
					writer.writeStartObject();
					writeItems(writer, l, item.getChildItems(), indexs.set(i));
					writer.writeEndObject();
				} else {
					indexArray[indexArray.length - 1] = i;
					//writer.writeCharacters(l.getFieldValue(item.arrayDef.getField(indexArray)).asString());
					AbstractFieldValue val = l.getFieldValue(item, indexArray);
					String s = null;
					if (val.isNumeric()) {
						BigDecimal bd = getNumVal(val);
						if (bd != null) {
							writer.writeNumber(val.asBigDecimal());
						} else {
							s = "";
						}
					} else {
						if ((s = getStrValue(val, item)) == null) {
							s = "";
						}
					}
					
					if (s != null) {
						writer.writeString(val.asString());
					}
				}
//				writer.writeEndArray();
			}
		}
		if (! writeArrayStart) {
			writer.writeEndArray();
		}
		indexs.remove();
	}
	
	private void writeItems(JsonGenerator writer, LineItemHelper l, List<? extends IItem> items, IntStack indexs) throws IOException {
		for (IItem item : items) {
			writeItem(writer, l, item, indexs);
		}
	}
	
	private void writeField(JsonGenerator writer, String fieldName, AbstractFieldValue val, IItem item) throws IOException {
		try {
			String s ;
			if (val.isNumeric()) {
				try {
					BigDecimal bd = getNumVal(val);
					if (bd != null) {
						writer.writeNumberField(fieldName, bd);
					}
				} catch (Exception e) {	}
			} else if ((s = getStrValue(val, item)) != null && s.trim().length() > 0){
				writer.writeStringField(fieldName, s);
			}
		} catch (Exception e) {
		}
	}
	
	private BigDecimal getNumVal(AbstractFieldValue val) {
		BigDecimal ret = null;
		
		try {
			ret = val.asBigDecimal();
		} catch (Exception e) {	}
		
		return ret;
	}
	
	private String getStrValue(AbstractFieldValue val, IItem item) {
		String ret = null;
		
		try {
			String s = val.asString();
			if ( s != null &&(skipValidation || (! item.isFieldRedefined()) || isValidString(s))) {
				ret = s;
			}
		} catch (Exception e) {	}
		
		return ret;
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

//	@Override
//	public void json2Cobol(String xmlFileName, String cobolFileName) 
//	throws RecordException, IOException,  XMLStreamException {
//		json2Cobol(new FileInputStream(xmlFileName), new BufferedOutputStream(new FileOutputStream(cobolFileName), 0x4000));
//	}
//
//	@Override
//	public void json2Cobol(InputStream xmlStream, OutputStream cobolStream) 
//	throws RecordException, IOException,  XMLStreamException{
//		//XMLInputFactory f = XMLInputFactory.newInstance();
//		doInit();
//		if (itemDtls.getDuplicateFieldsStatus() == UpdateSchemaItems.D_DUPLICATES) {
//			throw new RuntimeException("Duplicate names are not supported for Xml --> Cobol");
//		}
////		String spaces = "                                                                                                  ";
//		String lastName = "", name;
//		int lvl = 0;
//		JsonToken lastType, type = JsonToken.VALUE_NULL;
//		JsonFactory jfactory = new JsonFactory();
//
//		/*** read from file ***/
//		JsonParser parser = jfactory.createParser(xmlStream);
//		//XMLStreamReader parser = XMLInputFactory.newInstance().createXMLStreamReader(xmlStream);
//		AbstractLine l = null;
//		AbstractLineWriter w = iob.newWriter(cobolStream);		
//		StringBuilder b = new StringBuilder();
//		Map<String, ? extends IItem> arrayItems = itemDtls.getArrayItems();
//		IntStack arrayDtls = new IntStack();
//		IntStack levelNames = new IntStack();
//		IGetRecordFieldByName fieldLookup = itemDtls.getFieldLookup();
//		Map<String, Integer> recordHierarchyMap = itemDtls.getRecordHierarchyMap();
//		int maxHierarchLvl = itemDtls.getMaxRecordHierarchyLevel();
//		String recordName = "";
//		Integer lookupLvl;
//		boolean lastWasArray = false;
//		
//
//		lastType = type;
//		type = parser.nextToken();
//		
//		switch (type) {
//		case START_ARRAY:
//			break;
//		case FIELD_NAME:
//			String n = parser.getText();
//			type = parser.nextToken();
//			
////			switch (type) {
////			case START_ARRAY:
////				break;
////			case START_OBJECT:
////				//TODO single Object
////				break;
////			default:
////				throw new RecordException("Invalid JSon, Expecting Array/Object and  not a " + type.name());
////			}
//			break;
//		default:
//			throw new RecordException("Invalid JSon, was not expecting a " + type.name() + " at the start");
//		}
////		while ((type = parser.nextToken()) != null) {
////			
////			switch (type) {
////			case START_OBJECT:
////            	lvl += 1;
////            	name = parser.getName().toString();
////            	if (lvl == 2 
////                || (     maxHierarchLvl >= lvl - 3
////                   &&	(lookupLvl = recordHierarchyMap.get(name.toUpperCase())) != null
////             	   &&    lookupLvl >= lvl - 3 
////             		)) {
////            		if (l != null) {
//////           			System.out.println();
//////            			System.out.println(l.getFullLine());
////            			w.write(l);
////            		}
////        			recordName = name;
////
////            		l = iob.newLine();
////            		if (schema.getRecordCount() > 1) {
////            			int recIdx = schema.getRecordIndex(name);
////            			if (recIdx >= 0) {
////            				l.setWriteLayout(recIdx);
////            			}
////            		}
//////            	} else if ((lookupLvl = recordHierarchyMap.get(name.toUpperCase())) != null) {
//////            		System.out.println("## " + name + " " + lookupLvl + " " + lvl );
//////            	} else if (lvl <= 4 && name.indexOf("Record") >= 0) {
//////            		System.out.println("** " + name + " " + lvl );
////            	}
//////            	System.out.println();
//////           	System.out.print(spaces.substring(spaces.length() - 2 * lvl +1) + parser.getName() + " >");
////            	String ucName;
////            	if (name != null && arrayItems.containsKey((ucName = name.toUpperCase()))) {
////            		if (name.equalsIgnoreCase(levelNames.getLastName())) {
////            			arrayDtls.inc();
////            		} else {
////            			arrayDtls.add(0, name, arrayItems.get(ucName));
////            		}
////            	}
////            	
////            	lastName = name;
////            	levelNames.add(0, name, null);
////            	b.setLength(0);
////            	
////            	
////            	break;
////            case END_OBJECT:
////            	String name2 = parser.getName().toString();
////    			int[] indexes = arrayDtls.toArray();
////            	
//////				System.out.print(b + "< " + name2 );
////				
////				if (lastName.equals(name2)) {
////	        		IFieldDetail f;
////	        		String n = lastName;
////	        		
////	        		f = fieldLookup.getField(recordName, n, indexes);
////	        		
////	        		if (f == null) {
////	        			if (b.length() > 0) {
////	        				f = fieldLookup.getField(recordName, n, indexes);
////	        				throw new RuntimeException("Field: " + n + " does not exist, can not assign '" + b.toString() + "'");
////	        			}
////	        		} else {
////		        		AbstractFieldValue fieldValue = l.getFieldValue(f);
////			        	if (lastType == XMLStreamConstants.START_ELEMENT) {
////							fieldValue.set(CommonBits.NULL_VALUE);
////			        	} else {
////			        		String txt = b.toString();
////			        		if (fieldValue.isNumeric()) {
////			        			txt = txt.trim();
////			        		}
////							fieldValue.set(txt);
////			        	}
////	        		}
////		        	b.setLength(0); 	
////				}
////				
////				if (lastWasArray) {
////					IItem item = arrayDtls.getLastItem();
////					if (item != null && item.arrayCheck != null && arrayDtls.size >= 0) {
////						item.arrayCheck.updateForCount(l, item, indexes, arrayDtls.stack[arrayDtls.size]+1);
////					}					
////				}
////				lastWasArray = false;
////				if (name2 != null && name2.equalsIgnoreCase(arrayDtls.getName())) {
////					arrayDtls.remove();
////					lastWasArray = true;
////				}
////				levelNames.remove();
////            	lvl -= 1;
////            	break;
////            case CHARACTERS:
////            	String text = parser.getText();
////            	
////            	b.append(text);
////            	
//// //           	System.out.print(text.trim());
////            	break;
//////            case (XMLStreamConstants.START_DOCUMENT) :
//////            break;
//////            case (XMLStreamConstants.COMMENT) :
//////            break;
//////            case (XMLStreamConstants.DTD) :
//////            	break;
//////            case (XMLStreamConstants.ENTITY_REFERENCE) :
//////            	break;
//////            case (XMLStreamConstants.CDATA) :
//////              break;
//////            case (XMLStreamConstants.END_DOCUMENT): 
//////            	break;
////          	default:
////			}
////			lastType = type;
////		}
//		
////		if (l != null) {
////			w.write(l);
////		}
//		parser.close();
//		xmlStream.close();
//		w.close();
//		cobolStream.close();
//	}
	
//	private void processJson(String recordType, JsonParser parser, JsonToken type) throws JsonParseException, IOException {
//		//TODO write
//		//TODO write
//		//TODO write
//		//TODO write
//		Map<String, ? extends IItem> arrayItems = itemDtls.getArrayItems();
//		IntStack arrayDtls = new IntStack();
//		IntStack levelNames = new IntStack();
//		IGetRecordFieldByName fieldLookup = itemDtls.getFieldLookup();
//		Map<String, Integer> recordHierarchyMap = itemDtls.getRecordHierarchyMap();
//		int maxHierarchLvl = itemDtls.getMaxRecordHierarchyLevel();
//		JsonToken lastType = type;
//		IntStack stack = new IntStack();
//		IntStack nameStack = new IntStack();
//		AbstractLine l = null; //iob.newLine();
//		String currentName, name = recordType;
//
//		
//		while ((type = parser.nextToken()) != null) {
//			currentName = "";
//			switch (type) {
//			case START_ARRAY:			
//				IItem item = null;
//				if (nameStack.size > 0) {
//					//fieldLookup.getField(recordType, name, arrayDtls.toArray());
//					item = arrayItems.get(name.toUpperCase());
//					arrayDtls.add(0);
//				}
//				nameStack.add(-1, name, item, type);
//				break;
//			case END_ARRAY:
//				nameStack.remove();
//				break;
//			case START_OBJECT:
//				if (parser.getCurrentName() == null) {
//					
//				} else {
//					
//				}
//				break;
//			case END_OBJECT:
//				break;
//			case FIELD_NAME:
//				currentName = parser.getText();
//				nameStack.set(currentName); 
//				break;
//			}
//			
//			name = currentName;
//		}
//	}
	
	/**
	 * Class to keep track of Cobol Group Levels
	 * (in particular Groups that are arrays) 
	 * @author Bruce Martin
	 *
	 */
	private static class IntStack {
		private int[] stack = new int[100];
		private String[] names = new String[100];
//		private IItem[] items = new IItem[100];
//		private JsonToken[] tokens = new JsonToken[100];
		private int size = 0;
		
		public IntStack add(int item) {
			stack[size++] = item;
			return this;
		}
		
//		public IntStack add(int pos, String name, IItem item, JsonToken token) {
//			stack[size] = pos;
//			items[size] = item;
//			tokens[size] = token;
//			names[size++] = name;
//			
//			return this;
//		}
		
		public IntStack set(int item) {
			stack[size - 1] = item;
			return this;
		}
//		public IntStack set(String name) {
//			stack[size - 1] = 0;
//			names[size - 1] = name;
//			return this;
//		}
//	
//		public IntStack inc() {
//			size += 1;
//			stack[size - 1] += 1;
//			return this;
//		}
//
//		public String getName() {
//			if (size <= 0) return "";
//			
//			return names[size-1];
//		}
//
//		public String getLastName() {
//			if (size < 0) return "";
//			
//			return names[size];
//		}
//
//		public IItem getLastItem() {
//			if (size < 0) return null;
//			return items[size];
//		}
//		
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

	
	public static ICobol2Json newCobol2Json(String cobolCopybook) {
		return new Cobol2JsonImp(cobolCopybook, new CobolCopybookLoader());
	}
	
	
	public static ICobol2Json newCobol2Json(InputStream cobolCopybook, String copybookName) {
		return new Cobol2JsonImp(cobolCopybook, copybookName, new CobolCopybookLoader());
	}
	
	public static ICobol2Json newCobol2Json(Reader cobolCopybookReader, String copybookName) {
		return new Cobol2JsonImp(cobolCopybookReader, copybookName, new CobolCopybookLoader());
	}

	
	public static Icb2xml2Json newCb2Xml2Json(String cobolCopybook) {
		return new Cobol2JsonImp(cobolCopybook, new XmlCopybookLoader());
	}
	
	
	public static Icb2xml2Json newCb2Xml2Json(InputStream cobolCopybook, String copybookName) {
		return new Cobol2JsonImp(cobolCopybook, copybookName, new XmlCopybookLoader());
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
