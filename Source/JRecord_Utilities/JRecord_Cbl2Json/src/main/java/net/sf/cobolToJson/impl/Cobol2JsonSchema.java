package net.sf.cobolToJson.impl;

import static java.util.stream.Collectors.*;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.ICopybookLoaderCobol;
import net.sf.JRecord.External.XmlCopybookLoader;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;
import net.sf.JRecord.schema.CobolSchemaDetails;
import net.sf.JRecord.schema.CobolSchemaReader;
import net.sf.JRecord.schema.ISchemaInformation;
import net.sf.JRecord.schema.jaxb.IItem;
import net.sf.JRecord.schema.jaxb.Item;


/**
 * Purpose: Convert Cobol-Data-Files <---> Xml files
 *  
 * @author Moddy Te'eni. Bruce Martin
 *
 */
public class Cobol2JsonSchema extends CobolSchemaReader<Cobol2JsonSchema>  {

	
	private ISchemaInformation itemDtls = null;
	
	private int numberToOutput = 10;
	
	private CobolSchemaDetails cobolSchemaDetails = null;

	private boolean prettyPrint = true;

	private Cobol2JsonSchema(String copybookFilename, ICopybookLoaderCobol loader) {
		super(Conversion.getCopyBookId(copybookFilename), loader);
		loader.setSaveCb2xmlDocument(true);
		super.addCopyBook(copybookFilename);
		//ioBuilder = new CblIOBuilderMultiSchema(copybookFilename, loader, ICopybookDialects.FMT_MAINFRAME);
	}

	private Cobol2JsonSchema(InputStream is, String copybookname, ICopybookLoaderCobol loader) {
		super(copybookname, loader);
		loader.setSaveCb2xmlDocument(true);
		
		super.addCopyBook(is, copybookname);
		//ioBuilder = new CblIOBuilderMultiSchema(is, copybookname, loader, ICopybookDialects.FMT_MAINFRAME);
	}

	private Cobol2JsonSchema(Reader copybookReader, String copybookname, ICopybookLoaderCobol loader) {
		super(copybookname, loader);
		loader.setSaveCb2xmlDocument(true);
		
		super.addCopyBook(copybookReader, copybookname);
	}

	
	public void cobol2jsonSchema(String outputFileName) throws IOException  {
		cobol2jsonSchema(new BufferedOutputStream(new FileOutputStream(outputFileName), 0x4000));
	}

	
	public void cobol2jsonSchema(OutputStream jsonStream) throws IOException {
		doInit();
		
		ISchemaIOBuilder iob = cobolSchemaDetails.ioBuilder;
        LayoutDetail schema =  iob.getLayout();
       	JsonGenerator writer = new JsonFactory().createGenerator(jsonStream);
//        List<Item> items = cobolSchemaDetails.recordItems.stream().flatMap( ri -> ri.items.stream()).collect(toList());
        List<Item> items = new ArrayList<Item>();// = cobolSchemaDetails.recordItems.stream().flatMap( ri -> ri.items.stream()).collect(toList());
        List<Object> oList = cobolSchemaDetails.recordItems.stream().flatMap( ri -> ri.items.stream()).collect(toList());
        for ( Object o : oList) {
        	items.add((Item) o);
        }

        if (prettyPrint) {
        	writer.setPrettyPrinter(new DefaultPrettyPrinter());
        }
        
 		try {
 			IItem item;
 			writer.writeStartObject();
 			if (items.size() == 1 
 					&& (item = items.get(0)).getNameToUse() != null
 					&& (item.getOccurs() == null )) {
 				writer.writeArrayFieldStart(cobolSchemaDetails.copybookInformation.updateName(item.getName()));
 				if (item.getItemType() == IItem.TYPE_GROUP) {
 					writer.writeStartObject();
 					writeItems(writer, item.getChildItems(), new IntStack());
 					writer.writeEndObject();
 				} else
 					writePrimitive(writer, item);

			} else if (schema.getRecordCount() == 1) {
				writer.writeArrayFieldStart(itemDtls.updateName(schema.getRecord(0).getRecordName()));
		    	writer.writeStartObject();
		    	writeItems(writer, items, new IntStack());
		        writer.writeEndObject();
			} else if (schema.hasTreeStructure()) {
				String rootRecordName = super.getRootRecord();
				if (rootRecordName == null) {
					writer.writeArrayFieldStart(cobolSchemaDetails.copybookInformation.updateName(cobolSchemaDetails.schema.getLayoutName()));
					writer.writeStartObject();
					writeItemInTree(writer, items);
					writer.writeEndObject();
				} else {
					int rootIdx = schema.getRecordIndex(rootRecordName);
					if (rootIdx < 0) {
						throw new RecordException("Root Record: " + rootRecordName + " does not exist" );
					}
					RecordDetail rec = schema.getRecord(rootIdx);
					if (rec.getParentRecordIndex() >= 0) {new RecordException("Root Record: " + rootRecordName + " has a parent record ???" ); }
					for (int i =0; i < schema.getRecordCount(); i++) {
						if (i != rootIdx && schema.getRecord(i).getParentRecordIndex() <0) {
							throw new RecordException("Schema has more than one root: " + schema.getRecord(i).getRecordName());
						}
					}
					writer.writeArrayFieldStart(itemDtls.updateName(schema.getRecord(rootIdx).getRecordName()));
					writer.writeStartObject();
					writeTreeItemRecord(writer, items, rootIdx, items.get(rootIdx));
					writer.writeEndObject();
				}		
			} else {
				writer.writeArrayFieldStart(cobolSchemaDetails.copybookInformation.updateName(cobolSchemaDetails.schema.getLayoutName()));
				for(int i=0; i< items.size(); ++i) {
					writer.writeStartObject();
					writeItem(writer, items.get(i), new IntStack());
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
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
 		
	}
	
	private void doInit() throws IOException {
		cobolSchemaDetails = super.getCobolSchemaDetails();

		itemDtls = cobolSchemaDetails.copybookInformation;
	}

	/**
	 * @param prettyPrint the prettyPrint to set
	 */
	public final void setPrettyPrint(boolean prettyPrint) {
		this.prettyPrint = prettyPrint;
	}

	private void writeItemInTree(JsonGenerator writer, List<? extends IItem> items) 
	throws IOException {


		for (int i = 0; i < items.size(); i++) {
			IItem item = items.get(i);
			writer.writeObjectFieldStart(item.getNameToUse());
			writeTreeItemRecord(writer, items, i, item);
		}
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
	public void writeTreeItemRecord(JsonGenerator writer,
			List<? extends IItem> items, int recIdx, IItem item) throws IOException {
		
		if (item.getItemType() == IItem.TYPE_GROUP) {
			writeAnItem(writer, null, item, new IntStack());
			readChildren(writer, items, recIdx);
		} else {
			writer.writeStartObject();
			writeAnItem(writer, null, item, new IntStack());
			readChildren(writer, items, recIdx);
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
	public void readChildren(JsonGenerator writer, List<? extends IItem> items, int recIdx) throws IOException {
		IItem item = items.get(recIdx);
		writer.writeArrayFieldStart(item.getNameToUse());

		writer.writeStartObject();
		writeTreeItemRecord(writer, items, recIdx, item);
		writer.writeEndObject();
		writer.writeEndArray();
	}
	
	private void writeItem(JsonGenerator writer, IItem item, IntStack indexs) throws IOException {

		String name = item.getName();
		if (name == null || item.getName().length() == 0 || "filler".equalsIgnoreCase(item.getName())) {
			if (item.getItemType() == IItem.TYPE_GROUP) {
				if (item.getOccurs() != null && item.getOccurs() > 1) {
					writeArray(writer, item, "filler", indexs);
				} else {
					writeItems(writer, item.getChildItems(), indexs);
				}
			}
		} else if (item.getOccurs() != null && item.getOccurs() > 1) {
			writeArray(writer, item, item.getNameToUse(), indexs);
		} else {
			writeAnItem(writer, item.getNameToUse(), item, indexs);
		}
	}

	/**
	 * @param writer
	 * @param l
	 * @param item
	 * @param indexs
	 * @throws IOException 
	 */
	private void writeAnItem(JsonGenerator writer, String fieldname, IItem item,
			IntStack indexs) throws IOException {
		if (item.getItemType() == IItem.TYPE_GROUP) {
			if (fieldname == null) {
				writeItems(writer, item.getChildItems(), indexs);
			} else {
				writer.writeObjectFieldStart(fieldname);
				writeItems(writer, item.getChildItems(), indexs);
				writer.writeEndObject();
			}
		} else if (indexs.size == 0) {
			//writer.writeCharacters(l.getFieldValue(item.fieldDef).asString());
			writeField(writer, fieldname, item);
		} else {
			//writer.writeCharacters(l.getFieldValue(item.arrayDef.getField(indexs.toArray())).asString());
			writeField(writer, fieldname, item);
		}
	}

	/**
	 * @param writer
	 * @param l
	 * @param item
	 * @param indexs
	 * @throws IOException 
	 */
	private void writeArray(JsonGenerator writer, IItem item, String name,
			IntStack indexs) throws IOException {

		indexs.add(0);
		int[] indexArray = indexs.toArray();

		boolean writeArrayStart = true;
		if (item.getArrayValidation() == null ) { 
			if (writeArrayStart) {
				writer.writeArrayFieldStart(name);
				writeArrayStart = false;
			}
			if (item.getItemType() == IItem.TYPE_GROUP) {
				writer.writeStartObject();
				writeItems(writer, item.getChildItems(), indexs.set(0));
				writer.writeEndObject();
			} else {
				indexArray[indexArray.length - 1] = 0;
				writePrimitive(writer, item);
			}
		}
		if (! writeArrayStart) {
			writer.writeEndArray();
		}
		indexs.remove();
	}

	private void writeItems(JsonGenerator writer, List<? extends IItem> items, IntStack indexs) throws IOException {
		for (IItem item : items) {
			writeItem(writer, item, indexs);
		}
	}
	
	private void writeField(JsonGenerator writer, String fieldName, IItem item) throws IOException {
		numberToOutput++;
		if (item.isNumeric()) {
			if (item.getScale() > 0)
				writer.writeNumberField(fieldName, numberToOutput+ 0.5);
			else
				writer.writeNumberField(fieldName, numberToOutput);
				
		} else {
			writer.writeStringField(fieldName, createStringValue(item));
		}
	}

	
	private void writePrimitive(JsonGenerator writer, IItem item) throws IOException {
		if (item.isNumeric())
			writer.writeNumber( numberToOutput);
		else
			writer.writeString(createStringValue(item));
	}
	
	private String createStringValue(IItem item) {
		final String string = ""+(numberToOutput++);
		char ch = (char) (64 + (numberToOutput % 27));
		final int displayLength = item.getDisplayLength();
		if (string.length() >= displayLength)
			return ch+ string.substring(0, displayLength-1);
		else {
			int len = displayLength- string.length();
			//String buf = StringUtils.join(Collections.nCopies(len, ch), "");
			char[] fill = new char[len];
			Arrays.fill(fill, ch);
			return (new StringBuilder()).append(fill).append(string).toString();
		}
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
		private int size = 0;
		
		public IntStack add(int item) {
			stack[size++] = item;
			return this;
		}
		
		public IntStack set(int item) {
			stack[size - 1] = item;
			return this;
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

	
	public static Cobol2JsonSchema newCobol2Json(String cobolCopybook) {
		return new Cobol2JsonSchema(cobolCopybook, new CobolCopybookLoader());
	}
	
	
	public static Cobol2JsonSchema newCobol2Json(InputStream cobolCopybook, String copybookName) {
		return new Cobol2JsonSchema(cobolCopybook, copybookName, new CobolCopybookLoader());
	}
	
	public static Cobol2JsonSchema newCobol2Json(Reader cobolCopybookReader, String copybookName) {
		return new Cobol2JsonSchema(cobolCopybookReader, copybookName, new CobolCopybookLoader());
	}

	
	public static Cobol2JsonSchema newCb2Xml2Json(String cobolCopybook) {
		return new Cobol2JsonSchema(cobolCopybook, new XmlCopybookLoader());
	}
	
	public static Cobol2JsonSchema newCb2Xml2Json(InputStream cobolCopybook, String copybookName) {
		return new Cobol2JsonSchema(cobolCopybook, copybookName, new XmlCopybookLoader());
	}

}
