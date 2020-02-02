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


import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.ILineFieldNames;
import net.sf.JRecord.Common.XmlConstants;

import net.sf.JRecord.External.Def.AbstractConversion;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.External.base.ExternalConversion;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalGroupSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.cb2xml.analysis.BaseItem;
import net.sf.cb2xml.analysis.Item;
import net.sf.cb2xml.analysis.ItemBuilder;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.IItem;

/**
 * Class to Load a RecordLayout (Record or Line Description)
 * from an XML file (RecordEditor-XML)
 *
 * <pre>
 *   <b>Usage:</b>
 *        CopybookLoader loader = new RecordEditorXmlLoader();
 *        LayoutDetail layout = loader.loadCopyBook(copybookName, 0, 0, "", 0, 0, null).asLayoutDetail();
 * </pre>
 *
 * @author Bruce Martin
 *
 */
public class BaseRecordEditorXmlLoader<XRecord extends BaseExternalRecord<XRecord>> {
		
		private static final String END_ITEMS = "/" + Constants.RE_XML_COBOL_ITEMS;
		private String lastGroupDetails = null;
		
		private final ILineReader xmlReader;
	    private final IExernalRecordBuilder<XRecord> recBuilder;

		public BaseRecordEditorXmlLoader(ILineReader xmlReader, IExernalRecordBuilder<XRecord> recBuilder) {
			super();
			this.xmlReader = xmlReader;
			this.recBuilder = recBuilder;
		}


		/**
		 * Load the Copybook
		 * @see CopybookLoader#loadCopyBook(String, int, int, String, int, int, AbsSSLogger)
		 */
		public XRecord loadCopyBook(String copyBookFile,
			int splitCopybookOption, int dbIdx, String font, int copybookFormat, int binFormat,
			int systemId, AbsSSLogger log) throws Exception {
	
	        XRecord rec = createXRecord(Conversion.getCopyBookId(copyBookFile), font);
	
			insertRecord(TextLog.getLog(log), null, rec, xmlReader, dbIdx);
	        xmlReader.close();
	
	        return rec;
		}
	

		public XRecord loadCopyBook(String layoutName, String font, AbsSSLogger log) throws IOException {
	
		        XRecord rec = createXRecord(layoutName, font);
	
				insertRecord(TextLog.getLog(log), null, rec, xmlReader, AbstractConversion.USE_DEFAULT_IDX);
		        xmlReader.close();
	
		        return rec;
		}


		private XRecord createXRecord(String layoutName, String font) {
			int rt = Constants.rtRecordLayout;

			XRecord rec = recBuilder.getNullRecord(
					layoutName,
			        rt,
			        font);
	        rec.setNew(true);

			return rec;
		}
	

	

	
		/**
		 * Add  an External-Record (Sub-Record) to an External-Record
		 * @param log log to write any errors that occur
		 * @param parentRec parent Record
		 * @param childRec Child Record
		 * @param reader line reader to read record Details from.
		 * @param dbIdx Database index
		 *
		 * @return wether Record was inserted or not
		 * @throws IOException 
		 *
		 * @throws Exception any error that occurs
		 */
		@SuppressWarnings("deprecation")
		private boolean insertRecord(AbsSSLogger log, XRecord parentRec, XRecord childRec,
				ILineReader reader, int dbIdx) throws IOException {
			ILineFieldNames line = reader.read();
			String name, s;
			log = TextLog.getLog(log);
	
			if (line == null || (name= line.getFieldValue(XmlConstants.XML_NAME).asString()).startsWith("/")) {
				//System.out.println("Exit Found " + name);
				return false;
			}
	
			if (XmlConstants.XML_START_DOCUMENT.equalsIgnoreCase(name)) {
				line = reader.read();
				name = line.getFieldValue(XmlConstants.XML_NAME).asString();
			}
	
			if (Constants.RE_XML_RECORD.equalsIgnoreCase(name)) {
				childRec.setRecordName(line.getFieldValue(Constants.RE_XML_RECORDNAME).asString());
				childRec.setCopyBook(line.getFieldValueIfExists(Constants.RE_XML_COPYBOOK).asString());
				childRec.setDelimiter(line.getFieldValueIfExists(Constants.RE_XML_DELIMITER).asString());
				childRec.setDescription(line.getFieldValueIfExists(Constants.RE_XML_DESCRIPTION).asString());
				childRec.setFileStructure(
						ExternalConversion.getFileStructure(
								dbIdx, line.getFieldValue(Constants.RE_XML_FILESTRUCTURE).asString()));
				
				Object o = line.getField(Constants.RE_XML_FONTNAME);
				if ( o != null ) {
					childRec.setFontName(o.toString());
				}
				childRec.setListChar(line.getFieldValueIfExists(Constants.RE_XML_LISTCHAR).asString());
				childRec.setParentName(line.getFieldValueIfExists(Constants.RE_XML_PARENT).asString());
				childRec.setQuote(line.getFieldValueIfExists(Constants.RE_XML_QUOTE).asString());
				childRec.setRecordStyle(
						ExternalConversion.getRecordStyle(
								dbIdx, line.getFieldValueIfExists(Constants.RE_XML_STYLE).asString()));
				childRec.setRecordType(
						ExternalConversion.getRecordType(
								dbIdx, line.getFieldValueIfExists(Constants.RE_XML_RECORDTYPE).asString()));
				childRec.setEmbeddedCr(
						"Y".equalsIgnoreCase(
								line.getFieldValueIfExists(Constants.RE_XML_EMBEDDED_CR).asString()));
				childRec.setInitToSpaces(
						"Y".equalsIgnoreCase(
								line.getFieldValueIfExists(Constants.RE_XML_INIT_SPACES).asString()));
				
				String formatParam = line.getFieldValueIfExists(Constants.RE_XML_STYLE_PARAM).asString();
				
				if (formatParam.length() > 0) {
					childRec.setLineFormatParam(formatParam);
				}
				
				s = line.getFieldValueIfExists(Constants.RE_XML_RECORDSEP).asString();
				if (s != null && ! "".equals(s)) {
					childRec.setRecSepList(s);
				}
				s = line.getFieldValueIfExists(Constants.RE_XML_SYSTEMNAME).asString();
				if (s != null && ! "".equals(s)) {
					childRec.setSystemName(s);
				}
				
				s = line.getFieldValueIfExists(Constants.RE_XML_RECORDLENTH).asString();
				if (s != null && ! "".equals(s)) {
					try {
						childRec.setRecordLength(Integer.parseInt(s));
					} catch (NumberFormatException e) {
						e.printStackTrace();
					}
				}
				//rec.setSystem((int) line.getFieldValue(Constants.RE_XML_COPYBOOK).asLong());
	
	
				ExternalFieldSelection fs = new ExternalFieldSelection();
				fs.setFieldName(line.getFieldValueIfExists(Constants.RE_XML_TESTFIELD).asString());
				fs.setFieldValue(line.getFieldValueIfExists(Constants.RE_XML_TESTVALUE).asString());
	
				if ("".equals(fs.getFieldName())) {
					if ("*".equals(fs.getFieldValue())) {
						childRec.setDefaultRecord(true);
					}
				} else {
					childRec.setRecordSelection(fs);
				}
	
				try {
					AbstractFieldValue val = line.getFieldValueIfExists(Constants.RE_XML_LINE_NO_FIELD_NAME);
					if (parentRec == null && val != null && ! "".equals(val.asString())) {
						childRec.setLineNumberOfFieldNames(val.asInt());
					}
				} catch (Exception e) {
				}
	
				if (parentRec != null) {
					parentRec.addRecord(childRec);
				}
			} else {
				s = "Error loading copybook; Expected " + Constants.RE_XML_RECORD + " Tag, but got " + name;
				log.logMsg(AbsSSLogger.ERROR, s);
				throw new RuntimeException(s);
			}
	
			line = reader.read();
	
			if (line != null) {
				if (Constants.RE_XML_RECORDS.equalsIgnoreCase(line.getFieldValue(XmlConstants.XML_NAME).asString())) {
					XRecord newRec;
					if (childRec.getRecordType() != Constants.rtGroupOfBinaryRecords 
					&& childRec.getRecordType() != Constants.rtBinaryRecord) {
						childRec.setRecordType(Constants.rtGroupOfRecords);
					}
		
					do {
						newRec = recBuilder.getNullRecord(
				        		"",
				        		Constants.rtRecordLayout,
				                childRec.getFontName());
						newRec.setNew(true);
						//rec.addRecord(newRec);
					} while (insertRecord(log, childRec, newRec, reader, dbIdx));
					childRec.setParentsFromName();
		
					line = reader.read();
				}
		
				line = addItems(childRec, reader, line, dbIdx);
				
				if (line != null) {
					if (END_ITEMS.equals(line.getFieldValue(XmlConstants.XML_NAME).asString())) {
						line = reader.read();
					} else { 	
						line = addFieldTsts(childRec, reader, line, dbIdx);
						line = addFields(childRec, reader, line, dbIdx);
						line = addFieldTsts(childRec, reader, line, dbIdx);
					}
				}
			}
			return true;
		}
	
	
		private ILineFieldNames addItems(
				XRecord childRec,
				ILineReader reader,
				ILineFieldNames line,
				int dbIdx)
		throws IOException {
			
			if (line != null
			&&  Constants.RE_XML_COBOL_ITEMS.equalsIgnoreCase(line.getFieldValue(XmlConstants.XML_NAME).asString())) {
				String endItem  = "/" + Cb2xmlConstants.ITEM;
				String endCondition = "/" + Cb2xmlConstants.CONDITION;
				ItemBuilder bldr = new ItemBuilder();
				ItemHelper itmReader = new ItemHelper(reader, new Item(null, 0, "00", ""));
				itmReader.line = line;
				
				String copybookPref = itmReader.getAttr(Constants.RE_XML_COPYBOOK_PREF);
				int dialect = ICopybookDialects.FMT_MAINFRAME;
				try { dialect = Integer.parseInt(itmReader.getAttr(Constants.RE_XML_COBOL_DIALECT)); } catch (Exception e) { }
				String[] groupNames = null;
				String groupNameStr = itmReader.getAttr(Constants.RE_XML_GROUP_NAMES);
				if (groupNameStr != null && groupNameStr.length() > 0) {
					groupNames = groupNameStr.split("\\.");
				}
				boolean keepFillers = Constants.RE_XML_TRUE.equals(itmReader.getAttr(Constants.RE_XML_KEEP_FILLER));
				boolean dropCopybookName = Constants.RE_XML_TRUE.equals(itmReader.getAttr(Constants.RE_XML_DROP_COPYBOOK_FROM_FIELD));
				boolean useJRecordNaming = Constants.RE_XML_TRUE.equals(itmReader.getAttr(Constants.RE_XML_JRECORD_NAMING));
				
				while ((line = itmReader.read()) != null) {
					String recType = line.getFieldValue(XmlConstants.XML_NAME).asString();
					if (Cb2xmlConstants.ITEM.equalsIgnoreCase(recType)) {
						processItem(bldr, itmReader);			
					} else if (Cb2xmlConstants.CONDITION.equalsIgnoreCase(recType)) {
	
					} else if (endItem.equalsIgnoreCase(recType)) {
						itmReader.pop();
					} else if (endCondition.equalsIgnoreCase(recType)) {
						
					} else if (END_ITEMS.equalsIgnoreCase(recType)) {
						break;
					}
				}
				List<? extends Item> childItems = itmReader.topItem.getChildItems();
				childRec.setCobolConversionOptions(keepFillers, dropCopybookName, useJRecordNaming);
				childRec.setItems(copybookPref, groupNames, dialect, childItems);
				childRec.updateTypeOnCobolItems();
			}
			
			return line;
		}
		
		
		
		private void processItem(ItemBuilder itmBldr, ItemHelper rBldr) {
			
			itmBldr.setLevelString(rBldr.getAttr(Cb2xmlConstants.LEVEL));
			itmBldr.setFieldName(rBldr.getAttr(Cb2xmlConstants.NAME));
			itmBldr.setBlankWhenZero(rBldr.getBooleanAttr(Cb2xmlConstants.BLANK_WHEN_ZERO));
			itmBldr.setDependingOn(rBldr.getAttr(Cb2xmlConstants.DEPENDING_ON));
			itmBldr.setDisplayLength(rBldr.getIntAttr(Cb2xmlConstants.DISPLAY_LENGTH));
			itmBldr.setFieldRedefined(rBldr.getBooleanAttr(Cb2xmlConstants.REDEFINED));
			itmBldr.setInheritedUsage(rBldr.getBooleanAttr(Cb2xmlConstants.INHERITED_USAGE));
			itmBldr.setJustified(rBldr.getJustified());
			itmBldr.setNumericClass(rBldr.getNumericClass());
			itmBldr.setOccursMin(rBldr.getIntAttr(Cb2xmlConstants.OCCURS_MIN));
			itmBldr.setOccurs(rBldr.getIntAttr(Cb2xmlConstants.OCCURS));
			itmBldr.setPicture(rBldr.getAttr(Cb2xmlConstants.PICTURE));
			itmBldr.setPosition(rBldr.getIntAttr(Cb2xmlConstants.POSITION));
			itmBldr.setRedefines(rBldr.getAttr(Cb2xmlConstants.REDEFINES));
			itmBldr.setScale(rBldr.getIntAttr(Cb2xmlConstants.SCALE));
			itmBldr.setSignClause(rBldr.getSignClause());
			itmBldr.setSigned(rBldr.getBooleanAttr(Cb2xmlConstants.SIGNED));
			itmBldr.setStorageLength(rBldr.getIntAttr(Cb2xmlConstants.STORAGE_LENGTH));
			itmBldr.setSync(rBldr.getBooleanAttr(Cb2xmlConstants.SYNC));
			itmBldr.setUsage(rBldr.getUsage());
			itmBldr.setValue(rBldr.getAttr(Cb2xmlConstants.VALUE));
			
			int size = rBldr.list.size();
			Item parent = size == 0 ? null : rBldr.list.get(size - 1);
			Item newItem = itmBldr.build(parent);
			
			if (! "True".equalsIgnoreCase(rBldr.getAttr(XmlConstants.END_ELEMENT))) {
				rBldr.add(newItem);
			}
		}

		
		private ILineFieldNames addFields(
				XRecord childRec,
				ILineReader reader,
				ILineFieldNames line,
				int dbIdx)
		throws IOException {
	
			if (Constants.RE_XML_FIELDS.equalsIgnoreCase(line.getFieldValue(XmlConstants.XML_NAME).asString())) {
				ExternalField fld;
				String fldName, s, cobolName;
				int decimal, len;
				line = reader.read();
	
				while (line != null && ! line.getFieldValue(XmlConstants.XML_NAME).asString().startsWith("/")) {
					s = null;
					cobolName = "";
					try {
						decimal = getIntFld(line, Constants.RE_XML_DECIMAL, 0);
						len     = getIntFld(line, Constants.RE_XML_LENGTH, Constants.NULL_INTEGER);
						fldName = line.getFieldValue(Constants.RE_XML_NAME).asString();
						try {
							s = line.getFieldValueIfExists(Constants.RE_XML_DEFAULT).asString();
							cobolName = line.getFieldValueIfExists(Constants.RE_XML_COBOLNAME).asString();
						} catch (Exception e) {}
						fld = new ExternalField(
							getIntFld(line, Constants.RE_XML_POS, Constants.NULL_INTEGER),
							len,
							fldName,
							line.getFieldValueIfExists(Constants.RE_XML_DESCRIPTION).asString(),
							ExternalConversion.getType(dbIdx,
									line.getFieldValueIfExists(Constants.RE_XML_TYPE).asString()),
							decimal,
			                ExternalConversion.getFormat(dbIdx,
									line.getFieldValueIfExists(Constants.RE_XML_CELLFORMAT).asString()),
							line.getFieldValueIfExists(Constants.RE_XML_PARAMETER).asString(),
							line.getFieldValueIfExists(Constants.RE_XML_DEFAULT).asString(),
			                cobolName,
			                0
						);
	
						if (! "".equals(s)) {
							fld.setDefault(s);
						}
						
						String groupDetails = line.getFieldValueIfExists(Constants.RE_XML_GROUP_NAMES).asString();
						if (! "".equals(groupDetails)) {
							fld.setGroup(groupDetails);
							lastGroupDetails = groupDetails;
						} else if (lastGroupDetails != null) {
							fld.setGroup(lastGroupDetails);
						}
						childRec.addRecordField(fld);
					} catch (Exception e) {
						e.printStackTrace();
					}
	
					line = reader.read();
				}
				line = reader.read();
			}
			return line;
		}
	
	
		private ILineFieldNames addFieldTsts(
				XRecord childRec,
				ILineReader reader,
				ILineFieldNames line,
				int dbIdx)
		throws IOException {
			String name = line.getFieldValue(XmlConstants.XML_NAME).asString();
			if (Constants.RE_XML_TST_FIELDS.equalsIgnoreCase(name)
			||  Constants.RE_XML_AND_FIELDS.equalsIgnoreCase(name)) {
				setDefault(childRec, line.getFieldValueIfExists(Constants.RE_XML_DEFAULTREC).asString());
				childRec.setRecordSelection(getGroup(reader, ExternalSelection.TYPE_AND));
	
				line = reader.read();
			} else if (Constants.RE_XML_OR_FIELDS.equalsIgnoreCase(name)) {
				setDefault(childRec, line.getFieldValueIfExists(Constants.RE_XML_DEFAULTREC).asString());
				childRec.setRecordSelection(getGroup(reader, ExternalSelection.TYPE_OR));
	
				line = reader.read();
			}
			return line;
		}
	
		private void setDefault(XRecord childRec, String s) {
			if (s != null && "Y".equalsIgnoreCase(s)) {
				childRec.setDefaultRecord(true);
			}
		}
	
	
		private ExternalGroupSelection<ExternalSelection> getGroup(ILineReader reader, int type) throws IOException {
	
			ExternalGroupSelection<ExternalSelection> g = new ExternalGroupSelection<ExternalSelection>();
			String name;
	
			g.setType(type);
	
			ILineFieldNames line = reader.read();
			while (line != null && ! line.getFieldValueIfExists(XmlConstants.XML_NAME).asString().startsWith("/")) {
				name = line.getFieldValueIfExists(XmlConstants.XML_NAME).asString();
				try {
					if (Constants.RE_XML_TST_FIELD.equalsIgnoreCase(name)) {
						g.add(new ExternalFieldSelection(
							line.getFieldValueIfExists(Constants.RE_XML_NAME).asString(),
							line.getFieldValueIfExists(Constants.RE_XML_VALUE).asString(),
							line.getFieldValueIfExists(Constants.RE_XML_OPERATOR).asString()));
					} else if (Constants.RE_XML_AND_FIELDS.equalsIgnoreCase(name)) {
						g.add(getGroup(reader, ExternalSelection.TYPE_AND));
					} else if (Constants.RE_XML_OR_FIELDS.equalsIgnoreCase(name)) {
						g.add(getGroup(reader, ExternalSelection.TYPE_OR));
					}
				} catch (Exception e) { }
	
				line = reader.read();
			}
	
			return g;
	
		}
	
//		/**
//		 * Get integer field from the line
//		 * @param line line to access
//		 * @param fldName field name required
//		 * @return requested field
//		 * @throws Exception any error
//		 */
//		private int getIntFld(AbstractLine line, String fldName) throws Exception {
//			try {
//				return line.getFieldValueIfExists(fldName).asInt();
//			} catch (Exception e) {
//				System.out.println("Error getting field " + fldName + ": " + e.getMessage() );
//				throw(e);
//			}
//		}
	
	
		/**
		 * Get integer field from the line
		 * @param line line to access
		 * @param fldName field name required
		 * @return requested field
		 * @throws Exception any error
		 */
		private int getIntFld(ILineFieldNames line, String fldName, int defaultValue)  {
			int ret = defaultValue;
			AbstractFieldValue value = line.getFieldValueIfExists(fldName);
			if (value != null && !"".equals(value.asString())) {
				try {
					ret = value.asInt();
				} catch (Exception e) {
				}
			}
			return ret;
		}


	public static interface ILineReader {
		public ILineFieldNames read() throws IOException;
		public void close() throws IOException;
	}
	
	/**
	 * 
	 * @author bruce
	 *
	 */
	private static class ItemHelper {
		final ILineReader reader; 
		//final ItemBuilder itmBldr;
		
		ILineFieldNames line;
		public final BaseItem topItem;
		public final ArrayList<Item> list = new ArrayList<Item>();
		
		public ItemHelper(ILineReader reader, Item topItem) {
			super();
			this.reader = reader;
			this.topItem = topItem;
			add((Item) topItem);
			
		}
		
		public ILineFieldNames read() throws IOException {
			return (line = reader.read());
		}
		
		public String getAttr(String localName) {
			@SuppressWarnings("deprecation")
			Object o = line.getField(localName);
			
			return o == null ? "" : o.toString();
		}
		
		public int getIntAttr(String localName) {
			String s = getAttr(localName);
			if (s != null && s.length() > 0) {
				return Integer.parseInt(s);
			}
			return IItem.NULL_INT_VALUE;
		}
		
		public boolean getBooleanAttr(String localName) {
			String s = getAttr(localName);
			return Cb2xmlConstants.TRUE.equals(s);
		}
		
		public Cb2xmlConstants.Justified getJustified() {
			String s = getAttr(Cb2xmlConstants.JUSTIFIED);
			return Cb2xmlConstants.toJustified(s);
		}
		
		public Cb2xmlConstants.Usage getUsage() {
			String s = getAttr(Cb2xmlConstants.USAGE);
			return Cb2xmlConstants.toUsage(s);
		}
		
		public Cb2xmlConstants.SignClause getSignClause() {
			String s = getAttr(Cb2xmlConstants.SIGN_CLAUSE);
			return Cb2xmlConstants.toSignClause(s);
		}
		
		public Cb2xmlConstants.NumericClass getNumericClass() {
			String s = getAttr(Cb2xmlConstants.NUMERIC);
			return Cb2xmlConstants.toNumeric(s);
		}

		public void add(Item itm) {
			list.add(itm);
		}

	
		public void pop() {
			list.remove(list.size() - 1);
		}

	}

}
