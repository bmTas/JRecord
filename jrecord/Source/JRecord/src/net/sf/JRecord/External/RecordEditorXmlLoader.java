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

package net.sf.JRecord.External;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.XmlConstants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.Def.AbstractConversion;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.External.base.ExternalConversion;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalGroupSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.IO.XmlLineReader;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Log.TextLog;

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
public class RecordEditorXmlLoader extends BaseCopybookLoader implements ICopybookLoaderStream {

	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.ICopybookLoaderStream#loadCopyBook(java.io.InputStream, java.lang.String, int, int, java.lang.String, int, int, int, net.sf.JRecord.Log.AbsSSLogger)
	 */
	@Override
	public ExternalRecord loadCopyBook(InputStream inputStream,
			String copyBookName, int splitCopybook, int dbIdx, String font,
			int copybookFormat, int binaryFormat, int systemId, AbsSSLogger log)
			throws IOException {
		return (new RecordEditorXmlLoaderImp()).loadCopyBook(inputStream, copyBookName, font, log);
	}
	
	public ExternalRecord loadCopyBook(InputStream is, String layoutName) throws IOException {
		return (new RecordEditorXmlLoaderImp()).loadCopyBook(is, layoutName, "", null); 
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.CopybookLoader#loadCopyBook(java.lang.String, int, int, java.lang.String, int, int, int, net.sf.JRecord.Log.AbsSSLogger)
	 */
	@Override
	public ExternalRecord loadCopyBook(String copyBookFile,
			int splitCopybookOption, int dbIdx, String font,
			int copybookFormat, int binFormat, int systemId, AbsSSLogger log)
			throws Exception {
		return (new RecordEditorXmlLoaderImp()).loadCopyBook(copyBookFile, splitCopybookOption, dbIdx, font, copybookFormat, binFormat, systemId, log);
	}

	
	
	@Override
	public ExternalRecord loadCopyBook(Reader reader, String copyBookName, int splitCopybook, int dbIdx, String font,
			int copybookFormat, int binaryFormat, int systemId, AbsSSLogger log) throws IOException {
		return (new RecordEditorXmlLoaderImp()).loadCopyBook(reader, copyBookName, font, null);
	}



	private static class RecordEditorXmlLoaderImp extends BaseCopybookLoader { 
		
		private String lastGroupDetails = null;
		
		/**
		 * Load the Copybook
		 * @see CopybookLoader#loadCopyBook(String, int, int, String, int, int, AbsSSLogger)
		 */
		@Override
		public ExternalRecord loadCopyBook(String copyBookFile,
			int splitCopybookOption, int dbIdx, String font, int copybookFormat, int binFormat,
			int systemId, AbsSSLogger log) throws Exception {
	
	        ExternalRecord rec = createXRecord(Conversion.getCopyBookId(copyBookFile), font);
	
	        XmlLineReader r = new XmlLineReader(true);
			r.open(copyBookFile);
			insertRecord(TextLog.getLog(log), null, rec, r, dbIdx);
	        r.close();
	
	        return rec;
		}
	

		public ExternalRecord loadCopyBook(InputStream is, String layoutName, String font, AbsSSLogger log) throws IOException {
	
		        ExternalRecord rec = createXRecord(layoutName, font);
	
		        XmlLineReader r = new XmlLineReader(true);
				r.open(is, (LayoutDetail) null);
				insertRecord(TextLog.getLog(log), null, rec, r, AbstractConversion.USE_DEFAULT_IDX);
		        r.close();
	
		        return rec;
		}


		private ExternalRecord createXRecord(String layoutName, String font) {
			int rt = Constants.rtRecordLayout;

			ExternalRecord rec = ExternalRecord.getNullRecord(
					layoutName,
			        rt,
			        font);
	        rec.setNew(true);

			return rec;
		}
	

		public ExternalRecord loadCopyBook(Reader reader, String layoutName, String font, AbsSSLogger log) throws IOException {
	
		        ExternalRecord rec = createXRecord(layoutName, font);
	
		        XmlLineReader r = new XmlLineReader(true);
				r.open(reader, (LayoutDetail) null);
				insertRecord(TextLog.getLog(log), null, rec, r, AbstractConversion.USE_DEFAULT_IDX);
		        r.close();
	
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
		private boolean insertRecord(AbsSSLogger log, ExternalRecord parentRec, ExternalRecord childRec,
				XmlLineReader reader, int dbIdx) throws IOException {
			AbstractLine line = reader.read();
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
					ExternalRecord newRec;
					if (childRec.getRecordType() != Constants.rtGroupOfBinaryRecords) {
						childRec.setRecordType(Constants.rtGroupOfRecords);
					}
		
					do {
						newRec = ExternalRecord.getNullRecord(
				        		"",
				        		Constants.rtRecordLayout,
				                childRec.getFontName());
						newRec.setNew(true);
						//rec.addRecord(newRec);
					} while (insertRecord(log, childRec, newRec, reader, dbIdx));
					childRec.setParentsFromName();
		
					line = reader.read();
				}
		
				line = addFieldTsts(childRec, reader, line, dbIdx);
				line = addFields(childRec, reader, line, dbIdx);
				line = addFieldTsts(childRec, reader, line, dbIdx);
			}
			return true;
		}
	
	
	
	
		private AbstractLine addFields(
				ExternalRecord childRec,
				XmlLineReader reader,
				AbstractLine line,
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
							s =line.getFieldValueIfExists(Constants.RE_XML_DEFAULT).asString();
							cobolName =line.getFieldValueIfExists(Constants.RE_XML_COBOLNAME).asString();
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
	
	
		private AbstractLine addFieldTsts(
				ExternalRecord childRec,
				XmlLineReader reader,
				AbstractLine line,
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
	
		private void setDefault(ExternalRecord childRec, String s) {
			if (s != null && "Y".equalsIgnoreCase(s)) {
				childRec.setDefaultRecord(true);
			}
		}
	
	
		private ExternalGroupSelection<ExternalSelection> getGroup(XmlLineReader reader, int type) throws IOException {
	
			ExternalGroupSelection<ExternalSelection> g = new ExternalGroupSelection<ExternalSelection>();
			String name;
	
			g.setType(type);
	
			AbstractLine line = reader.read();
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
		private int getIntFld(AbstractLine line, String fldName, int defaultValue)  {
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

	}
	public static ExternalRecord getExternalRecord(String xml, String name) throws Exception {
		ByteArrayInputStream bs = new ByteArrayInputStream(xml.getBytes());

		return (new RecordEditorXmlLoaderImp()).loadCopyBook(bs, name, "", null);
	}
	
}
