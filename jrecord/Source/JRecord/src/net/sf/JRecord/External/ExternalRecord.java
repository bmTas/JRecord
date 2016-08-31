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

import java.util.Collections;
import java.util.Comparator;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDecider;
import net.sf.JRecord.Extern.BaseExternalRecord;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.IO.builders.SchemaIOBuilder;


/**
 *  This class holds the interchange format of of a RecordLayout.
 *  It can be<ul>
 *    <li>Read from a file (See <b>CopybookLoaderFactory</b>).
 *    <li>Written to an external file (See <b>CopybookWriterManager</b>).
 *    <li>Converted to the internal format (<b>LayoutDetail</b>). See the method <b>asLayoutDetail</b>.
 *    <li>Read from RecordEditor's DB's with SQL like:
 *      <pre>
 *       Select
 *              RecordId,
 *              RecordName,
 *              Description,
 *              RecordType,
 *              System,
 *              ListChar,
 *              CopyBook,
 *              Delimiter,
 *              Quote,
 *              PosRecInd,
 *              RecSepList,
 *              RecordSep,
 *              Canonical_Name,
 *              Record_Style recordStyle,
 *              File_Structure fileStructure
 *       From Tbl_R_Records
 *
 *     </pre>
 *  </ul>
 *
 * This class also provides both specific field access methods
 * and Generic (based on Field number) access
 *
 * <pre>
 * Example:
 *       CopybookLoader loader = CopybookLoaderFactory.getInstance()
 *                                    .getLoader(CopybookLoaderFactory.RECORD_EDITOR_XML_LOADER);
 *       ExternalRecord externalLayout = loader.loadCopyBook(copybookName, 0, 0, "", 0, 0, null);
 *       LayoutDetail layout = externalLayout.asLayoutDetail();
 * </pre>
 */
public class ExternalRecord extends BaseExternalRecord<ExternalRecord> 
implements ICsvSchemaBuilder, IFixedWidthSchemaBuilder {

	//  private int recordId;
	//  private String recordName;
	//  private String description;
	//  private int recordType;
	//  private int system;
	//  private String listChar;
	//  private String copyBook;
	//  private String delimiter;
	//  private String quote;
	//  private int posRecInd;
	//  private String recSepList;
	//  private byte[] recordSep;
	//  private String fontName;
	//  private int recordStyle;
	
	//  private int fileStructure;
	
	public static enum FieldAdjustmentOptions {
			NO_ADJUSTMENT,
			ADJUST_BY_LENGTH,
			CALCULATE_ADJUSTMENT
	};

	private RecordDecider recordDecider = null;
	//private String tstFieldValue = "";





	//  private ArrayList<ExternalRecord> subRecords = new ArrayList<ExternalRecord>();
	//  private ArrayList<ExternalField> fields = new ArrayList<ExternalField>();
	//
	private int lastPosition = -1;
	//  
	//  private ArrayList<Cb2xmlDocument> cb2xmlDocuments = new ArrayList<Cb2xmlDocument>();


	/**
	 * Create External Record Definition
	 *
	 */
	public ExternalRecord () {
		super();

	}

	public ExternalRecord (
			final int pRecordId
			, final String pRecordName
			, final String pDescription
			, final int pRecordType
			, final int pSystem
			, final String pListChar
			, final String pCopyBook
			, final String pDelimiter
			, final String pQuote
			, final int pPosRecInd
			, final String pRecSepList
			, final byte[] pRecordSep
			, final String pFontName
			, final int precordStyle
			, final int pfileStructure
			) {
		this(pRecordId, pRecordName, pDescription, pRecordType, pSystem, pListChar, pCopyBook, pDelimiter, pQuote, pPosRecInd, pRecSepList, pRecordSep, pFontName, precordStyle, pfileStructure, false);
	}

	public ExternalRecord (
			final int pRecordId
			, final String pRecordName
			, final String pDescription
			, final int pRecordType
			, final int pSystem
			, final String pListChar
			, final String pCopyBook
			, final String pDelimiter
			, final String pQuote
			, final int pPosRecInd
			, final String pRecSepList
			, final byte[] pRecordSep
			, final String pFontName
			, final int precordStyle
			, final int pfileStructure
			, final boolean pEmbeddedCr
			) {
		super(pRecordId, pRecordName, pDescription, pRecordType, pSystem, pListChar,
				pCopyBook, pDelimiter, pQuote, pPosRecInd, pRecSepList, pRecordSep, 
				pFontName, precordStyle, pfileStructure, pEmbeddedCr);

	}

	/**
	 * Add a fixed width field into the fields list
	 * @param fld field to be added
	 * @param adjustment how subsequent fields will be adjusted
	 * 
	 * @return position where the field was inserted 
	 */
	public int addFixedWidthField(ExternalField fld, FieldAdjustmentOptions adjustment) {
		int pos = fld.getPos();
		int len = fld.getLen();
		int idx = 0;
		if (pos < 1 || len < 1) {
			throw new RecordException("Invalif position/length: " + pos + "/" + len);
		}
		
		super.loadFields();
		
		switch (adjustment) {
		
		case NO_ADJUSTMENT:
			return insertField(fld);
		case ADJUST_BY_LENGTH:
			idx = insertField(fld);
			adjustFieldPositions(idx, len);
			break;
		case CALCULATE_ADJUSTMENT:
			Collections.sort(fields, new Comparator<ExternalField>() {
				@Override public int compare(ExternalField o1, ExternalField o2) {
					return Integer.compare(o1.getPos(), o2.getPos());
				}
			});
			
			for (int i = 0; i < fields.size(); i++) {
				if (fields.get(i).getPos() >= pos) {
					int adj = len + pos - fields.get(i).getPos();
					fields.add(i, fld);
					
					if (adj > 0) {
						adjustFieldPositions(i, adj);
					}
					return i;
				}
			}
			
			idx = fields.size();
			fields.add(fld);
		}
		
		return idx;
	}
	
	private int insertField(ExternalField fld) {
		int pos = fld.getPos();
		for (int i = 0; i < fields.size(); i++) {
			if (fields.get(i).getPos() >= pos) {
				fields.add(i, fld);
				return i;
			}
		}
		
		fields.add(fld);
		return fields.size() - 1;
	}


	private void adjustFieldPositions(int idx, int adj) {
		ExternalField cf;
		for (int j = idx+1; j < fields.size(); j++) {
			cf = fields.get(j);
			if (cf.getPos() > 0) {
				cf.setPos(cf.getPos() + adj);
			}
		}
	}
	

	/**
	 *  This method returns clones the current record
	 *
	 *  @return a duplicate of the current record
	 */
	public Object clone() {
		return fullClone();
	}


	/**
	 * clone as a ExternalRecord
	 * @return cloned record
	 */
	@SuppressWarnings("deprecation")
	public ExternalRecord fullClone() {

		ExternalRecord ret;

		try {
			ret = (ExternalRecord) super.clone();
		} catch (Exception e) {
			ret = new ExternalRecord(
					super.getRecordId()
					, super.getRecordName()
					, super.getDescription()
					, super.getRecordType()
					, super.getSystem()
					, super.getListChar()
					, super.getCopyBook()
					, super.getDelimiter()
					, super.getQuote()
					, super.getPosRecInd()
					, super.getRecSepList()
					, super.getRecordSep()
					, super.getFontName()
					, super.getRecordStyle()
					, super.getFileStructure()
					, false
					);
		}
		return ret;
	}





	/**
	 * @return the recordDecider
	 */
	final RecordDecider getRecordDecider() {
		return recordDecider;
	}

	/**
	 * @param recordDecider the recordDecider to set
	 */
	public final void setRecordDecider(RecordDecider recordDecider) {
		this.recordDecider = recordDecider;
	}







	/**
	 * Create a new record
	 * @param pRecordName name of the new record
	 * @param fontName fontname to use
	 *
	 * @return the new record
	 */
	public static final ExternalRecord getNullRecord(final String pRecordName,
			final String fontName) {

		return getNullRecord(pRecordName, Constants.rtRecordLayout, fontName);
	}


	/**
	 * Create a new record
	 * @param pRecordName name of the new record
	 * @param recordType record type for the record
	 * @param fontName fontname to use
	 *
	 * @return the new record
	 */
	public static final ExternalRecord getNullRecord(final String pRecordName,
			final int recordType,
			final String fontName) {

		return new ExternalRecord(-1, pRecordName, "", recordType, 0, "N",
				"", "<Tab>", "", 0, Constants.DEFAULT_STRING, Constants.SYSTEM_EOL_BYTES, fontName, 0, -1, false);
	}


	//  Code for implementing IBasicSchema
	//
	//	/* (non-Javadoc)
	//	 * @see net.sf.JRecord.Common.IBasicFileSchema#isBinary()
	//	 */
	//	//@Override
	//	public boolean isBinary() {
	//
	//		for (int i = subRecords.size() - 1; i >= 0; i--) {
	//			if (subRecords.get(i).isBinary()) {
	//				return true;
	//			}
	//		}
	//		TypeManager typeMgr = TypeManager.getInstance();
	//		for (int i = fields.size() - 1; i >= 0; i--) {
	//			if (typeMgr.getType(fields.get(i).getType()).isBinary()) {
	//				return true;
	//			}
	//		}
	//		return false;
	//	}
	//
	//	/* (non-Javadoc)
	//	 * @see net.sf.JRecord.Common.IBasicFileSchema#getMaximumRecordLength()
	//	 */
	//	//@Override
	//	public int getMaximumRecordLength() {
	//		int len = 0;
	//		for (int i = subRecords.size() - 1; i >= 0; i--) {
	//			len = Math.max(len, subRecords.get(i).getMaximumRecordLength());
	//		}
	//		
	//		for (int i = fields.size() - 1; i >= 0; i--) {
	//			len = Math.max(len, fields.get(i).getPos() + fields.get(i).getLen() - 1);
	//		}
	//		return len;
	//	}


	/**
	 * Get a copy of all subrecords.
	 *
	 * @return Sub records
	 */
	public ExternalRecord[] toArray() {
		return subRecords.toArray(new ExternalRecord[subRecords.size()]);
	}


	/**
	 * Add a Csv field to the Record
	 * 
	 * @param name Field name
	 * @param type Field Type
	 * @param decimal number of decimal places
	 * 
	 * @return This Record.
	 */
	@Override
	public ExternalRecord addCsvField(String name, int type, int decimal) {
		addRecordField(new ExternalField(fields.size() + 1, Constants.NULL_INTEGER, name, "", type, decimal, 0, "", "", "", 0));
		return this;
	}

	/**
	 * Add a field to the Record
	 * 
	 * @param name Field name
	 * @param type Field Type
	 * @param pos Fields position in the record
	 * @param length Field length
	 * @param decimal number of decimal places
	 * 
	 * @return This Record.
	 */
	@Override
	public ExternalRecord addField(String name, int type, int pos, int length, int decimal) {
		addRecordField(new ExternalField(pos, length, name, "", type, decimal, 0, "", "", "", 0));
		return this;
	}



	/**
	 * Add a field to the Record with the field length (and calculate the position)
	 * 
	 * @param name Field name
	 * @param type Field Type
	 * @param length Field length
	 * @param decimal number of decimal places
	 * 
	 * @return This Record.
	 */
	@Override
	public ExternalRecord addFieldByLength(String name, int type, int length, int decimal) {
		addRecordField(new ExternalField(calcNextPos(), length, name, "", type, decimal, 0, "", "", "", 0));
		return this;
	}



	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.FixedWidthSchemaBuilders.IByLengthBuilder#skipBytes(int)
	 */
	@Override
	public ExternalRecord skipBytes(int numberOfBytes) {
		lastPosition = calcNextPos() + numberOfBytes;
		return this;
	}

	private int calcNextPos() {
		int pos = 1;
		if (fields.size() > 0) {
			ExternalField lf = fields.get(fields.size() - 1);
			pos = Math.max(lastPosition, lf.getPos() + lf.getLen());
		}
		return pos;
	}

	/**
	 * Add a field to the Record using the Field position and calculating lengths
	 * 
	 * @param name Field name
	 * @param type Field Type
	 * @param pos Fields position in the record
	 * @param decimal number of decimal places
	 * 
	 * @return This Record.
	 */
	@Override
	public ExternalRecord addFieldByPosition(String name, int type, int pos, int decimal) { 
		int length = 1;

		setLastFieldsLength(pos);

		addRecordField(new ExternalField(pos, length, name, "", type, decimal, 0, "", "", "", 0));
		return this;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.FixedWidthSchemaBuilders.IByPositionBuilder#skipFieldPosition(int)
	 */
	@Override
	public ExternalRecord skipFieldPosition(int pos) {
		setLastFieldsLength(pos);
		lastPosition = pos;
		return this;
	}

	private void setLastFieldsLength(int pos) {
		if (fields.size() > 0) {
			ExternalField lf = fields.get(fields.size() - 1);
			if (lf.getPos() >= lastPosition) {
				lf.setLen(pos -  lf.getPos()); 
			}
		}

	}

	/**
	 * Add a field to the Record using the Field position and calculating lengths
	 * 
	 * @param name Field name
	 * @param type Field Type
	 * @param pos Fields position in the record
	 * @param length Field length
	 * @param decimal number of decimal places
	 * 
	 * @return This Record.
	 */
	@Override
	public ExternalRecord addFieldByPosition(String name, int type, int pos, int length, int decimal) {

		if (fields.size() > 0) {
			ExternalField lf = fields.get(fields.size() - 1);
			lf.setLen(pos -  lf.getPos());
		}
		addRecordField(new ExternalField(pos, length, name, "", type, decimal, 0, "", "", "", 0));
		return this;
	}






	//	/**
	//	 * @return the tstFields
	//	 */
	//	public List<TstField> getTstFields() {
	//		return tstFields;
	//	}
	//
	//	private ArrayList<TstField> getTestField() {
	//		if (tstFields == null) {
	//			tstFields = new ArrayList<TstField>(5);
	//		}
	//		return tstFields;
	//	}


	/**
	 * Convert to internal format
	 * @return the internal LayoutDetail equivalent
	 *
	 */
	public final LayoutDetail asLayoutDetail() {
		return ToLayoutDetail.getInstance().getLayout(this);
	}

	
	/**
	 * Convert it the ExternalRecord Into an IOBuilder
	 * @return
	 */
	public final net.sf.JRecord.def.IO.builders.ISchemaIOBuilder asIOBuilder() {
		LayoutDetail layoutDetail = this.asLayoutDetail();
		return layoutDetail==null
				? null
				: SchemaIOBuilder.newSchemaIOBuilder(layoutDetail);
	}


	//	/**
	//	 * For internal use - Get parent record name (may not be set)
	//	 * @return parent name
	//	 */
	//	private String getParentName() {
	//		return parentName;
	//	}



	//	/**
	//	 * @param e cb2xml document details
	//	 * @see java.util.ArrayList#add(java.lang.Object)
	//	 */
	//	public void addCb2xmlDocument(Cb2xmlDocument e) {
	//		cb2xmlDocuments.add(e);
	//	}


	public static IFixedWidthSchemaBuilder newFixedWidthRecord(String name, int fileStructure, String fontName) {
		ExternalRecord r =  getNullRecord(name, Constants.rtRecordLayout, fontName);
		r.setFileStructure(fileStructure);

		return r;
	}


	public static ICsvSchemaBuilder newCsvRecord(String name, int fileStructure, String fontName, String delimeter, String quote) {
		ExternalRecord r =  new ExternalRecord(-1, name, "", Constants.rtDelimited, 0, "N",
				"", delimeter, quote, 0, Constants.DEFAULT_STRING, Constants.SYSTEM_EOL_BYTES, fontName, 0, fileStructure, false);

		return r;
	}
}
