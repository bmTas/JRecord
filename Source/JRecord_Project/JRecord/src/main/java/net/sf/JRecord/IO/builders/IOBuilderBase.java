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

package net.sf.JRecord.IO.builders;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import net.sf.JRecord.ByteIO.IByteRecordReader;
import net.sf.JRecord.ByteIO.IByteRecordWriter;
import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.CharLineProvider;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LineProvider;
import net.sf.JRecord.Details.RecordDecider;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineByteRecordReaderWrapper;
import net.sf.JRecord.IO.LineByteRecordWriterWrapper;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.IO.ListLineReader;
import net.sf.JRecord.IO.ListLineWriter;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Option.IRecordPositionOption;


/**
 * Base class for the various IOBuilders. IOBuilders are used to create
 * LineReaders (readers for Cobol files) and LineWriter (Writers for cobol files)
 * 
 * @author Bruce Martin
 *
 */
public abstract class IOBuilderBase<IOB> /*implements ISchemaIOBuilder*/  {

	private static final TextLog DEFAULT_LOG = new TextLog();
	
	@SuppressWarnings("unchecked")
	protected final IOB self = (IOB) this;
	private LayoutDetail layout = null;
	private LineProvider lineProvider;
	private RecordDecider recordDecider=null;
	Map<String, RecordUpdate> recordSelectionMap = null;

	int dialect;
    //final int copybookType;

	int splitCopybook = CopybookLoader.SPLIT_NONE;
	private String defaultFont = null;
	private String font = null;
	int copybookFileFormat = 1; // actually Cb2xmlConstants.USE_STANDARD_COLUMNS;
	int fileOrganization = Constants.NULL_INTEGER;
//	int defaultFileOrganization = Constants.NULL_INTEGER;
	boolean dropCopybookNameFromFields = false;
	Boolean initToSpaces = null;
	int recordLength = -1;
	 
	
    AbsSSLogger log = DEFAULT_LOG; 
    
	protected IOBuilderBase(int dialect) {
		super();
		this.dialect = dialect;
	}

 
	protected IOBuilderBase(LayoutDetail schema) {
		super();
		//this.copybookType = copybookType;
		this.layout = schema;
		this.lineProvider = LineIOProvider.getInstance().getLineProvider(layout);
	}


	/**
	 * @param dialect the dialect to set
	 */
	public final IOB setDialect(int dialect) {
		this.dialect = dialect;
		clearLayout();
		return self;
	}




	/**
	 * @see net.sf.JRecord.def.IO.ICobolIOBuilder#setSplitCopybook(int)
	 */
	public IOB setSplitCopybook(int splitCopybook) {
		this.splitCopybook = splitCopybook;
		clearLayout();
		return self;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#setFont(java.lang.String)
	 */
	public IOB setFont(String font) {
		this.font = font;
		this.defaultFont = font;
		clearLayout();
		return self;
	}

	public IOB setDefaultFont(String defaultFont) {
		this.defaultFont = defaultFont;
		clearLayout();
		return self;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#setCopybookFileFormat(int)
	 */
	public final IOB setCopybookFileFormat(int copybookFileFormat) {
		this.copybookFileFormat = copybookFileFormat;
		clearLayout();
		return self;
	}

	

	/**
	 * @return the fileOrganization
	 */
	protected int getFileOrganization() {
//		if (fileOrganization < 0 && defaultFileOrganization >= 0) {
//			return defaultFileOrganization;
//		}
		return fileOrganization;
	}
//
//	public IOB setDefaultFileOrganization(int defaultFileOrganization) {
//		this.defaultFileOrganization = defaultFileOrganization;
//		clearLayout();
//		return self;
//	}


	/**
	 * @param fileOrganization the fileOrganization to set
	 */
	public IOB setFileOrganization(int fileOrganization) {
		this.fileOrganization = fileOrganization;
		clearLayout();
		return self;
	}




	/**
	 * @return the dropCopybookNameFromFields
	 */
	public final boolean isDropCopybookNameFromFields() {
		return dropCopybookNameFromFields;
	}


	/**
	 * @param dropCopybookNameFromFields the dropCopybookNameFromFields to set
	 */
	public final IOB setDropCopybookNameFromFields(
			boolean dropCopybookNameFromFields) {
		this.dropCopybookNameFromFields = dropCopybookNameFromFields;
		clearLayout();
		return self;
	}




	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#setLog(net.sf.JRecord.Log.AbsSSLogger)
	 */
	public final IOB setLog(AbsSSLogger log) {
		this.log = log;
		clearLayout();
		return self;
	} 
	
	
    /**
	 * @see net.sf.JRecord.def.IO.builders.ICobolIOBuilder#newLine()
	 */
	public AbstractLine newLine() throws IOException {
		LayoutDetail schema = getLayout();
		
		return lineProvider.getLine(schema);
	}


    /**
	 * @see net.sf.JRecord.def.IO.builders.ICobolIOBuilder#newLine()
	 */
	public AbstractLine newLine(byte[] data) throws IOException {
		LayoutDetail schema = getLayout();
		
		return lineProvider.getLine(schema, data);
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#newReader(java.lang.String)
	 */
	public final AbstractLineReader newReader(String filename) throws FileNotFoundException, IOException {
		//checkOk(true);
		return newReader(new FileInputStream(filename));
	}
	
    /* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#newReader(java.io.InputStream)
	 */
	public final AbstractLineReader newReader(InputStream datastream) throws IOException {
		checkOk(true);
		LayoutDetail schema = getLayout();
		AbstractLineReader r = LineIOProvider.getInstance().getLineReader(schema);
		
		r.open(datastream, schema);
		return r;
	}
	
	public final ListLineReader newReader(List<? extends AbstractLine> lines) throws IOException {
		return new ListLineReader(lines, getLayout());
	}
	
	public final AbstractLineReader newReader(IByteRecordReader reader) throws IOException {
		checkOk(true);
		LayoutDetail schema = getLayout();
		
		return new LineByteRecordReaderWrapper<IByteRecordReader>(lineProvider, reader, schema);
	}
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#newWriter(java.lang.String)
	 */
	public final AbstractLineWriter newWriter(String filename) throws FileNotFoundException, IOException {
		checkOk(false);
		return newWriter(new FileOutputStream(filename));
	}

	public final ListLineWriter newListWriter() {
		return new ListLineWriter();
	}
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#newWriter(java.io.OutputStream)
	 */
	public final AbstractLineWriter newWriter(IByteRecordWriter writer) { 
		checkOk(false);
		//LayoutDetail schema = getLayout();

		return new LineByteRecordWriterWrapper<IByteRecordWriter>(writer);
	}
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#newWriter(java.io.OutputStream)
	 */
	public final AbstractLineWriter newWriter(OutputStream datastream) throws IOException {
		checkOk(false);
		LayoutDetail schema = getLayout();
		AbstractLineWriter r = LineIOProvider.getInstance().getLineWriter(schema);
		
		r.open(datastream);
		return r;
	}
	
	/**
	 * Method to allow ChildBuilders to validate the schema prior to
	 * creating Reader / Writer
	 * @param input
	 */
	protected void checkOk(boolean input) {
		
	}
	
	/**
	 * Get all the attributes:
	 * <pre>
	 *     dialect,
	 *     splitCopybook,
	 *     copybookFileFormat,
	 *     fileOrganization,
	 *     font,
	 *     dropCopybookNameFromFields,
	 * </pre>    
	 * Used to test if attributes are set correctly
	 * @return all the attributes
	 * 
	 * @deprecated for testing
	 */ @Deprecated
	public final Object[] getAllAttributes() {
		Object[] r = {
				dialect, 
				splitCopybook,
				copybookFileFormat,// actually Cb2xmlConstants.USE_STANDARD_COLUMNS;
				fileOrganization,
				getFont(),
				dropCopybookNameFromFields,
				//defaultFileOrganization,
				defaultFont
				
		};
		 
		return r;

	}
	
	
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.builders.ICobolIOBuilder#setRecordSelection(java.lang.String, net.sf.JRecord.ExternalRecordSelection.ExternalSelection)
	 */
	public IOB setRecordSelection(String recordName, ExternalSelection selectionCriteria) {
		getRecordUpdate(recordName).selection = selectionCriteria;
		clearLayout();

		return self;
	}


	public IOB setRecordPositionCode(String recordName, IRecordPositionOption positionOption) {
		getRecordUpdate(recordName).positionCode = positionOption;
		clearLayout();

		return self;
	}


	public IOB setRecordParent(String recordName, String parentName) {
		getRecordUpdate(recordName).parentName = parentName;
		clearLayout();

		return self;
	}


	/**
	 * @param recordDecider the recordDecider to set
	 */
	public final IOB setRecordDecider(RecordDecider recordDecider) {
		this.recordDecider = recordDecider;
		clearLayout();

		return self;
	}

	
	public IOB setRecordLength(int recordLength) {
		this.recordLength = recordLength;
		clearLayout();

		return self;
	}


	/**
	 * @param initToSpaces the initToSpaces to set
	 */
	public final IOB setInitToSpaces(boolean initToSpaces) {
		this.initToSpaces = Boolean.valueOf(initToSpaces);
		clearLayout();

		return self;
	}


	public String getFont() {
		return deriveActualFont(font == null ? defaultFont : font);
	}
	
//	public String getDefaultFont() {	
//		return deriveActualFont(defaultFont);
//	}
//

	private String deriveActualFont(String f) {
		
		if (f == null) {
			f = "";
			if (Conversion.DEFAULT_CHARSET_DETAILS.isMultiByte
			&&  CommonBits.getLineType(fileOrganization) == CommonBits.LT_BYTE) {
				f = Conversion.getDefaultSingleByteCharacterset();
			}
		}
		return f;
	}

	public final ExternalRecord getExternalRecord() throws IOException  {			
		ExternalRecord schema =  getExternalRecordImpl();
				
		schema.setRecordDecider(recordDecider);
		schema.setRecordLength(recordLength);
		//schema.setFontName(getFont());
		
		if (font != null) {
			schema.setFontName(font);
		}
		
		if (fileOrganization >= 0) {
			schema.setFileStructure(fileOrganization);
//		} else if (defaultFileOrganization >= 0 && schema.getFileStructure() <= 0 
//				&& schema.isFileStructureUpdated() == false) {
//			schema.setFileStructure(defaultFileOrganization);
		}
		
		if (initToSpaces == null) {
			schema.setInitToSpaces(! schema.isBinary());
		} else {
			schema.setInitToSpaces(initToSpaces);
		}
		
		if (recordSelectionMap != null) {
			for (int i = 0; i < schema.getNumberOfRecords(); i++) {
				ExternalRecord record = schema.getRecord(i);
				RecordUpdate recUpdate = recordSelectionMap.get(record.getRecordName().toLowerCase());
				if (recUpdate != null) {
					if (recUpdate.selection != null) {	
						record.setRecordSelection(recUpdate.selection);
					}
		
					if (recUpdate.positionCode != null) {
						record.setRecordPositionOption(recUpdate.positionCode);
					}
					
					if (recUpdate.parentName != null) {
						record.setParentName(recUpdate.parentName);
					}
				}
			}
		}

		return schema; 
	}
	
	private RecordUpdate getRecordUpdate(String recordName) {
		RecordUpdate rec;
		if (recordSelectionMap == null) {
			synchronized (this) {
				if (recordSelectionMap == null) {
					recordSelectionMap = new TreeMap<String, RecordUpdate>();
				}
			}
		}
		
		String key = recordName.toLowerCase();
		rec = recordSelectionMap.get(key);
		if (rec == null) {
			rec = new RecordUpdate();
			recordSelectionMap.put(key, rec);
		}

		return rec;
	}
	
	protected abstract ExternalRecord getExternalRecordImpl() throws IOException;

	protected void clearLayout() {
		if (layout != null) {
			synchronized (this) {
				layout = null;
			}
		}
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#getLayout()
	 */
	public final LayoutDetail getLayout() throws IOException {
		LayoutDetail l = layout;
		if (l == null) {
			synchronized (this) {
				if (layout == null) {
					layout = getExternalRecord()	.asLayoutDetail();
					lineProvider = LineIOProvider.getInstance().getLineProvider(layout);
				}
				l = layout;
			}
		}
		
		if (l.hasBinaryField()) {
			if (CommonBits.getLineType(l.getFileStructure()) != CommonBits.LT_BYTE) {
				throw new RuntimeException("This is a binary Layout (Schema) but you have selected a Character file Organisation,"
						+ " you must use you must Byte based Organisation");
			}
			if (lineProvider instanceof CharLineProvider) {
				throw new RuntimeException("This is a binary schema (requires byte based line-provider), but the line provider is a Text Based Line provider");
			}
		}

		return l;
	}

	
	
	private static class RecordUpdate {
		ExternalSelection selection = null;
		String parentName = null;
		IRecordPositionOption positionCode = null;
	}
	
}
