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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.Def.AbstractUpdatableRecord;
import net.sf.JRecord.External.Def.DependingOn;
import net.sf.JRecord.External.Def.DependingOnDefinition;
import net.sf.JRecord.External.Def.DependingOnDtls;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.External.Def.IFieldUpdatedListner;
import net.sf.JRecord.External.Item.IItemJRec;
import net.sf.JRecord.External.Item.IItemJRecUpd;
import net.sf.JRecord.External.Item.ItemJRec;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalGroupSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.ExternalRecordSelection.StreamLine;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Option.ICobolSplitOptions;
import net.sf.JRecord.Option.IRecordPositionOption;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;
import net.sf.cb2xml.def.ICopybook;
import net.sf.cb2xml.def.IItemJrUpd;

//import net.sf.RecordEditor.utils.Common;

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
public class BaseExternalRecord<xRecord extends BaseExternalRecord<xRecord>> 
extends AbstractUpdatableRecord 
implements IFieldUpdatedListner, IAddDependingOn {

	protected static final int POSITION_IDX = 0;
	protected static final int LENGTH_IDX = 1;
	private int recordId;
	private int initRecordId; 
	private String recordName;
	private String description;
	private    int recordType;
	private    int system;
	private String systemName = null;
	private String listChar;
	private String copyBook;
	private String delimiter;
	private String quote;
	private int posRecInd;
	private String recSepList;
	private byte[] recordSep;
	private String fontName;
	private    int recordStyle;
	private    int fileStructure;
	private    int lineNumberOfFieldNames = 1;
	private    int recordLength = -1;
	private    int dialectCode = ICopybookDialects.FMT_MAINFRAME;
	private String[] parentGroupNames;
	
	private String lineFormatParam = "";

	private ExternalSelection recSelect;
	private IRecordPositionOption recordPosistionOption = null;
	//private ArrayList<TstField> tstFields = null;
	private boolean defaultRecord = false;
	private boolean embeddedCr    = false;  //private String tstField = "";
	private boolean initToSpaces  = false;  // for backward compatibility
	protected boolean optimizeTypes = true;
//	private boolean fileStructureUpdated = false; 
//  private RecordDecider recordDecider = null;
  //private String tstFieldValue = "";
	protected List<? extends IItemJRecUpd> items;
	private ICopybook copybook;


	private int parentRecord = -1;

	private String parentName = null;
	private String copybookPref;


	protected ArrayList<xRecord> subRecords = new ArrayList<xRecord>();
	protected ArrayList<ExternalField> fields = new ArrayList<ExternalField>(250);
	protected ArrayList<DependingOn> dependingOn = new ArrayList<DependingOn>(3);
	private DependingOnDefinition dependingOnDef;

	//  private int lastPosition = -1;

	private ArrayList<Cb2xmlDocument> cb2xmlDocuments = new ArrayList<Cb2xmlDocument>();
	private boolean keepFillers = false, 
					dropCopybookFromFieldNames, 
					//saveCb2xml = false,
					useJRecordNaming;
	

	@SuppressWarnings("unchecked")
	private final xRecord self = (xRecord) this;
	

  /**
   * Create External Record Definition
   *
   */
  public BaseExternalRecord () {
      super(true);

      recordId = 0;
      recordName = "";
      description = "";
      recordType = 0;
      system = 0;
      listChar = "";
      copyBook = "";
      delimiter = "";
      quote = "";
      posRecInd = 0;
      recSepList = "";
      recordSep = NULL_BYTES;
      fontName = "";
      recordStyle = 0;
      fileStructure = 0;

      setKeys();
  }

  
  public BaseExternalRecord (
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
      super(false);

      recordId = pRecordId;
      recordName = pRecordName;
      description = pDescription;
      recordType = pRecordType;
      system = pSystem;
      listChar = pListChar;
      copyBook = pCopyBook;
      delimiter = pDelimiter;
      quote = pQuote;
      posRecInd = pPosRecInd;
      recSepList = pRecSepList;
      recordSep = pRecordSep;
      fontName = pFontName;
      recordStyle = precordStyle;
      fileStructure = pfileStructure;
      embeddedCr = pEmbeddedCr;

      setKeys();
  }


  /**
   *  This method copies the key fields to the Init* fields
   */
  public void setKeys() {

      initRecordId = recordId;
  }

//  /**
//   *  This method returns clones the current record
//   *
//   *  @return a duplicate of the current record
//   */
//  public Object clone() {
//      return fullClone();
//  }
//
//
//  /**
//   * clone as a ExternalRecord
//   * @return cloned record
//   */
//  public ExternalRecord fullClone() {
//
//      ExternalRecord ret;
//
//      try {
//          ret = (ExternalRecord) super.clone();
//      } catch (Exception e) {
//          ret = new ExternalRecord(
//                  recordId
//                  , recordName
//                  , description
//                  , recordType
//                  , system
//                  , listChar
//                  , copyBook
//                  , delimiter
//                  , quote
//                  , posRecInd
//                  , recSepList
//                  , recordSep
//                  , fontName
//                  , recordStyle
//                  , fileStructure
//                  , false
//          );
//      }
//      return ret;
//  }
//



  /**
   *  This method gets the vaule of RecordId
   * @return record identifier
   */
  public int getRecordId() {
      return recordId;
  }

  /**
   *  This method sets the vaule of RecordId
   *
   * @param val value to be assigned to RecordId
   */
  public void setRecordId(int val) {

      if ((val != recordId) || (updateStatus == NULL_INT_VALUE)) {
           recordId = val;
           updateStatus = UPDATED;
      }
  }

  /**
   *  This method gets the vaule of RecordName
   * @return record name
   */
  public String getRecordName() {
      return recordName;
  }

  /**
   *  This method sets the vaule of RecordName
   *
   * @param val value to be assigned to RecordName
   */
  public void setRecordName(String val) {

      if ((val == null || "".equals(val))
      && (recordName == null || "".equals(recordName))) {
          return;
      }

      if ((val == null) || (! val.equals(recordName)) || (updateStatus == NULL_INT_VALUE)) {
           recordName = val;
           updateStatus = UPDATED;
      }
  }

//  /**
// * @return the recordDecider
// */
//final RecordDecider getRecordDecider() {
//	return recordDecider;
//}
//
///**
// * @param recordDecider the recordDecider to set
// */
//public void setRecordDecider(RecordDecider recordDecider) {
//	this.recordDecider = recordDecider;
//}

/**
   *  This method gets the vaule of Description
   * @return record description
   */
  public String getDescription() {
      return description;
  }

  /**
   *  This method sets the vaule of Description
   *
   * @param val value to be assigned to Description
   */
  public void setDescription(String val) {

      if ((val == null || "".equals(val))
      && (description == null || "".equals(description))) {
          return;
      }

      if ((val == null) || (! val.equals(description)) || (updateStatus == NULL_INT_VALUE)) {
           description = val;
           updateStatus = UPDATED;
      }
  }

  /**
   *  This method gets the vaule of RecordType
   * @return record type
   */
  public int getRecordType() {
      return recordType;
  }

  /**
   *  This method sets the vaule of RecordType
   *
   * @param val value to be assigned to RecordType
   */
  public xRecord setRecordType(int val) {

      if ((val != recordType) || (updateStatus == NULL_INT_VALUE)) {
           recordType = val;
           updateStatus = UPDATED;
      }
      return self;
  }

  /**
   *  This method gets the vaule of System
   * @return system identifier
   * 
   * @deprecated Was originally Used in the RecordEditor; it serves not purpose in JRecord. 
   */
  public int getSystem() {
      return system;
  }

  /**
   *  This method sets the vaule of System
   *
   * @param val value to be assigned to System
   * 
   * @deprecated Was originally Used in the RecordEditor; it serves not purpose in JRecord. 
   */
  public void setSystem(int val) {

      if ((val != system) || (updateStatus == NULL_INT_VALUE)) {
           system = val;
           updateStatus = UPDATED;
      }
  }

  /**
   *  This method gets the vaule of ListChar
   * @return list (ie Y(es) or N(o))
   */
  public String getListChar() {
      return listChar;
  }

  /**
   *  This method sets the vaule of ListChar
   *
   * @param val value to be assigned to ListChar
   */
  public void setListChar(String val) {

      if ((val == null || "".equals(val))
      && (listChar == null || "".equals(listChar))) {
          return;
      }

      if ((val == null) || (! val.equals(listChar)) || (updateStatus == NULL_INT_VALUE)) {
           listChar = val;
           updateStatus = UPDATED;
      }
  }

  /**
   * This method gets the vaule of CopyBook
   * @return the cobol copybook name
   */
  public String getCopyBook() {
      return copyBook;
  }

  /**
   *  This method sets the vaule of CopyBook
   *
   * @param val value to be assigned to CopyBook
   * 
   * @deprecated Was originally Used in the RecordEditor; it serves not purpose in JRecord. 
   */
  public void setCopyBook(String val) {

      if ((val == null || "".equals(val))
      && (copyBook == null || "".equals(copyBook))) {
          return;
      }

      if ((val == null) || (! val.equals(copyBook)) || (updateStatus == NULL_INT_VALUE)) {
           copyBook = val;
           updateStatus = UPDATED;
      }
  }

  /**
   * This method gets the vaule of Delimiter
   * @return field delimiter (CSV files)
   */
  public String getDelimiter() {
      return delimiter;
  }

  /**
   *  This method sets the vaule of Delimiter
   *
   * @param val value to be assigned to Delimiter
   */
  public void setDelimiter(String val) {

      if ((val == null || "".equals(val))
      && (delimiter == null || "".equals(delimiter))) {
          return;
      }

      if ((val == null) || (! val.equals(delimiter)) || (updateStatus == NULL_INT_VALUE)) {
           delimiter = val;
           updateStatus = UPDATED;
      }
  }

  	/**
	 * @param keepFillers the keepFillers to set
	 */
	public xRecord setCobolConversionOptions(
			boolean keepFillers, boolean dropCopybookFromFieldNames, 
			boolean saveCb2xml,  boolean useJRecordNaming) {

		this.keepFillers = keepFillers;
		this.dropCopybookFromFieldNames = dropCopybookFromFieldNames;
		//this.saveCb2xml = saveCb2xml;
		this.useJRecordNaming = useJRecordNaming;

		return self;
	}

	public xRecord setCobolConversionOptions(
				boolean keepFillers,
				boolean dropCopybookFromFieldNames, 
				boolean useJRecordNaming) {

		this.keepFillers = keepFillers;
		this.dropCopybookFromFieldNames = dropCopybookFromFieldNames;
		this.useJRecordNaming = useJRecordNaming;

		return self;
	}


/**
   * This method gets the vaule of Quote
   * @return Quote
   */
  public String getQuote() {
	  if (quote == null) {
		  return "";
	  }
      return quote;
  }

  /**
   *  This method sets the vaule of Quote
   *
   * @param val value to be assigned to Quote
   */
  public void setQuote(String val) {

      if ((val == null || "".equals(val))
      && (quote == null || "".equals(quote))) {
          return;
      }

      if ((val == null) || (! val.equals(quote)) || (updateStatus == NULL_INT_VALUE)) {
           quote = val;
           updateStatus = UPDATED;
      }
  }

  /**
   *  This method gets the vaule of PosRecInd
   * @return pos record id
   */
  public int getPosRecInd() {
      return posRecInd;
  }

  /**
   *  This method sets the vaule of PosRecInd
   *
   * @param val value to be assigned to PosRecInd
   */
  public void setPosRecInd(int val) {

      if ((val != posRecInd) || (updateStatus == NULL_INT_VALUE)) {
           posRecInd = val;
           updateStatus = UPDATED;
      }
  }

  /**
   * This method gets the value of RecSepList
   * @return record Separator list value
   */
  public String getRecSepList() {
      return recSepList;
  }

  /**
   *  This method sets the vaule of RecSepList
   *
   * @param val value to be assigned to RecSepList
   */
  public void setRecSepList(String val) {

      if ((val == null || "".equals(val))
      && (recSepList == null || "".equals(recSepList))) {
          return;
      }

      if ((val == null) || (! val.equals(recSepList)) || (updateStatus == NULL_INT_VALUE)) {
           recSepList = val;
           updateStatus = UPDATED;
      }
  }

  /**
   * This method gets the vaule of RecordSep
   * @return Record Seperapors
   */
  public byte[] getRecordSep() {
      return recordSep;
  }

  /**
   *  This method sets the vaule of RecordSep
   *
   * @param val value to be assigned to RecordSep
   */
  public void setRecordSep(byte[] val) {

      if ((val == null) && (recordSep == null)) {
          return;
      }
      if (! isEqual(val, recordSep) || (updateStatus == NULL_INT_VALUE)) {
           recordSep = val;
           updateStatus = UPDATED;
      }
  }

  /**
   * This method gets the vaule of font name
   * @return font name
   */
  public String getFontName() {
      return fontName;
  }

  /**
   *  This method sets the value of fontName
   *
   * @param val value to be assigned to Canonical_Name
   */
  public xRecord setFontName(String val) {

      if ((val == null || "".equals(val))
      && (fontName == null || "".equals(fontName))) {
          return self;
      }

      if ((val == null) || (! val.equals(fontName)) || (updateStatus == NULL_INT_VALUE)) {
           fontName = val;
           updateStatus = UPDATED;
      }
      return self;
  }

  /**
   * This method gets the vaule of RecordStyle
   * @return Record Style
   */
  public int getRecordStyle() {
      return recordStyle;
  }

  /**
   *  This method sets the vaule of RecordStyle
   *
   * @param val value to be assigned to RecordStyle
   */
  public xRecord setRecordStyle(int val) {

      if ((val != recordStyle) || (updateStatus == NULL_INT_VALUE)) {
           recordStyle = val;
           updateStatus = UPDATED;
      }
      if (subRecords != null && subRecords.size() > 0) {
    	  for (xRecord r : subRecords) {
    		  r.setRecordStyle(val);
    	  }
      }
      return self;
  }

  /**
   * This method gets the vaule of FileStructure
   * @return File Structure
   */
  public int getFileStructure() {
	  //System.out.println(" ~~ getFileStructure " +  fileStructure);
      return fileStructure;
  }

  /**
   *  This method sets the vaule of FileStructure
   *
   * @param val value to be assigned to FileStructure
   */
  public xRecord setFileStructure(int val) {

      if ((val != fileStructure) || (updateStatus == NULL_INT_VALUE)) {
           fileStructure = val;
 //          fileStructureUpdated = true;
           updateStatus = UPDATED;
      }
      
      return self;
  }

//
//  public boolean isFileStructureUpdated() {
//	return fileStructureUpdated;
//  }


/**
   * Add a sub-record to this record
   * @param o record to add
   * @return wether added correctly
   */
  public boolean addRecord(xRecord o) {
      return subRecords.add(o);
  }



	/**
	 * @return the items
	 */
	public List<? extends IItemJRec> getItems() {
		return items;
	}
	
//	/**
//	 * @return the items
//	 */
//	protected final List<? extends IItemJrUpd> getItemsU() {
//		return items;
//	}

	/**
	 * @param items the items to set
	 */
	public void setItems(String copybookPref, String[] groupArray, int dialectCode, List<? extends IItemJrUpd> items) {
		this.copybookPref = copybookPref;
		this.parentGroupNames = groupArray;
		this.dialectCode  = dialectCode;
		
		if (items == null || items.size() == 0) {
			this.items = null;
		} else {
			ArrayList<IItemJRecUpd> itms = new ArrayList<IItemJRecUpd>();
			for (int i = 0; i < items.size(); i++) {
				itms.add(new ItemJRec(null, items.get(i)));
			}
			this.items = itms;
		}

		fields.clear();
	}

	/**
	 * @return the copybook
	 */
	public ICopybook getCopybook() {
		return copybook;
	}


	/**
	 * @param copybook the copybook to set
	 */
	public void setCopybook(ICopybook copybook) {
		this.copybook = copybook;
	}


	public void setItems(String copybookPref, String[] parentGroupNames, int dialectCode, IItemJrUpd item) {
		ArrayList<IItemJRecUpd> itms = new ArrayList<IItemJRecUpd>(1);
		itms.add(new ItemJRec(null, item));

		this.copybookPref = copybookPref;
		this.dialectCode  = dialectCode;
		this.parentGroupNames = parentGroupNames;
		this.items = itms;
		fields.clear();
	}


	/**
	 * @return the keepFillers
	 */
	public boolean isKeepFillers() {
		return keepFillers;
	}


	/**
	 * @return the useJRecordNaming
	 */
	public boolean isUseJRecordNaming() {
		return useJRecordNaming;
	}

	/**
	 * @return the dropCopybookFromFieldNames
	 */
	public boolean isDropCopybookFromFieldNames() {
		return dropCopybookFromFieldNames;
	}

	public void updateTypeOnCobolItems() {
		FieldCreatorHelper fldhelper = createFieldHelper();

		updateType(fldhelper, items);
	}


	private void updateType(FieldCreatorHelper fldhelper, List<? extends IItemJrUpd> items) {
		if (items != null) { 
			for (IItemJrUpd itm : items) {
				updateItemForType(fldhelper, itm);
				updateType(fldhelper, itm.getChildItems());
			}
		}
	}
	
	private void updateItemForType(FieldCreatorHelper fldhelper, IItemJrUpd item) {
		int typeId = Type.ftChar;
		List<? extends IItemJrUpd> childItems = item.getChildItems();
		if (childItems == null || childItems.size() == 0) {
			typeId = fldhelper.deriveType(
					item.getNumericClass().numeric, item.getUsage().getName(), item.getPicture(), 
					item.getSignClause().signSeparate, item.getSignClause().signPosition.getName(), 
					item.getJustified().isJustified); 
		}
		item.setType(typeId);
	}

	/**
	 * @return the dialectCode
	 */
	public int getDialectCode() {
		return dialectCode;
	}


	/**
	 * @return the parentGroupNames
	 */
	public String[] getParentGroupNames() {
		return parentGroupNames;
	}


	/**
	 * @return the copybookPref
	 */
	public String getCopybookPref() {
		return copybookPref;
	}


	//	/**
//	 * Create a new record
//	 * @param pRecordName name of the new record
//	 * @param fontName fontname to use
//	 *
//	 * @return the new record
//	 */
//	public static final ExternalRecord getNullRecord(final String pRecordName,
//			final String fontName) {
//
//	    return getNullRecord(pRecordName, Constants.rtRecordLayout, fontName);
//	}
//
//
//	/**
//	 * Create a new record
//	 * @param pRecordName name of the new record
//	 * @param recordType record type for the record
//	 * @param fontName fontname to use
//	 *
//	 * @return the new record
//	 */
//	public static final ExternalRecord getNullRecord(final String pRecordName,
//	        									final int recordType,
//	        									final String fontName) {
//
//	    return new ExternalRecord(-1, pRecordName, "", recordType, 0, "N",
//			"", "<Tab>", "", 0, Constants.DEFAULT_STRING, Constants.SYSTEM_EOL_BYTES, fontName, 0, -1, false);
//	}
	/**
	 * Get the initial record id (before any updates where made.
	 * It is used in the RecordEditor in the update SQL.
	 * @return Returns the initial RecordId.
	 */
	public int getInitRecordId() {
	    return initRecordId;
	}


	/**
	 * Get a sub-record (via its index)
	 * @param index sub record to retrieved
	 * @return requested sub record
	 */
	public xRecord getRecord(int index) {
	    return subRecords.get(index);
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

	public xRecord getRecord(String recordName) {
	    if (recordName == null || "".equals(recordName)) {
	    	throw new RuntimeException("Invalid Record Name: " + recordName);
	    }
	    
	    for (int i = subRecords.size() - 1; i >= 0; i--) {
	    	xRecord rec = subRecords.get(i);
			if (recordName.equalsIgnoreCase(rec.getRecordName())) {
	    		return rec;
	    	}
	    }
	    
	    StringBuilder b = new StringBuilder("Record Names: ");
	    for (int i = subRecords.size() - 1; i >= 0; i--) {
	    	b.append(subRecords.get(i).getRecordName()).append("; ");
	    }

	    throw new RuntimeException("No Record named \"" + recordName + "\" exists: " + b.toString());
	}


	/**
	 * Get the number of child (sub) records.
	 * @return number of subrecords
	 */
	public int getNumberOfRecords() {
	    return subRecords.size();
	}

	public void fieldUpdated(ExternalField field) {
		items = null;
	}
	
	/**
	 * Add a field definition
	 *
	 * @param o Field to add
	 * @return wether added correctly
	 */
	public boolean addRecordField(ExternalField o) {
		loadFields();
		fieldUpdated(o);
		o.setListner(this);
	    return fields.add(o);
	}
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.base.IAddDependingOn#addDependingOn(net.sf.JRecord.External.Def.DependingOn)
	 */
	@Override
	public void addDependingOn(DependingOn child) {
		dependingOn.add(child);
	}


	/**
	 * Add a list of fields
	 * @param flds fields to add
	 */
	public void addRecordFields(List<? extends ExternalField> flds) {
		if (flds != null && flds.size() > 0) {
			loadFields();
			fieldUpdated(null);
			fields.addAll(flds);
			
			for (ExternalField f : flds) {
				f.setListner(this);
			}
		}
	}
	
	/**
	 * Remove one field
	 * @param index index of the field to be removed
	 */
	public void removeRecordField(int index) {
		loadFields();
		fieldUpdated(null);
		fields.remove(index);
	}

	/**
	 * Get a field (via its index)
	 * @param index which field is required
	 * @return requested field
	 */
	public ExternalField getRecordField(int index) {
		loadFields();
	    return fields.get(index);
	}
	
	public ExternalField getRecordField(String name) {
		if (name != null) {
			loadFields();
			for (ExternalField f : fields) {
				if (name.equalsIgnoreCase(f.getName())) {
					return f;
				}
			}
			ExternalField f;
			for (xRecord r :subRecords) {
				f = r.getRecordField(name);
				if (f != null) { return f;}
			}
		}
		return null;
	}
	
	public int getfieldPosition(String name) {
		if (items != null && fields.size() == 0 && name != null && name.indexOf('(') < 0) {
			int pos = searchFieldPos(items, name);
			if (pos > 0) {
				return pos;
			}
		}
		
		ExternalField f = getRecordField(name);
		if (f == null) {
			return -1;
		}
		return f.getPos();
	}
	
	private int searchFieldPos(List<? extends IItemJrUpd> items, String name) {
		int pos;
		for (IItemJrUpd itm : items) {
			if (itm.getChildItems().size() > 0) {
				if ((pos = searchFieldPos(itm.getChildItems(), name)) > 0) {
					return pos;
				} 
			} else if (name.equalsIgnoreCase(itm.getFieldName())) {
				return itm.getPosition();
			}
		}
		return -1;
	}

//	/**
//	 * Insert a new field at a specific position
//	 * @param index index of the new Position
//	 * @param name Field name
//	 * @param type Field Type (e.g. <b>Type.ftChar</b>)
//	 * @param length Field Length
//	 * @param decimal number of decimal places
//	 * @return
//	 */
//	public ExternalField addField(int index, String name, int type, int length, int decimal) {
//		int pos = 1;
//		if (index > fields.size() || index < 0) { 
//			throw new RecordException("Invalid field index: " + index );
//		}
//		
//		ExternalField field, fld;
//		if (index < fields.size()) {
//			field = new ExternalField(fields.get(index).getPos(), length, name, "", type, decimal, 0, "", "", "", 0);
//			fields.add(index, field);
//			
//			for (int i = index + 1; i < fields.size(); i++) {
//				fld = fields.get(i);
//				int currPos = fld.getPos();
//				if (currPos > 0) {
//					fld.setPos(currPos + length);
//				}
//			}
//		} else {
//			if (index > 0) {
//				fld = fields.get(index - 1);
//				int lastlen = fld.getLen();
//				if (lastlen < 1) {
//					throw new RecordException("Field at index: " + (index-1) + " has an invalid length" );
//				}
//				int lastPos = fld.getPos();
//				if (lastPos > 0) {
//					pos = lastPos + lastlen;
//				}
//			}
//			field = new ExternalField(pos, length, name, "", type, decimal, 0, "", "", "", 0);
//		
//			fields.add(field);
//		}
//		
//		return field;
//	}

//	
//	public int addFixedWidthField(ExternalField fld, boolean adjPositions) {
//		int pos = fld.getPos();
//		int len = fld.getLen();
//		int idx = 0;
//		if (pos < 1 || len < 1) {
//			throw new RecordException("Invalif position/length: " + pos + "/" + len);
//		}
//		
//		this.loadFields();
//		
//		Collections.sort(fields, new Comparator<ExternalField>() {
//			@Override public int compare(ExternalField o1, ExternalField o2) {
//				return Integer.compare(o1.getPos(), o2.getPos());
//			}
//		});
//		
//		ExternalField lf = fields.get(0);
//		for (int i = 1; i < fields.size(); i++) {
//			if (fields.get(i).getPos() >= pos) {
//				int adj = len + pos - fields.get(i).getPos();
//				fields.add(idx, fld);
//				
//				if (adjPositions && adj > 0) {
//					ExternalField cf;
//					for (int j = idx+1; j < fields.size(); j++) {
//						cf = fields.get(j);
//						if (cf.getPos() > 0) {
//							cf.setPos(cf.getPos() + adj);
//						}
//					}
//				}
//				return i;
//			}
//			lf = fields.get(i);
//		}
//		
//		idx = fields.size();
//		fields.add(fld);
//		
//		return idx;
//	}
//	
//	private void insertField(int idx, ExternalField fld, int adj, boolean adjPositions ) {
//		fields.add(idx, fld);
//		
//		if (adjPositions && adj > 0) {
//			for (int j = idx+1; j < fields.size(); j++) {
//				
//			}
//		}
//	}
	
	
	/**
	 * Get the number of fields
	 *
	 * @return number of fields
	 */
	public int getNumberOfRecordFields() {
		loadFields();
	    return fields.size();
	}

	public void clearRecordFields() {
	    fields.clear();
	}

	/**
	 * Get a copy of all the fields
	 * @return Fields array
	 */
	public ExternalField[] getRecordFields() {
		loadFields();
	    return fields.toArray(new ExternalField[fields.size()]);
	}
	
	protected final void loadFields() {
		if (items != null && fields.size() == 0) {
			FieldCreatorHelper fldHelper = createFieldHelper();
			
			loadItems(
					fldHelper, 
					items,
					"",
					null, fldHelper.getInitialLevel(), 0);
		
			if (fldHelper.lastFiller != null 
			&& (fldHelper.lastFiller.getPos() + fldHelper.lastFiller.getLen() > fldHelper.lastEndPos)) {
				fields.add(fldHelper.lastFiller);
			}
		}
	}


	/**
	 * @return
	 */
	protected FieldCreatorHelper createFieldHelper() {
		FieldCreatorHelper fldHelper = new FieldCreatorHelper(
				ICobolSplitOptions.SPLIT_NONE, dialectCode, useJRecordNaming, copybookPref, fontName);
		fldHelper.setDropCopybookFromFieldNames(dropCopybookFromFieldNames);
		fldHelper.setParentGroup(parentGroupNames);
		return fldHelper;
	}
	
	private void loadItems(
			FieldCreatorHelper fldHelper, 
			List<? extends IItemJRecUpd> itms, String nameSuffix,
			DependingOnDtls dependOnParentDtls,
			int level,
			int basePos) {

		for (IItemJRecUpd itm  : itms) {
			if (itm.getLevelNumber() == 88) {
				
			} else {
				List<? extends IItemJRecUpd> childItems = itm.getChildItems();
				int numChildItems = childItems.size();
				String dependingVar = itm.getDependingOn();
				fldHelper.updateGroup(level, itm.getFieldName());
				
				if (itm.getOccurs() > 0) {
                    DependingOn dependOn = null;
                    
                    if (dependingVar != null && dependingVar.length() > 0) {
                    	dependOn = fldHelper
                    					.dependingOnBuilder()
                    						.setPosition(itm.getPosition() + basePos)
                    						.setLength(itm.getStorageLength())
                    						.setChildOccurs(itm.getOccurs())
                    					.newDependingOn(this, dependOnParentDtls, dependingVar);
                    }
					if (numChildItems == 0) {
						for (int i = 0; i < itm.getOccurs(); i++) {
							createField(
									fldHelper, level, itm, 
									fldHelper.updateFieldNameIndex(nameSuffix, i), // problem area
									createDependingOnDtls(dependOn, dependOnParentDtls, i),
									basePos + i * itm.getStorageLength());
						}						
					} else {
						for (int i = 0; i < itm.getOccurs(); i++) {
							loadItems(
									fldHelper, childItems, 
									fldHelper.updateFieldNameIndex(nameSuffix, i),
									createDependingOnDtls(dependOn, dependOnParentDtls, i),
									level+1,
									basePos + i * itm.getStorageLength());
						}
					}		
				} else if (itm.getOccurs() == 0) {
				} else if (numChildItems == 0) {
					createField(fldHelper, level, itm, nameSuffix, dependOnParentDtls, basePos);
				} else {
					loadItems(fldHelper, childItems, nameSuffix, dependOnParentDtls, level+1, basePos);
				}
			}
		}
	}

	private DependingOnDtls createDependingOnDtls(DependingOn dependOn, DependingOnDtls dependOnParentDtls, int idx) {
        DependingOnDtls dependOnDtls = dependOnParentDtls;
        if (dependOn != null) {
        	dependOnDtls = new DependingOnDtls(dependOn, idx, dependOnParentDtls);
        }
        return dependOnDtls;
	}

	private void createField(
			FieldCreatorHelper fieldHelper, int level, IItemJRecUpd itm, 
			String nameSuffix, DependingOnDtls dependOnParentDtls,
			int basePos) {
		
		String fieldName = itm.getFieldName();
		ExternalField fld = new ExternalField(
			itm.getPosition() + basePos, itm.getStorageLength(), 
			fieldHelper.createFieldName(fieldName, nameSuffix), 
			"", 
			itm.getType(), 
			fieldHelper.calculateDecimalSize(itm.getType(), itm.getPicture(), itm.getScale()),
			itm.getFormatId(), itm.getParameter(), "", itm.getFieldName(), 0,
			dependOnParentDtls);
		
	    if (level > 1 && fieldHelper.getGroupNameSize() > level) {
	       	fld.setGroup(fieldHelper.getGroupName(level - 1));
	    }

	    int fldEnd = fld.getPos() + fld.getLen();
		if (keepFillers || (fieldName != null && fieldName.length() > 0 && ! "filler".equalsIgnoreCase(fieldName))) {
	    	fields.add(fld);
	    	fieldHelper.lastEndPos = Math.max(fldEnd, fieldHelper.lastEndPos);
	    } else if (fieldHelper.lastFiller == null || (fieldHelper.lastFiller.getPos() + fieldHelper.lastFiller.getLen() < fldEnd)) {
	    	fieldHelper.lastFiller = fld;
	    }
	}

	
	/**
	 * Get the System Name of the system this record belongs to
	 * @return System Name
	 * @deprecated Was originally Used in the RecordEditor; it serves not purpose in JRecord. 
	 */
	public String getSystemName() {
		return systemName;
	}

	/**
	 * Set the System Name
	 * @param newSystemName new System Name
	 * 
	 * @deprecated Was originally Used in the RecordEditor; it serves not purpose in JRecord. 
	 */
	public void setSystemName(String newSystemName) {
		this.systemName = newSystemName;
	}

	/**
	 * Get the Field that should be tested to determine if this is the valid
	 * Sub-Record for the current line.
	 *
	 * @return the tstField
	 *
	 * @deprecated Use getTstFields
	 */ @Deprecated
	public String getTstField() {

		ExternalFieldSelection f = getFirstSelection(recSelect);
		if (f == null) {
			return null;
		} else {
			return f.getFieldName();
        }
	}

	/**
	 * Set the Field / value that should be tested to determine if this is the valid
	 * Sub-Record for the current line.
	 *
	 * @param tstField the tstField to set
	 * @param value Value to compare field to
	 *
	 *  @deprecated  use addTstField
	 */ @Deprecated
	public void setTstField(String tstField, String value) {

		recSelect = new ExternalFieldSelection(tstField, value);
	}

	/**
	 * Add a Field/Value that should be tested to determine if this is the valid
	 * Sub-Record for the current line.
	 *
	 * @param tstField the tstField to set
	 * @param value Value to compare field to
	 */
	public void addTstField(String tstField, String value) {
		addTstField(tstField, ExternalFieldSelection.EQUALS_OPERATOR, value);
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public void addTstField(String tstField, String op, String value) {

		if (recSelect == null) {
			recSelect = new ExternalGroupSelection(1);
		}	
		if (recSelect instanceof ExternalGroupSelection) {
			ExternalGroupSelection g = (ExternalGroupSelection) recSelect;
			g.add(new ExternalFieldSelection(tstField, value, op));
			return;
		}
		//System.out.println();
		//System.out.println("-->" + recSelect);
		//System.out.println("-->" + recSelect.getClass().getName());
		throw new RuntimeException("Can not add Test Field");
	}
	
	/**
	 * Get the value the TestField should be compared to
	 *
	 * @return the tstFieldValue
	 * @deprecated Use getTstFields
	 */
	public String getTstFieldValue() {
		ExternalFieldSelection f = getFirstSelection(recSelect);
		if (f == null) {
			return null;
		} else {
			return f.getFieldValue();
		}
	}


	 public int getTstFieldCount() {
		 int ret = 0;
		 if (recSelect != null) {
			 ret = recSelect.getElementCount();
		 }

		 return ret;
	 }

	 @SuppressWarnings("rawtypes")
	 private ExternalFieldSelection getFirstSelection(ExternalSelection s) {

		 if (s == null) {
			 return null;
		 } else if (s instanceof ExternalFieldSelection) {
			 return (ExternalFieldSelection) s;
		 } else {
			 ExternalGroupSelection g = (ExternalGroupSelection) s;
			 ExternalFieldSelection fs = null;

			 for (int i = 0; i < g.size() && fs == null; i++) {
				 fs = getFirstSelection(g.get(i));
			 }

			 return fs;
		 }
	 }

//	/**
//	 * @return the tstFields
//	 */
//	public final List<TstField> getTstFields() {
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
	 * Get the parent Record of this record
	 * @return the parent Record
	 */
	public int getParentRecord() {
		return parentRecord;
	}

	/**
	 * Set the parent record
	 *
	 * @param parentRecord the parentRecord to set
	 */
	public void setParentRecord(int parentRecord) {
		this.parentRecord = parentRecord;
	}

	/**
	 *
	 * @see net.sf.JRecord.External.Def.AbstractUpdatableRecord#setNew(boolean)
	 */
	@Override
	public void setNew(boolean isNew) {
		super.setNew(isNew);

		if (isNew) {
			setChildrenNew(true);
		}
	}

	/**
	 * Set New status of all Child records
	 * @param isNew wether they are new records or not
	 */
	public final void setChildrenNew(boolean isNew) {
		for (int i = 0; i < subRecords.size(); i++) {
			subRecords.get(i).setNew(isNew);
		}
	}

	/**
	 * This method drops filler fields (apart from the last one). In Cobol
	 * filler denote an unused block of storage. Filler fields can not be accessed
	 * in Cobol, it generally makes sense to remove them.
	 */
	public final void dropFiller() {
		ArrayList<ExternalField> tmpFields;
		int maxPos = 0;
		int count = 1;
		int i, endPos;
		ExternalField fld;
		
		loadFields();

		if (fields != null) {
			loadFields();
			for (i = 0; i < fields.size(); i++) {
				if (! isFiller(fields.get(i).getName())) {
					count += 1;
				}

				maxPos = Math.max(maxPos, fields.get(i).getPos() + fields.get(i).getLen());
			}

			tmpFields = new ArrayList<ExternalField>(count + 1);

			for (i = 0; i < fields.size(); i++) {
				fld = fields.get(i);
				endPos = fld.getPos() + fld.getLen();

				if ((endPos == maxPos) || (! isFiller(fld.getName()))) {
					tmpFields.add(fld);
				}
			}
			fields = tmpFields;
		}

		if (subRecords != null) {
			for (i = 0; i < subRecords.size(); i++) {
				subRecords.get(i).dropFiller();
			}
		}
	}

	private boolean isFiller(String s) {
		return s == null || "".equals(s.trim()) 
			  || "filler".equalsIgnoreCase(s)
			  || s.toLowerCase().startsWith("filler (");
	}

	/**
	 * Set the parent value from the parent name
	 */
	public void setParentsFromName() {
		int i,j;
		String parent;

		if (subRecords != null) {
			for (i = 0; i < subRecords.size(); i++) {
				parent = ((BaseExternalRecord<xRecord>)subRecords.get(i)).parentName; // getParentName();
				if (parent != null && ! "".equalsIgnoreCase(parent)) {
					for (j = 0; j < subRecords.size(); j++) {
//						System.out.println("~~>> " + i + ", " + j
//								+ " >" + parent + "< >"
//								+ subRecords.get(j).getRecordName() + "<");
						if (parent.equalsIgnoreCase(subRecords.get(j).getRecordName())) {
							subRecords.get(i).setParentRecord(j);
							subRecords.get(i).setParentName(null);
							break;
						}
					}
				}
			}
		}

	}

	/**
	 * Used in interface to convert back to ExternalRecord
	 * @return this ExternalRecord
	 */
	public final xRecord asExternalRecord() {
		return self;
	}
	
//	/**
//	 * For internal use - Get parent record name (may not be set)
//	 * @return parent name
//	 */
//	private String getParentName() {
//		return parentName;
//	}

	/**
	 * for internal use only - Set parent record name
	 * @param tmpParentName new parent name
	 */
	public final void setParentName(String tmpParentName) {
		this.parentName = tmpParentName;
	}

	/**
	 * @return the dependingOn
	 */
	public final DependingOnDefinition getDependingOnDefinition() {

		if (dependingOnDef == null) {
			dependingOnDef = new DependingOnDefinition(dependingOn);
		}
		return dependingOnDef;
	}
	
	public final void newDependingOn() {
		dependingOn = new ArrayList<DependingOn>();
		dependingOnDef = null;
	}
	

	/**
	 * @return the lineNumberOfFieldNames
	 */
	public int getLineNumberOfFieldNames() {
		return lineNumberOfFieldNames;
	}

	/**
	 * @param lineNumberOfFieldNames the lineNumberOfFieldNames to set
	 */
	public void setLineNumberOfFieldNames(int lineNumberOfFieldNames) {
		this.lineNumberOfFieldNames = lineNumberOfFieldNames;
	}

	/**
	 * @return the recordLength
	 */
	public final int getRecordLength() {
		return recordLength;
	}

	/**
	 * @param recordLength the recordLength to set
	 */
	public final void setRecordLength(int recordLength) {
		this.recordLength = recordLength;
	}

	/**
	 * @param optimizeTypes the optimizeTypes to set
	 */
	public void setOptimizeTypes(boolean optimizeTypes) {
		this.optimizeTypes = optimizeTypes;
	}


	/**
	 * @return the defaultRecord
	 */
	public boolean isDefaultRecord() {
		return defaultRecord;
	}

	public boolean isBinary() {
		return checkBinary(this);
	}
	
	public boolean isCsv() {
		if (subRecords.size() == 0) {
			return isCsv(this);
		} else {
			for (int i = 0; i < subRecords.size(); i++) {
				if (isCsv(subRecords.get(i))) {
					return true;
				}
			}
		}
		return false;
	}
	
	private boolean isCsv(BaseExternalRecord<xRecord> r) {
		return r.recordType == Constants.rtDelimited || r.recordType == Constants.rtDelimitedAndQuote;
	}
	
	private boolean checkBinary(BaseExternalRecord<xRecord> rec) {
		if (rec.fields.size() == 0 && rec.items != null && rec.items.size() > 0) {
			if (checkBinary(rec.items)) {
				return true;
			}
		} 

		for (ExternalField f : rec.fields ) {
			if (TypeManager.isBinary(f.getType())) {
				return true;
			}
		}
		
		
		for (xRecord r : rec.subRecords) {
			if (checkBinary(r)) {
				return true;
			}
		}
		return false;
	}
	
	private boolean checkBinary(List<? extends IItemJrUpd> itms) {
		for (IItemJrUpd item : itms) {
			if (item.getChildItems().size() == 0) {
				if (TypeManager.isBinary(item.getType())) { 
					return true; 
				}
			} else if (checkBinary(item.getChildItems())) { 
				return true; 
			}
		}
		
		return false;
	}
	
	/**
	 * @param defaultRecord the defaultRecord to set
	 */
	public void setDefaultRecord(boolean defaultRecord) {
		this.defaultRecord = defaultRecord;
	}

	/**
	 * @return the recSelect
	 */
	public ExternalSelection getRecordSelection() {
		return recSelect;
	}

	/**
	 * @param recSelect the recSelect to set
	 */
	public void setRecordSelection(ExternalSelection recSelect) {
		this.recSelect = StreamLine.getExternalStreamLine().streamLine(recSelect);
	}

	public String getLineFormatParam() {
		return lineFormatParam;
	}


	public void setLineFormatParam(String lineFormatParam) {
		this.lineFormatParam = lineFormatParam;
	}


	/**
	 * @return the embeddedCr
	 */
	public boolean isEmbeddedCr() {
		return embeddedCr;
	}

	/**
	 * @param embeddedCr the embeddedCr to set
	 */
	public void setEmbeddedCr(boolean embeddedCr) {
		this.embeddedCr = embeddedCr;
	}


	/**
	 * @return the recordOption
	 */
	public final IRecordPositionOption getRecordPositionOption() {
		return recordPosistionOption;
	}

	/**
	 * @param recordOption the recordOption to set
	 */
	public final void setRecordPositionOption(IRecordPositionOption recordOption) {
		this.recordPosistionOption = recordOption;
	}

	/**
	 * @return the initToSpaces
	 */
	public final boolean isInitToSpaces() {
		return initToSpaces;
	}

	/**
	 * @param initToSpaces the initToSpaces to set
	 */
	public final void setInitToSpaces(boolean initToSpaces) {
		this.initToSpaces = initToSpaces;
	}

	/**
	 * @return the cb2xmlDocuments details
	 */
	public final List<Cb2xmlDocument> getCb2xmlDocuments() {
		return cb2xmlDocuments;
	}

	/**
	 * @param e cb2xml document details
	 * @see java.util.ArrayList#add(java.lang.Object)
	 */
	public void addCb2xmlDocument(Cb2xmlDocument e) {
		cb2xmlDocuments.add(e);
	}

	/**
	 * @param c cb2xmlDoc collection to add.
	 * @return whether added successfully 
	 * @see java.util.ArrayList#addAll(java.util.Collection)
	 */
	public boolean addAllCb2xmlDocuments(Collection<Cb2xmlDocument> c) {
		return cb2xmlDocuments.addAll(c);
	}

	/**
	 * create a Position / length array
	 * @return Position / length
	 */
	protected int[][] getPosLength() {
		loadFields();
		int[][] ret = new int[2][];
		ret[POSITION_IDX] = new int[fields.size()];
		ret[LENGTH_IDX] = new int[fields.size()];
		
		if (recordType == Constants.rtDelimited || recordType == Constants.rtDelimitedAndQuote) {
			int lastPos = 0;

			for (int i = 0; i < fields.size(); i++) {
				int pos = fields.get(i).getPos();
				if (pos <= 0) {
					lastPos += 1;
				} else {
					lastPos = pos;
				}
				
				ret[LENGTH_IDX][i] = Constants.NULL_INTEGER;
				ret[POSITION_IDX][i] = lastPos;
			}
		} else {
			int lastPos = 0;
			int lastLen = 1;

			for (int i = 0; i < fields.size(); i++) {
				ExternalField fld = fields.get(i);
				int pos = fld.getPos();
				if (pos <= 0) {
					if (lastLen <= 0) {
						throw new RuntimeException("Error Field: " + i + " " + fld.getName()
								+": Can not calculate position");
					}
					lastPos += lastLen;
				} else {
					if (lastLen <= 0 && i > 0) {
						ret[LENGTH_IDX][i-1] = pos - lastPos;
					}
					lastPos = pos;
				}
				
				lastLen = fld.getLen();
				ret[LENGTH_IDX][i] = lastLen;
				ret[POSITION_IDX][i] = lastPos;
			}
			if (lastLen < 0) {
				ret[LENGTH_IDX][ret[LENGTH_IDX].length - 1] = 1;
			}
		}
		
		return ret;
	}
}
