package net.sf.JRecord.External;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.Def.AbstractUpdatableRecord;
import net.sf.JRecord.External.Def.DependingOn;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalGroupSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.ExternalRecordSelection.StreamLine;
import net.sf.JRecord.Option.IRecordPositionOption;
import net.sf.JRecord.Types.TypeManager;

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
public class ExternalRecord extends AbstractUpdatableRecord 
implements ICsvSchemaBuilder, IFixedWidthSchemaBuilder {

  private int recordId;
  private int initRecordId; 
  private String recordName;
  private String description;
  private int recordType;
  private int system;
  private String systemName = null;
  private String listChar;
  private String copyBook;
  private String delimiter;
  private String quote;
  private int posRecInd;
  private String recSepList;
  private byte[] recordSep;
  private String fontName;
  private int recordStyle;
  private int fileStructure;
  private int lineNumberOfFieldNames = 1;

  private ExternalSelection recSelect;
  private IRecordPositionOption recordPosistionOption = null;
  //private ArrayList<TstField> tstFields = null;
  private boolean defaultRecord = false;
  private boolean embeddedCr    = false;  //private String tstField = "";
  private boolean initToSpaces  = false;  // for backward compatibility
  //private String tstFieldValue = "";



  private int parentRecord = -1;

  private String parentName = null;


  private ArrayList<ExternalRecord> subRecords = new ArrayList<ExternalRecord>();
  private ArrayList<ExternalField> fields = new ArrayList<ExternalField>();
  private final ArrayList<DependingOn> dependingOn = new ArrayList<DependingOn>(3);

  private int lastPosition = -1;
  
  private ArrayList<Cb2xmlDocument> cb2xmlDocuments = new ArrayList<Cb2xmlDocument>();


  /**
   * Create External Record Definition
   *
   */
  public ExternalRecord () {
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
  public ExternalRecord fullClone() {

      ExternalRecord ret;

      try {
          ret = (ExternalRecord) super.clone();
      } catch (Exception e) {
          ret = new ExternalRecord(
                  recordId
                  , recordName
                  , description
                  , recordType
                  , system
                  , listChar
                  , copyBook
                  , delimiter
                  , quote
                  , posRecInd
                  , recSepList
                  , recordSep
                  , fontName
                  , recordStyle
                  , fileStructure
                  , false
          );
      }
      return ret;
  }




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
  public ExternalRecord setRecordType(int val) {

      if ((val != recordType) || (updateStatus == NULL_INT_VALUE)) {
           recordType = val;
           updateStatus = UPDATED;
      }
      return this;
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
  public void setFontName(String val) {

      if ((val == null || "".equals(val))
      && (fontName == null || "".equals(fontName))) {
          return;
      }

      if ((val == null) || (! val.equals(fontName)) || (updateStatus == NULL_INT_VALUE)) {
           fontName = val;
           updateStatus = UPDATED;
      }
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
  public ExternalRecord setRecordStyle(int val) {

      if ((val != recordStyle) || (updateStatus == NULL_INT_VALUE)) {
           recordStyle = val;
           updateStatus = UPDATED;
      }
      if (subRecords != null && subRecords.size() > 0) {
    	  for (ExternalRecord r : subRecords) {
    		  r.setRecordStyle(val);
    	  }
      }
      return this;
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
  public ExternalRecord setFileStructure(int val) {

      if ((val != fileStructure) || (updateStatus == NULL_INT_VALUE)) {
           fileStructure = val;
           updateStatus = UPDATED;
      }
      
      return this;
  }


  /**
   * Add a sub-record to this record
   * @param o record to add
   * @return wether added correctly
   */
  public boolean addRecord(ExternalRecord o) {
      return subRecords.add(o);
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
	public ExternalRecord getRecord(int index) {
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

	public ExternalRecord getRecord(String recordName) {
	    if (recordName == null || "".equals(recordName)) {
	    	throw new RuntimeException("Invalid Record Name: " + recordName);
	    }
	    
	    for (int i = subRecords.size() - 1; i >= 0; i--) {
	    	ExternalRecord rec = subRecords.get(i);
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

	/**
	 * Get a copy of all subrecords.
	 *
	 * @return Sub records
	 */
	public ExternalRecord[] toArray() {
	    return subRecords.toArray(new ExternalRecord[subRecords.size()]);
	}

	/**
	 * Add a field definition
	 *
	 * @param o Field to add
	 * @return wether added correctly
	 */
	public boolean addRecordField(ExternalField o) {
	    return fields.add(o);
	}
	
	public void addDependingOn(DependingOn child) {
		DependingOn.addChild(dependingOn, child);
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
		return null;
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


	/**
	 * Get a field (via its index)
	 * @param index which field is required
	 * @return requested field
	 */
	public ExternalField getRecordField(int index) {
	    return fields.get(index);
	}


	/**
	 * Get the number of fields
	 *
	 * @return number of fields
	 */
	public int getNumberOfRecordFields() {
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
	    return fields.toArray(new ExternalField[fields.size()]);
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

		if (fields != null) {
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
				parent = subRecords.get(i).parentName; // getParentName();
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
	 * Convert to internal format
	 * @return the internal LayoutDetail equivalent
	 *
	 */
	public final LayoutDetail asLayoutDetail() {
		return ToLayoutDetail.getInstance().getLayout(this);
	}

	/**
	 * Used in interface to convert back to ExternalRecord
	 * @return this ExternalRecord
	 */
	public final ExternalRecord asExternalRecord() {
		return this;
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
	public final ArrayList<DependingOn> getDependingOn() {
		return dependingOn;
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
	 * @return the defaultRecord
	 */
	public boolean isDefaultRecord() {
		return defaultRecord;
	}

	public boolean isBinary() {
		return checkBinary(this);
	}
	
	private boolean checkBinary(ExternalRecord rec) {
		for (ExternalField f : rec.fields ) {
			if (TypeManager.isBinary(f.getType())) {
				return true;
			}
		}
		
		for (ExternalRecord r : rec.subRecords) {
			if (checkBinary(r)) {
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
