package net.sf.JRecord.External;

import java.util.ArrayList;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.Def.AbstractUpdatableRecord;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalGroupSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.ExternalRecordSelection.StreamLine;

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
public class ExternalRecord extends AbstractUpdatableRecord {

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
  //private ArrayList<TstField> tstFields = null;
  private boolean defaultRecord = false;
  private boolean embeddedCr    = false;  //private String tstField = "";
  //private String tstFieldValue = "";



  private int parentRecord = -1;

  private String parentName = null;


  private ArrayList<ExternalRecord> subRecords = new ArrayList<ExternalRecord>();
  private ArrayList<ExternalField> fields = new ArrayList<ExternalField>();


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
  public void setRecordType(int val) {

      if ((val != recordType) || (updateStatus == NULL_INT_VALUE)) {
           recordType = val;
           updateStatus = UPDATED;
      }
  }

  /**
   *  This method gets the vaule of System
   * @return system identifier
   */
  public int getSystem() {
      return system;
  }

  /**
   *  This method sets the vaule of System
   *
   * @param val value to be assigned to System
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
   * This method gets the vaule of RecSepList
   * @return record Seperator list value
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
  public void setRecordStyle(int val) {

      if ((val != recordStyle) || (updateStatus == NULL_INT_VALUE)) {
           recordStyle = val;
           updateStatus = UPDATED;
      }
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
  public void setFileStructure(int val) {

      if ((val != fileStructure) || (updateStatus == NULL_INT_VALUE)) {
           fileStructure = val;
           updateStatus = UPDATED;
      }
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
			"", "<Tab>", "", 0, Constants.DEFAULT_STRING, Constants.SYSTEM_EOL_BYTES, fontName, 0, -1);
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
	    ExternalField[] r = new ExternalField[fields.size()];
	    fields.toArray(r);
	    return r;
	}

	/**
	 * Get the System Name of the system this record belongs to
	 * @return System Name
	 */
	public String getSystemName() {
		return systemName;
	}

	/**
	 * Set the System Name
	 * @param newSystemName new System Name
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
	 * @Deprecated Use getTstFields
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
	 *  @Deprecated  use addTstField
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

		if (recSelect == null) {
			recSelect = new ExternalGroupSelection(1);
		}
		if (recSelect instanceof ExternalGroupSelection) {
			ExternalGroupSelection g = (ExternalGroupSelection) recSelect;
			g.add(new ExternalFieldSelection(tstField, value));
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
	 * @Deprecated Use getTstFields
	 */@Deprecated
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
			tmpFields = new ArrayList<ExternalField>(count);

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
		return s == null || "".equals(s.trim()) || "filler".equalsIgnoreCase(s);
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
	 * @throws RecordException any error that occurs
	 */
	public final LayoutDetail asLayoutDetail()
	throws RecordException {
		return ToLayoutDetail.getInstance().getLayout(this);
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


}
