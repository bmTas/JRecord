/*
 * This class holds a Record Layout (ie it describes one line in
 * the file).
 *
 * Changes
 * # Version 0.56 Bruce Martin 2007/01/16
 *   - remove unused field editorStatus
 */
package net.sf.JRecord.Details;

import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.Common.AbstractRecordX;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.detailsSelection.FieldSelectX;




/**
 * This class holds a Record Description. A <b>Record</b> consists of
 * one ore more <b>Fields</b> (Class FieldDetail). The class is used by
 * {@link LayoutDetail}. Each LayoutDetail holds one or more RecordDetail's
 *
 * <pre>
 *     LayoutDetail  - Describes a file
 *       |
 *       +----- RecordDetail (1 or More) - Describes one record in the file
 *                |
 *                +------  FieldDetail (1 or More)  - Describes one field in the file
 * </pre>
 *
 * @param pRecordName  Record Name
 * @param pSelectionField Selection Field
 * @param pSelectionValue Selection Value
 * @param pRecordType Record Type
 * @param pDelim Record Delimiter
 * @param pQuote String Quote (for Comma / Tab Delimeted files)
 * @param pFontName fontname to be used
 * @param pFields Fields belonging to the record
 *
 *
 * @author Bruce Martin
 * @version 0.55
 */
public class RecordDetail implements AbstractRecordX<FieldDetail> {

    //private static final int STATUS_EXISTS         =  1;

	private String recordName;

	private int fieldCount;
	private FieldDetail[] fields;

	//private FieldDetail selectionFld = null;
	//private int    selectionFieldIdx = Constants.NULL_INTEGER;
	private int    recordType;

	//private String selectionField;
	//private String selectionValue;
	private RecordSelection recordSelection = new RecordSelection(this);

	private String delimiter;
	private int    length = 0;
	private String fontName;
	private String quote;

	private int    recordStyle;

	private int parentRecordIndex = Constants.NULL_INTEGER;

	private int sourceIndex = 0;

	private int numberOfFieldsAdded = 0;
	//private int editorStatus = STATUS_UNKOWN;


	/**
	 * Create a Record
	 *
	 * @param pRecordName  Record Name
	 * @param pSelectionField Selection Field
	 * @param pSelectionValue Selection Value
	 * @param pRecordType Record Type
	 * @param pDelim Record Delimiter
	 * @param pQuote String Quote (for Comma / Tab Delimeted files)
	 * @param pFontName fontname to be used
	 * @param pFields Fields belonging to the record
	 */
	public RecordDetail(final String pRecordName,
	        			final String pSelectionField,
	        			final String pSelectionValue,
						final int pRecordType,
						final String pDelim,
						final String pQuote,
						final String pFontName,
						final FieldDetail[] pFields,
						final int pRecordStyle
						) {
		this(pRecordName, pRecordType, pDelim,
			 pQuote, pFontName, pFields, pRecordStyle);

		if (!"".equals(pSelectionField)) {
			recordSelection.setRecSel(FieldSelectX.get(pSelectionField, pSelectionValue, "=", getField(pSelectionField)));
		}
	}

	/**
	 * Create a Record
	 *
	 * @param pRecordName  Record Name
	 * @param pRecordType Record Type
	 * @param pDelim Record Delimiter
	 * @param pQuote String Quote (for Comma / Tab Delimeted files)
	 * @param pFontName fontname to be used
	 * @param pFields Fields belonging to the record
	 * @param pRecordStyle Record Style
	 * @param selection RecordSelection
	 */
	public RecordDetail(final String pRecordName,
						final int pRecordType,
						final String pDelim,
						final String pQuote,
						final String pFontName,
						final FieldDetail[] pFields,
						final int pRecordStyle,
						final RecordSelection selection
						) {
		this(pRecordName, pRecordType, pDelim,
			 pQuote, pFontName, pFields, pRecordStyle);

		recordSelection = selection;
	}

	/**
	 * Create a Record
	 *
	 * @param pRecordName  Record Name
	 * @param pRecordType Record Type
	 * @param pDelim Record Delimiter
	 * @param pQuote String Quote (for Comma / Tab Delimited files)
	 * @param pFontName font name to be used
	 * @param pFields Fields belonging to the record
	 * @param pRecordStyle Record Style
	 */
	public RecordDetail(final String pRecordName,
						final int pRecordType,
						final String pDelim,
						final String pQuote,
						final String pFontName,
						final FieldDetail[] pFields,
						final int pRecordStyle
						) {
		super();

		int j, l;
		this.recordName = pRecordName;
		this.recordType = pRecordType;

		this.fields   = pFields;
		this.quote    = pQuote;
		this.fontName = pFontName;
		this.recordStyle = pRecordStyle;

		this.fieldCount = pFields.length;
		while (fieldCount > 0 && fields[fieldCount - 1] == null) {
		    fieldCount -= 1;
		}

		delimiter = convertFieldDelim(pDelim);

		//System.out.println("Quote 1 ==>" + pQuote + "<==");
		for (j = 0; j < fieldCount; j++) {
		    pFields[j].setRecord(this);
		    l = pFields[j].getPos() + pFields[j].getLen() - 1;
		    if (pFields[j].getLen() >= 0 && length < l) {
		    	length = l;
		    }
		}
	}

//	/**
//	 * if it is null then return "" else return s
//	 *
//	 * @param str string to test
//	 *
//	 * @return Corrected string
//	 */
//	private String correct(String str) {
//		if (str == null) {
//			return "";
//		}
//		return str;
//	}


	/**
	 * Add a record to the layout
	 * @param field new field
	 */
	public void addField(FieldDetail field) {

	    if (fieldCount >= fields.length) {
	        FieldDetail[] temp = fields;
	        fields = new FieldDetail[fieldCount + 5];
	        System.arraycopy(temp, 0, fields, 0, temp.length);
	        fieldCount = temp.length;
	    }
	    field.setRecord(this);
	    fields[fieldCount] = field;
	    fieldCount += 1;
	    numberOfFieldsAdded += 1;
	}


	/**
	 * Get the Record Name
	 *
	 * @return Record Name
	 */
	public String getRecordName() {
		return recordName;
	}



	/**
	 * This method returns the Selection Field Index. This is the index of
	 * the field which is used to determine which record layout to use to
	 * display a line
	 *
	 * @return Selection Field Index
	 *
	 * @deprecated use getSelectionField
	 */
	public int getSelectionFieldIdx() {
		 if (recordSelection.getElementCount() <= 0) {
			 return Constants.NULL_INTEGER;
		 }
		 return getFieldIndex(recordSelection.getFirstField().getFieldName());
	}


	/**
	 * Get Selection Field
	 * @return Seelction field
	 *
	 */ @Deprecated
	public final IFieldDetail getSelectionField() {
		 if (recordSelection.getElementCount() <= 0) {
			 return null;
		 }

		 return recordSelection.getFirstField().getFieldDetail();
	}


	/**
	 * @param newSelectionField the selectionFld to set
	 */
	public final void setSelectionField(FieldDetail newSelectionField) {

		 recordSelection.setRecSel(
				 FieldSelectX.get(newSelectionField.getName(), getSelectionValue(), "=", newSelectionField));
	}


	/**
	 * This method returns a value to be compared with the selection field.
	 * If the Selection field is equals this value, it is assumed that this
	 * record Layout should be used to display the line
	 *
	 * @return Selection Value
	 */
	public String getSelectionValue() {
		 if (recordSelection.size() <= 0) {
			 return null;
		 }
		 return recordSelection.getFirstField().getFieldValue();
	}


	/**
	 * Get the Record Type
	 *
	 * @return Record - Return the record layout
	 */
	public int getRecordType() {
		return recordType;
	}


	/**
	 * Gets the records length
	 *
	 * @return the records length
	 */
	public int getLength() {
		return length;
	}


	/**
	 * Get the font name to use when viewing string fields
	 *
	 * @return font Name
	 */
    public String getFontName() {
        return fontName;
    }


    /**
     * wether it is a binary field or not
     *
     * @param fldNum fldNum Field Number
     *
     * @return wether it is a binary field
     */
    public boolean isBinary(int fldNum) {
        return TypeManager.getSystemTypeManager()
        		.getType(fields[fldNum].getType()).isBinary();
    }

    /**
     * Check if field is numeric
     * @param fldNum field to check
     * @return wether it is numeric or not
     */
    public boolean isNumericField(int fldNum) {
        return TypeManager.getSystemTypeManager()
        		.getType(fields[fldNum].getType()).isNumeric();
    }



    /**
     * Get the Index of a specific record (base on name)
     *
     * @param fieldName record name being searched for
     *
     * @return index of the record
     */
    public int getFieldIndex(String fieldName) {
        int ret = Constants.NULL_INTEGER;
        int i;

        if (fieldName != null) {
            for (i = 0; i < fieldCount; i++) {
                if (fieldName.equalsIgnoreCase(fields[i].getName())) {
                    ret = i;
                    break;
                }
            }
        }
        return ret;
    }


    /**
     * Get a specific field definition
     * @param idx index of the required field
     * @return requested field
     */
    public FieldDetail getField(int idx) {
        return this.fields[idx];
    }

    /**
     * Get the numeric Type of field
     * @param idx field index
     * @return numeric type (if it is numeric)
     */
    public int getFieldsNumericType(int idx) {
        return TypeManager.getSystemTypeManager()
        		.getType(this.fields[idx].getType())
        		.getFieldType();
    }

    /**
     * Get a specific field definition (using the field name)
     *
     * @param fieldName record name being searched for
     *
     * @return index of the record
     */
    public FieldDetail getField(String fieldName) {
        FieldDetail ret = null;
        int idx = getFieldIndex(fieldName);

        if (idx >= 0) {
            ret = fields[idx];
        }

        return ret;
    }


    /**
     * get the number of fields in the record
     *
     * @return the number of fields in the record
     */
    public int getFieldCount() {
        return fieldCount;
    }


    /**
     * Get the maximum width of the fields (a value < 0 means
     * use the default width).
     *
     * @return Returns the widths.
     */
    public int[] getWidths() {
        return null;
    }


    /**
     * Get the Field Delimiter (ie Tab / Comma etc in CSV files)
     *
     * @return Returns the delimiter.
     */
    public String getDelimiter() {
        return delimiter;
    }

    protected void setDelimiter(String delimiter) {
		this.delimiter = delimiter;
	}


	/**
	 * @see net.sf.JRecord.Common.AbstractRecord#getQuote()
	 */
    public String getQuote() {
        return quote;
    }


	/**
	 * @see net.sf.JRecord.Common.AbstractRecord#getParentRecordIndex()
	 */
//	public int getParentRecordIndex() {
//		return parentRecordIndex;
//	}


	/**
	 * @param parentRecordIndex the parentRecordIndex to set
	 */
//	public void setParentRecordIndex(int parentRecordIndex) {
//		this.parentRecordIndex = parentRecordIndex;
//	}


	/**
	 * @see net.sf.JRecord.Common.AbstractRecord#getRecordStyle()
	 */
	public int getRecordStyle() {
		return recordStyle;
	}


	/**
	 * @return the sourceIndex
	 *
	 *  @see net.sf.JRecord.Common.AbstractRecord#getSourceIndex()
	 */
	public int getSourceIndex() {
		return sourceIndex;
	}


	/**
	 * @param sourceIndex the sourceIndex to set
	 */
	public void setSourceIndex(int sourceIndex) {
		this.sourceIndex = sourceIndex;
	}


	/**
	 * @return the numberOfFieldsAdded
	 */
	protected final int getNumberOfFieldsAdded() {
		return numberOfFieldsAdded;
	}


	/**
	 * @param numberOfFieldsAdded the numberOfFieldsAdded to set
	 */
	protected final void setNumberOfFieldsAdded(int numberOfFieldsAdded) {
		this.numberOfFieldsAdded = numberOfFieldsAdded;
	}

	public final static String convertFieldDelim(String pDelim) {
		String delimiter = pDelim;
		if ((pDelim == null) || (pDelim.trim().equalsIgnoreCase("<tab>"))) {
			delimiter = "\t";
		} else if (pDelim.trim().equalsIgnoreCase("<space>")) {
			delimiter = " ";
		}
		return delimiter;
	}

	/**
	 * @return the recordSelection
	 */
	public RecordSelection getRecordSelection() {
		return recordSelection;
	}

	/**
	 * @return the parentRecordIndex
	 */
	public int getParentRecordIndex() {
		return parentRecordIndex;
	}

	/**
	 * @param parentRecordIndex the parentRecordIndex to set
	 */
	public void setParentRecordIndex(int parentRecordIndex) {
		this.parentRecordIndex = parentRecordIndex;
	}

	public final List<FieldDetail> getFields(String fieldName, String... levelNames) {

		ArrayList<FieldDetail> ret = new ArrayList<FieldDetail>();
		boolean ok;
		String groupName;
		int numLevelNames = levelNames == null ? 0 : levelNames.length;
		ArrayList<String> ln = new ArrayList<String>(numLevelNames);

		for (int i = 0; i < numLevelNames; i++) {
			if (levelNames[i] != null && ! "".equals(levelNames[i])) {
				ln.add("." + levelNames[i].toUpperCase() + ".");
			}
		}

		for (FieldDetail f : fields) {
			if (f.getName().equals(fieldName)) {
				groupName = f.getGroupName().toUpperCase();
				ok = true;
				for (String n : ln) {
					if (groupName.indexOf(n) < 0) {
						ok = false;
						break;
					}
				}

				if (ok) {
					ret.add(f);
				}
			}
		}

		return ret;
	}

	public final FieldDetail getUniqueField(String fieldName, String... levelNames) {
		List<FieldDetail> flds = getFields(fieldName, levelNames);

		switch (flds.size()) {
		case 0: throw new RuntimeException("No Field Found");
		case 1: return flds.get(0);
		}

		throw new RuntimeException("Found " + flds.size() + " fields; should be only one");
	}
}
