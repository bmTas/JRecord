/*
 * This class holds a Record Layout (ie it describes one line in
 * the file).
 *
 * Changes
 * # Version 0.56 Bruce Martin 2007/01/16
 *   - remove unused field editorStatus
 */
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

package net.sf.JRecord.Details;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.sf.JRecord.Common.AbstractIndexedLine;
import net.sf.JRecord.Common.AbstractRecordX;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.IGetFieldByName;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.CsvParser.BasicCsvLineParser;
import net.sf.JRecord.CsvParser.CsvParserManagerByte;
import net.sf.JRecord.CsvParser.ICsvDefinition;
import net.sf.JRecord.CsvParser.ICsvCharLineParser;
import net.sf.JRecord.CsvParser.CsvParserManagerChar;
import net.sf.JRecord.CsvParser.ICsvByteLineParser;
import net.sf.JRecord.External.Def.DependingOn;
import net.sf.JRecord.External.Def.DependingOnDefinition;
import net.sf.JRecord.External.Def.DependingOnDtls;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.Option.IRecordPositionOption;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.cgen.def.IRecordDetail4gen;
import net.sf.JRecord.detailsBasic.CsvCharDetails;
import net.sf.JRecord.detailsBasic.IItemDetails;
import net.sf.JRecord.detailsSelection.Convert;
import net.sf.JRecord.detailsSelection.FieldSelectX;
import net.sf.JRecord.detailsSelection.RecordSelection;
import net.sf.JRecord.occursDepending.IOccursDependingPositionCalculation;
import net.sf.JRecord.occursDepending.ODCalculationComplex;
import net.sf.JRecord.occursDepending.ODCalculationStandard;




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
public class RecordDetail implements AbstractRecordX<FieldDetail>, ICsvDefinition, IRecordDetail4gen {
	
	public static final int DO_NONE = 1;
	public static final int DO_SIMPLE_NO_COMPRESSION = 2;
	public static final int DO_SIMPLE = 5;

	public static final int DO_COMPLEX = 6;
	public static final int DO_COMPLEX_SIZE_IN_ARRAY = 7;


	private static final byte UNDEFINED = -121;
	private static final byte NO = 1;
	private static final byte YES = 2;

	/**
	 * This is the default stratergy to use when there is no Occurs-Depending
	 */
	private static IOccursDependingPositionCalculation DEFAULT_POSITION_CALCULATOR
		= new IOccursDependingPositionCalculation() {
			@Override public int calculateActualPosition(AbstractIndexedLine line, DependingOnDtls dependingOnDtls, int pos) {
				return pos;
			}

			@Override public void checkForSizeFieldUpdate(AbstractLine line, IFieldDetail fld) { }

			@Override public void clearBuffers(AbstractLine line) { }
		};
	
	private String recordName;

	private int fieldCount;
	private FieldDetail[] fields;
	private final List<? extends IItemDetails> cobolItems;

	//private FieldDetail selectionFld = null;
	//private int    selectionFieldIdx = Constants.NULL_INTEGER;
	private int    recordType;

	//private String selectionField;
	//private String selectionValue;
	private RecordSelection recordSelection = new RecordSelection();
    private final IRecordPositionOption recordPositionOption;

	private CsvCharDetails delimiter;
	private final CsvCharDetails quote;
	private int    length = 0;
	private int    minumumPossibleLength;
	private String fontName;

	private final int    recordStyle;

	private int parentRecordIndex = Constants.NULL_INTEGER;

	private int sourceIndex = 0;

	private int numberOfFieldsAdded = 0;
	//private int editorStatus = STATUS_UNKOWN;


	private byte singleByteFont = UNDEFINED;
	private boolean embeddedNewLine = false;
	
	private int[] fieldTypes = null;
	private DependingOnDefinition dependingOn = null;
	private int dependingOnLevel = DO_NONE;
	private IOccursDependingPositionCalculation odCalculator = DEFAULT_POSITION_CALCULATOR;
	private HashMap<String, ArrayDtls> arrays;
	
	private final ICsvCharLineParser csvCharParser;
	private final ICsvByteLineParser csvByteParser;


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

		this(pRecordName, null, pRecordType, pDelim,
			 pQuote, pFontName, pFields, null, pRecordStyle);

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
		this(pRecordName, pRecordType, pDelim, pQuote, pFontName, pFields, pRecordStyle, selection, false);
	}

	public RecordDetail(final String pRecordName,
			final int pRecordType,
			final String pDelim,
			final String pQuote,
			final String pFontName,
			final FieldDetail[] pFields,
			final int pRecordStyle,
			final RecordSelection selection,
			final boolean embeddedCr
			) {
		this(pRecordName, null, pRecordType, pDelim,
				pQuote, pFontName, pFields, null, pRecordStyle);
	
		if (selection != null) {
			this.recordSelection = selection;
		}
		this.embeddedNewLine = embeddedCr;
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
	 					final IRecordPositionOption rpOpt,
						final int pRecordType,
						final String pDelim,
						final String pQuote,
						final String pFontName,
						final FieldDetail[] pFields,
						final int pRecordStyle
						) {
		this(pRecordName, rpOpt, pRecordType, pDelim, pQuote, pFontName, pFields, null, pRecordStyle);
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
	 					final IRecordPositionOption rpOpt,
						final int pRecordType,
						final String pDelim,
						final String pQuote,
						final String pFontName,
						final FieldDetail[] pFields,
						      List<? extends IItemDetails> items,
						final int pRecordStyle
						) {


		int j, l;
		this.recordName = pRecordName;
		this.recordType = pRecordType;

		this.fields   = pFields;
		this.cobolItems = items;
		this.quote    = CsvCharDetails.newQuoteDefinition(pQuote, pFontName);
		this.fontName = pFontName;
		this.recordStyle = pRecordStyle;
		this.recordPositionOption  = rpOpt;


		this.fieldCount = pFields.length;
		while (fieldCount > 0 && fields[fieldCount - 1] == null) {
		    fieldCount -= 1;
		}

		setDelimiter(CsvCharDetails.newDelimDefinition(pDelim, fontName));
		
		//delimiterUneditted = pDelim;
		//delimiter = Conversion.decodeFieldDelim(pDelim, fontName);

		//System.out.println("Quote 1 ==>" + pQuote + "<==");
		for (j = 0; j < fieldCount; j++) {
		    pFields[j].setRecord(this);
		    l = pFields[j].getPos() + pFields[j].getLen() - 1;
		    if (pFields[j].getLen() >= 0 && length < l) {
		    	length = l;
		    }
		}
		minumumPossibleLength = length;
		
		csvByteParser = CsvParserManagerByte.getInstance().get(recordStyle);
		csvCharParser = CsvParserManagerChar.getInstance().get(recordStyle);
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
	 * @return the cobolItems
	 */
	@Override
	public List<? extends IItemDetails> getCobolItems() {
		return cobolItems;
	}
	
	void updateNameCobolItemMap(Map<String, List<IItemDetails>> groupMap, Map<String, List<IItemDetails>> groupAndFieldMap) {
		if (cobolItems != null) {
			updateNameCobolItemMap(cobolItems, groupMap, groupAndFieldMap);
		}
	}

	private void updateNameCobolItemMap(
			List<? extends IItemDetails> cblItems, 
			Map<String, List<IItemDetails>> groupMap, Map<String, List<IItemDetails>> groupAndFieldMap) {
		if (cblItems != null) {
			for (IItemDetails itm : cblItems) {
				List<IItemDetails> singletonList = updateItemListMap(groupAndFieldMap, itm, null);
				if (! itm.isLeaf()) {
					singletonList = updateItemListMap(groupMap, itm, singletonList);
					updateNameCobolItemMap(itm.getChildItems(), groupMap, groupAndFieldMap);
				}
			}
		}
	}

	private List<IItemDetails> updateItemListMap(Map<String, List<IItemDetails>> itmMap, IItemDetails itm, List<IItemDetails> siList) {
		if (itmMap != null && itm.getFieldName() != null) {
			String key = itm.getFieldName().toUpperCase();
			List<IItemDetails> similarlyNamedItems = itmMap.get(key);
			if (similarlyNamedItems == null) {
				List<IItemDetails> singletonList = siList != null ? siList : Collections.singletonList(itm);
				itmMap.put(key, singletonList);
				return singletonList;
			} else if (similarlyNamedItems.size() == 1) {
				List<IItemDetails> list = new ArrayList<IItemDetails>(5);
				list.add(similarlyNamedItems.get(0));
				list.add(itm);
				itmMap.put(key, list);
			} else {
				similarlyNamedItems.add(itm);	
			}
		}
		return null;
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
	 * @deprecated you should do this at the External record stage before you build the layout
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
	 * @return the minumumPossibleLength
	 */
	public final int getMinumumPossibleLength() {
		return minumumPossibleLength;
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

    public List<FieldDetail> getFields() {
		return Collections.unmodifiableList(
					Arrays.asList(fields)
		);

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
     *
     * @deprecated
     */
    public String getDelimiter() {
        return delimiter.asString();
    }

    @Override
	public CsvCharDetails getDelimiterDetails() {
		return delimiter;
	}

	protected void setDelimiter(CsvCharDetails delimiter) {
		this.delimiter = delimiter;//CsvCharDetails.newDelimDefinition(delimiter, fontName);
	}


//	/**
//	 * @return the delimiterUneditted
//	 */
//	public final String getDelimiterUneditted() {
//		return delimiterUneditted;
//	}

//	/**
//	 * @see net.sf.JRecord.Common.AbstractRecord#getQuote()
//	 */
//    @Override
//    public String getQuote() {
//        return quote;
//    }


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


//	/**
//	 * @return the quoteUneditted
//	 */
//	public final String getQuoteUneditted() {
//		return quoteUneditted;
//	}

	@Override
	public CsvCharDetails getQuoteDefinition() {
		return quote;
	}

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
//
//	public final String convertFieldDelim(String pDelim) {
//		String delimiter = pDelim;
//
//		if ((pDelim == null) || pDelim == "\t" || ((pDelim = pDelim.trim()).equalsIgnoreCase("<tab>"))) {
//			delimiter = "\t";
//		} else if (pDelim.equalsIgnoreCase("<space>")) {
//			delimiter = " ";
//		} else if (pDelim.length() == 0 || pDelim.startsWith("x'") || pDelim.startsWith("X'")) {
//			
//		} else {
//			delimiter = Conversion.decodeCharStr(pDelim, fontName);
//		}
//		return delimiter;
//	}

//	/**
//	 * The input to this method can be either:<ul>
//	 * <li>A single character
//	 * <li>A character represented in unicode format: \\u0001
//	 * (\\u followed by the character code in hex format).
//	 * </ul>
//	 * 
//	 * @param charId character id to be decoded
//	 * @return character decoded character.
//	 * @throws NumberFormatException
//	 */
//	public static String decodeUnicodeChar(String charId) {
//		char ch;
//		int charLength = charId.length();
//		
//		if (charLength > 2 && charLength < 7 && charId.charAt(0) == '\\'
//		&&((ch = charId.charAt(1)) == 'u' || ch == 'U')) {
//			char[] chars = { (char)Integer.parseInt(charId.substring(2), 16) };
//			charId = new String(chars);
//		}
//		return charId;
//	}
//
	/**
	 * @return the recordSelection
	 */
	public RecordSelection getRecordSelection() {
		return recordSelection;
	}
	
	public void updateRecordSelection(ExternalSelection selection, IGetFieldByName fieldInfo) {
	    if (selection != null && selection.getSize() > 0) {
	    	this.getRecordSelection().setRecSel((new Convert()).convert(selection, fieldInfo));
	    }
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

	/**
	 * Find all fields with supplied Field name / Group (or level) name.
	 * The Group names can be supplied in any sequence
	 *
	 * <pre>
	 *  For:
	 *
	 *    01 Group-1.
	 *       05 Group-2.
	 *          10 Group-3
	 *             15 Field-1         Pic X(5).
	 *
	 * you could code any of the following:
	 *
	 *   flds = line.getFields("Field-1", "Group-1", "Group-2", "Group-3");
	 *   flds = line.getFields("Field-1", "Group-3", "Group-2", "Group-1");
	 *   flds = line.getFields("Field-1", "Group-3", "Group-1", "Group-2");
	 *   flds = line.getFields("Field-1", "Group-2", "Group-1", "Group-3");
	 *   flds = line.getFields("Field-1", "Group-3",");
	 *   flds = line.getFields("Field-1", "Group-3", "Group-1");
	 *   flds = line.getFields("Field-1", "Group-1", "Group-3");
	 *
	 * </pre>
	 *
	 * @param fieldName field name to search for
	 * @param groupNames group names to search for
	 * @return requested fields
	 * @deprecated use {@link #getGroupFields(String...)}
	 */
	public final List<IFieldDetail> getFields(String fieldName, String... groupNames) {

		ArrayList<IFieldDetail> ret = new ArrayList<IFieldDetail>();
		boolean ok;
		String groupName;
		int numLevelNames = groupNames == null ? 0 : groupNames.length;
		ArrayList<String> ln = new ArrayList<String>(numLevelNames);

		for (int i = 0; i < numLevelNames; i++) {
			if (groupNames[i] != null && ! "".equals(groupNames[i])) {
				ln.add("." + groupNames[i].toUpperCase() + ".");
			}
		}

		for (FieldDetail f : fields) {
			if (f.getName().equalsIgnoreCase(fieldName)) {
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


	/**
	 * Find all fields with supplied Field name / Group (or level) name.
	 * The Group names must be supplied in any sequence they appears in the copybook
	 *
	 * <pre>
	 *  For:
	 *
	 *    01 Group-1.
	 *       05 Group-2.
	 *          10 Group-3
	 *             15 Field-1         Pic X(5).
	 *
	 * you would code:
	 *
	 *   flds = line.getFieldsGroupsInSequence("Field-1", "Group-1", "Group-2", "Group-3");
	 *
	 * or
	 *
	 *   flds = line.getFieldsGroupsInSequence("Field-1", "Group-1", "Group-3");
	 *   flds = line.getFieldsGroupsInSequence("Field-1", "Group-3");
	 *   flds = line.getFieldsGroupsInSequence("Field-1", "Group-1");
	 *
	 * </pre>
	 *
	 * @param fieldName field name to search for
	 * @param groupNames group names to search for
	 * @return requested fields
	 * 
	 * @deprecated {@link #getGroupFields(String...)}
	 */
	public final List<IFieldDetail> getFieldsGroupsInSequence(String fieldName, String... groupNames) {
		return getFieldsGroupsInSequence(fieldName, 0, 0, groupNames);
	}
	
	/**
	 * Find all fields with supplied Field name / Group (or level) name.
	 * The Group names must be supplied in any sequence they appears in the copybook
	 *
	 * <pre>
	 *  For:
	 *
	 *    01 Group-1.
	 *       05 Group-2.
	 *          10 Group-3
	 *             15 Field-1         Pic X(5).
	 *
	 * you would code:
	 *
	 *   flds = line.getFieldsGroupsInSequence("Group-1", "Group-2", "Group-3", "Field-1");
	 *
	 * or
	 *
	 *   flds = line.getGroupFields("Group-1", "Group-3", "Field-1");
	 *   flds = line.getGroupFields("Group-3", "Field-1");
	 *   flds = line.getGroupFields("Group-1", "Field-1");
	 *
	 * </pre>
	 *
	 * @param fieldNames group/fields names to search for
	 * @return requested fields
	 */
	public final List<IFieldDetail> getGroupFields(String... fieldNames) {
		return getGroupFields(0, fieldNames);
	}

	private List<IFieldDetail> getGroupFields(int firstItem, String... fieldNames) {
		if (fields == null || fields.length <= firstItem) {
			return new ArrayList<IFieldDetail>();
		}
		return getFieldsGroupsInSequence(fieldNames[fieldNames.length-1], firstItem, 1, fieldNames);
	}

	
	private final List<IFieldDetail> getFieldsGroupsInSequence(String fieldName, int start, int drop, String... groupNames) {


		ArrayList<IFieldDetail> ret = new ArrayList<IFieldDetail>();
		boolean ok;
		String groupName;
		int st;
		int numLevelNames = groupNames == null ? 0 : groupNames.length - drop;
		ArrayList<String> ln = new ArrayList<String>(numLevelNames);

		for (int i = start; i < numLevelNames; i++) {
			if (groupNames[i] != null && ! "".equals(groupNames[i])) {
				ln.add("." + groupNames[i].toUpperCase() + ".");
			}
		}

		for (FieldDetail f : fields) {
			if (f.getName().equalsIgnoreCase(fieldName)) {
				groupName = f.getGroupName().toUpperCase();
				ok = true;
				st = 0;
				for (String n : ln) {
					if ((st = groupName.indexOf(n, st)) < 0) {
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


	/**
	 * Retrieve a single field that match's the supplied Field-Name Group-Names.
	 * The Group names can be supplied in any sequence
	 *
	 * <pre>
	 *  For:
	 *
	 *    01 Group-1.
	 *       05 Group-2.
	 *          10 Group-3
	 *             15 Field-1         Pic X(5).
	 *
	 * you could code any of the following:
	 *
	 *   fld = line.getUniqueField("Field-1", "Group-1", "Group-2", "Group-3");
	 *   fld = line.getUniqueField("Field-1", "Group-3", "Group-2", "Group-1");
	 *   fld = line.getUniqueField("Field-1", "Group-3", "Group-1", "Group-2");
	 *   fld = line.getUniqueField("Field-1", "Group-2", "Group-1", "Group-3");
	 *   fld = line.getUniqueField("Field-1", "Group-3",");
	 *   fld = line.getUniqueField("Field-1", "Group-3", "Group-1");
	 *   fld = line.getUniqueField("Field-1", "Group-1", "Group-3");
	 *
	 * </pre>
	 *
	 *
	 * @param fieldName field name to search for
	 * @param groupNames group names to search for
	 *
	 * @return Requested Field
	 * 
	 * @deprecated use {@link #getGroupField(String...)}
	 */
	public final IFieldDetail getUniqueField(String fieldName, String... groupNames) {
		List<IFieldDetail> flds = getFields(fieldName, groupNames);

		switch (flds.size()) {
		case 0: throw new RuntimeException("No Field Found");
		case 1: return flds.get(0);
		}

		throw new RuntimeException("Found " + flds.size() + " fields; should be only one");
	}

	/**
	 * Find requested Field with supplied Field name / Group (or level) name.
	 * The Group names must be supplied in any sequence they appears in the copybook
	 *
	 * <pre>
	 *  For:
	 *
	 *    01 Group-1.
	 *       05 Group-2.
	 *          10 Group-3
	 *             15 Field-1         Pic X(5).
	 *
	 * you would code:
	 *
	 *   fld = line.getFieldsGroupsInSequence("Field-1", "Group-1", "Group-2", "Group-3");
	 *
	 * or
	 *
	 *   fld = line.getUniqueFieldGroupsInSequence("Field-1", "Group-1", "Group-3");
	 *   fld = line.getFieldsGroupsInSequence("Field-1", "Group-3");
	 *
	 * </pre>
	 *
	 * @param fieldName field name to search for
	 * @param groupNames group names to search for
	 * @return requested fields
	 * 
	 * @deprecated use {@link #getGroupField(String...)}
	 */
	public final IFieldDetail getUniqueFieldGroupsInSequence(String fieldName, String... groupNames) {
		List<IFieldDetail> flds = getFieldsGroupsInSequence(fieldName, 0, 0, groupNames);

		switch (flds.size()) {
		case 0: throw new RuntimeException("No Field Found");
		case 1: return flds.get(0);
		}

		throw new RuntimeException("Found " + flds.size() + " fields; should be only one");
	}
	
	
	/**
	 * Find requested Field with supplied Field name / Group (or level) name.
	 * The Group names must be supplied in any sequence they appears in the copybook
	 *
	 * <pre>
	 *  For:
	 *
	 *    01 Group-1.
	 *       05 Group-2.
	 *          10 Group-3
	 *             15 Field-1         Pic X(5).
	 *
	 * you would code:
	 *
	 *   fld = line.getGroupField("Group-1", "Group-2", "Group-3", "Field-1");
	 *
	 * or
	 *
	 *   fld = line.getGroupField("Group-1", "Group-3", "Field-1");
	 *   fld = line.getGroupField("Group-3", "Field-1");
	 *
	 * </pre>
	 *
	 * @param fieldNames group/field names to search for
	 * @return requested fields
	 */
	@Override
	public final IFieldDetail getGroupField(String...fieldNames) {
		return getGroupFieldX(0, fieldNames);
	}
	
	final IFieldDetail getGroupFieldX(int start, String...fieldNames) {
		List<IFieldDetail> flds = getGroupFields(start, fieldNames);
		
		IFieldDetail fld = checkForFldMatch(flds, fieldNames);
		
		if (fld != null) {
			return fld;
		}
		
		if (flds.size() == 0) {
			throw new RecordException("No Field Found: "
					+ (fieldNames== null || fieldNames.length==0? "" : fieldNames[fieldNames.length - 1]));
		}

		throw new RecordException("Found " + flds.size() + " fields named " + fieldNames[fieldNames.length-1] + "; there should be only one");
	}

	static IFieldDetail checkForFldMatch(List<IFieldDetail> flds, String... fieldNames) {
		switch (flds.size()) {
		case 0: break;
		case 1: return flds.get(0);
		default:
			if (fieldNames.length > 1) {
				String lastGroup = "." + fieldNames[fieldNames.length - 2].toLowerCase() + ".";
				IFieldDetail fld = null;
				int cmpSize = 1;
				boolean multi = false;
				for (int i = fieldNames.length - 2; i>= 0; i-- ) {
					cmpSize += fieldNames[i].length() + 1;
				}
				for (IFieldDetail f : flds) {
					String gn = ((FieldDetail) f).getGroupName().toLowerCase();
					if (gn.endsWith(lastGroup)) {
						if (cmpSize == gn.length()) {
							return f;
						}
						multi = fld != null;
						fld = f;
					}
				}
				
				if (! multi) {
					return fld;
				}
			}
		}
		return null;
	}

	public final int[] getFieldTypes() {
		if (fieldTypes == null) {
			fieldTypes = new int[fields.length];
			for (int i =0; i < fieldTypes.length; i++) {
				fieldTypes[i] = fields[i].getType();
			}
		}
		return fieldTypes;
	}

	@Override
	public int getDelimiterOrganisation() {
		int delimiterOrganisation = ICsvDefinition.NORMAL_SPLIT;
		ICsvCharLineParser parser =  getCharParser();
		if (parser != null && parser instanceof BasicCsvLineParser) {
			BasicCsvLineParser bp = (BasicCsvLineParser) parser;
			delimiterOrganisation = bp.delimiterOrganisation;
		}
		
		return delimiterOrganisation;
	}

	/**
	 * @return the embeddedNewLine
	 */
	@Override
	public boolean isEmbeddedNewLine() {
		return embeddedNewLine;
	}



	/**
	 * @return the singleByteFont
	 */
	public boolean isSingleByteFont() {
		if (singleByteFont == UNDEFINED) {
			try {
				singleByteFont = YES;
				if (Conversion.isMultiByte(fontName)) {
					singleByteFont = NO;
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		return singleByteFont == YES;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Common.AbstractRecord#calculateActualPosition(net.sf.JRecord.Common.AbstractIndexedLine, net.sf.JRecord.External.Def.DependingOnDtls, int)
	 */
	@Override
	public int calculateActualPosition(AbstractIndexedLine line, DependingOnDtls dependingOnDtls, int pos) {
		return odCalculator.calculateActualPosition(line, dependingOnDtls, pos);
	}
	
	
	public void checkForSizeFieldUpdate(AbstractLine line, IFieldDetail fld) {
		odCalculator.checkForSizeFieldUpdate(line, fld);
	}
	
	public void clearOdBuffers(AbstractLine line) {
		odCalculator.clearBuffers(line);
	}

//	/* (non-Javadoc)
//	 * @see net.sf.JRecord.Common.AbstractRecord#calculateActualPosition(net.sf.JRecord.Common.AbstractIndexedLine, net.sf.JRecord.External.Def.DependingOnDtls, int)
//	 */
//	@Override
//	public int calculateActualPosition(AbstractIndexedLine line, DependingOnDtls dependingOnDtls, int pos) {
//		DependingOnDtls[] tree = null;
//		if (dependingOnDtls != null) {
//			tree = dependingOnDtls.getTree();
//		}
//		return pos - calculateAdjustment(dependingOn, line, tree, 0, pos);
//	}
//	
//	/**
//	 * Calculate an adjustment to the record Position based on 
//	 * 
//	 * @param dependingOnList the list of Depending on clauses to use in the calculation
//	 * @param line line or record for which to calculate the position
//	 * @param dependingOnDtls depending on details for the field which we are calculating the adjustment for
//	 * @param lvl current level (or index in dependingOnDtls array 
//	 * @param pos position of the field
//	 * 
//	 * @return Adjustment to be made to the field
//	 */
//	private int calculateAdjustment(List<DependingOn> dependingOnList, final AbstractIndexedLine line, DependingOnDtls[] dependingOnDtls,
//			int lvl, final int pos) {
//		if (dependingOnList == null || dependingOnList.size() == 0 || pos < dependingOnList.get(0).getPosition()) {
//			return 0;
//		}
//
//		int tmpAdj = 0;
//		
//		for (int i = 0; i < dependingOnList.size() && pos >= dependingOnList.get(i).getPosition(); i++) {
//			DependingOn dependingOnDef = dependingOnList.get(i);
//			IFieldDetail field = dependingOnDef.getField();	
//
//			int adj = 0;
//			try {
//				Object value = line.getField(field);
//				List<DependingOn> children = dependingOnDef.getChildren();
//				int actualOccurs = Integer.parseInt(value.toString().trim());
//				int occursLength = dependingOnDef.getOccursLength();
//
//				int childAdjustment = calculateAdjustment(children, line, dependingOnDtls, lvl + 1, pos);
//				if (pos > dependingOnDef.getEnd()) {
//					int occursMaxLength = dependingOnDef.getOccursMaxLength();
//					int actualOccursLength = occursLength 
//									 - childAdjustment; 
//					int actualLength = actualOccurs * actualOccursLength;
//	//						- calculateAdjustment(dependingOnDef.getChildren(), line, pos);
//					adj = occursMaxLength - actualLength;//calculateAdjustment(dependingOnDef.getChildren(), line, pos);
//					if (pos - adj < dependingOnDef.getPosition() + actualLength) {
//						return tmpAdj;
//					}
//				} else if (children != null && children.size() > 0 && childAdjustment > 0) { 
//					if (dependingOnDtls != null
//					&& lvl < dependingOnDtls.length ) {
//						int idx = dependingOnDtls[lvl].index;
//						adj = childAdjustment * (idx + 1);
//						
//						DependingOn c = children.get(children.size() - 1);	
//
//						if (pos < c.getEnd() + idx * occursLength) {
//							adj = childAdjustment;
//							int occurs = 1;
////							System.out.print("\t$$ " + pos + " - " + c.getEnd() + " > " + occursLength);
//							if (occursLength != 0 && pos - c.getEnd() > occursLength) {
//								occurs = ((int) (pos - c.getEnd()) / occursLength) + 1;
//								if (occurs > 1) {
//									adj = childAdjustment * occurs;											
//								}
//							}
//							
//							if (pos - children.get(0).getPosition() + children.get(0).getOccursLength() > occursLength) {
//								int tChildAdj = calculateAdjustment(children, line, dependingOnDtls, lvl + 1, pos - occurs * occursLength);	
//								adj += tChildAdj;						
//							}
//						}
//					} else {
//						adj = childAdjustment;
//					}
//				}
//				tmpAdj += adj;
//			} catch (RuntimeException e) {
//				System.out.println();
//				System.out.println("Error Retrieving: " + (field==null?"null field":field.getName()));
//				System.out.println();
//				throw e;
//			} catch (Exception e) {
//				throw new RecordException("Error calculation Occurs Depending On for Variable: " + dependingOnDef.getVariableName() + " msg="+ e.getMessage(), e); 
//			}
//		} 
//		return tmpAdj;	
//	}
//


	/**
	 * @param dependingOnDef the dependingOn to set
	 */
	public final void setDependingOn(DependingOnDefinition dependingOnDef) {
		
		int len = 0;
		this.dependingOn = dependingOnDef;
		this.dependingOnLevel = DO_NONE;
		
		List<DependingOn> dependOnList = dependingOnDef.dependOnList;
		if (dependOnList != null && dependOnList.size() > 0) {
			if (dependOnList.size() == 1 
			&& (dependOnList.get(0).getPosition() + dependOnList.get(0).getOccursMaxLength() -1 == fields[fields.length - 1].getEnd())) {
				dependingOnLevel = DO_SIMPLE_NO_COMPRESSION;
				dependOnList.get(0).updateField(this);
			} else {
				int firstPos = Integer.MAX_VALUE; 
				this.dependingOnLevel = DO_SIMPLE;
				if (dependOnList.size() > 3) {
					dependingOnLevel = DO_COMPLEX;
				}
				
				for (DependingOn  d : dependOnList) {
					List<DependingOn> children = d.getChildren();
					d.updateField(this);
					
					if (firstPos == Integer.MAX_VALUE) {
						firstPos = d.getField().getPos();
					}
					if ((children != null && children.size() > 0)
					||  (d.getField().getPos() > firstPos)) {
						dependingOnLevel = Math.max(dependingOnLevel, DO_COMPLEX);
					}
					if (d.isComplicatedDependingOn()) {
						dependingOnLevel = DO_COMPLEX_SIZE_IN_ARRAY;
					}
				}
			}
			
			for (DependingOn d : dependOnList) {
				len += d.getOccursMaxLength();
			}
			
			if (dependingOnLevel < DO_COMPLEX_SIZE_IN_ARRAY
			&& dependingOnDef.getMoveableSizeFields() > 2) {
				dependingOnLevel = DO_COMPLEX_SIZE_IN_ARRAY;
			}
		}

		dependingOnDef.buildSizeFieldMap();
		switch (dependingOnLevel) {
		case DO_NONE: odCalculator = DEFAULT_POSITION_CALCULATOR;		break;
		case DO_COMPLEX_SIZE_IN_ARRAY:
			odCalculator = new ODCalculationComplex(dependingOnDef);
			break;
		default:
			odCalculator = new ODCalculationStandard(dependingOnDef);
		}
		minumumPossibleLength = length - len;
	}
	
	/**
	 * @return the dependingOn
	 */
	public final DependingOnDefinition getDependingOn() {
		if (dependingOn == null) {
			return null;
		}
		return dependingOn;
	}

	public final boolean hasDependingOn() {
		return dependingOnLevel > DO_NONE;
	}

	/**
	 * @return the dependingOnLevel
	 */
	public final int getDependingOnLevel() {
		return dependingOnLevel;
	}
	

	/**
	 * @return the recordOption
	 */
	public final IRecordPositionOption getRecordPositionOption() {
		return recordPositionOption;
	}


	public ICsvByteLineParser getCsvByteParser() {
		return csvByteParser;
	}

	@Override
	public final ICsvCharLineParser getCharParser() {
		return csvCharParser; 
	}

	public final FieldDetail[] getArrayFields(FieldDetail field, String aname) {
		if (arrays == null) {
			synchronized (this) {
				if (arrays == null) {
					HashMap<String, RecordDetail.ArrayDtls> tmpArrays = new HashMap<String, RecordDetail.ArrayDtls>();
					ArrayList<RecordDetail.ArrayDtls> arrayList = new ArrayList<RecordDetail.ArrayDtls>();
					int pos;
					for (int i = 0; i < fields.length; i++) {
						if ((pos = fields[i].getName().indexOf('(')) > 0) {
							String arrayName = fields[i].getName().substring(0, pos-1);
							String id = (fields[i].getGroupName() +  arrayName).toLowerCase();
							ArrayDtls dtls = tmpArrays.get(id);
							if (dtls == null) {
								dtls = new ArrayDtls(fields[i].getGroupName(), arrayName);
								tmpArrays.put(id, dtls);
								arrayList.add(dtls);
							}
							dtls.fieldList.add(fields[i]);
						}
					}
					for (RecordDetail.ArrayDtls ad : arrayList) {
						ad.fields = ad.fieldList.toArray(new FieldDetail[ad.fieldList.size()]);
						ad.fieldList = null;
					}
					arrays = tmpArrays;
				}
			}
		}
		
		RecordDetail.ArrayDtls a = arrays.get((field.getGroupName() + aname) .toLowerCase());
		return a == null ? null : a.fields;
	}

	private static class ArrayDtls {
		final String group, name;
		ArrayList<FieldDetail> fieldList = new ArrayList<FieldDetail>(); 
		FieldDetail[] fields;
		protected ArrayDtls(String group, String name) {
			super();
			this.group = group;
			this.name = name;
		}
		
		
	}

}
