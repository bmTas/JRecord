/*
 * Created on 8/05/2004
 *
 *  This class represents a group of records
 *
 * Modification log:
 * On 2006/06/28 by Jean-Francois Gagnon:
 *    - Made sure a Group of Records is tested to see if
 *      there is binary format to be handled
 *
 * Version 0.61 (2007/03/29)
 *    - CSV Split for when there a blank columns in the CSV file
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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IBasicFileSchema;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.CsvParser.ICsvCharLineParser;
import net.sf.JRecord.CsvParser.ICsvDefinition;
import net.sf.JRecord.IO.builders.recordDeciders.SingleFieldDecider;
import net.sf.JRecord.CsvParser.CsvDefinition;
import net.sf.JRecord.CsvParser.ICsvByteLineParser;
import net.sf.JRecord.CsvParser.CsvParserManagerChar;
import net.sf.JRecord.CsvParser.CsvParserManagerByte;
import net.sf.JRecord.Option.IRecordPositionOption;
import net.sf.JRecord.Option.Options;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.cgen.def.ILayoutDetails4gen;
import net.sf.JRecord.detailsBasic.CsvCharDetails;
import net.sf.JRecord.detailsBasic.IItemDetails;




/**
 * This class represents a <b>record-layout</b> description. i.e.
 * It describes the Structure of both a File and the lines in it.
 * <p>A <b>Layout</b> can have
 * one or more <b>Records</b> (class RecordDetail) which intern
 * holds one or more <b>fields</b> (class FieldDetail).
 *
 * <pre>
 *     LayoutDetail  - Describes a file
 *       |
 *       +----- RecordDetail (1 or More) - Describes one record in the file
 *                |
 *                +------  FieldDetail (1 or More)  - Describes one field in the file
 * </pre>
 *
 * <p>There are several ways to load a RecordLayout
 * <pre>
 * <b>Loading an RecordEditor-XML:</b>
 *          LayoutDetail layout = CopybookLoaderFactory.getInstance().getLayoutRecordEditXml(copybookName, null);
 *
 * <b>Using the Loader Factory CopybookLoaderFactory:</b>
 *          CopybookLoader loader = CopybookLoaderFactory.getInstance()
 *                  .getLoader(CopybookLoaderFactory.RECORD_EDITOR_XML_LOADER);
 *          LayoutDetail layout = loader.loadCopyBook(copybookName, 0, 0, "", 0, 0, null).asLayoutDetail();
 *
 * <b>Creating the loader:</b>
 *          CopybookLoader loader = new RecordEditorXmlLoader();
 *          LayoutDetail layout = loader.loadCopyBook(copybookName, 0, 0, "", 0, 0, null).asLayoutDetail();
 *
 * @param pLayoutName Record Layout Name
 * @param pRecords All the sub records
 * @param pDescription Records Description
 * @param pLayoutType Record Type
 * @param pRecordSep Record Separator
 * @param pEolIndicator End of line indicator
 * @param pFontName Canonical Name
 * @param pRecordDecider used to decide which layout to use
 * @param pFileStructure file structure
 *
 *
 * @author Bruce Martin
 * @version 0.55
 *
 */
public class LayoutDetail implements IBasicFileSchema, ILayoutDetails4gen {
	

	private String layoutName;
	private String description;
	private byte[] recordSep;
	private final int layoutType;
	private RecordDetail[] records;
	private boolean binary = false, binaryField=false;
	private String fontName = "";
	private String eolString;
	//private TypeManager typeManager;
	private final RecordDecider decider;

	private HashMap<String, IFieldDetail> fieldNameMap = null;
	private HashMap<String, IFieldDetail> recordFieldNameMap = null;
	private HashSet<String> duplicateFieldNames = null;
	
	private CsvCharDetails delimiter;
	private int fileStructure;

	private int recordCount;
	
	private boolean treeStructure = false;

	private final boolean multiByteCharset, csvLayout, headerTrailerRecords;
	
	private final byte spaceByte, initByte ;
	
	private final int maxPossibleLength, minPossibleLength;
	

	private Map<String, List<IItemDetails>> groupMap, groupFieldMap;
	
	/**
	 * This class holds a one or more records
	 *
	 * @param pLayoutName Record Layout Name
	 * @param pRecords All the sub records
	 * @param pDescription Records Description
	 * @param pLayoutType Record Type
	 * @param pRecordSep Record Separator
	 * @param pEolIndicator End of line indicator
	 * @param pFontName Canonical Name
	 * @param pRecordDecider used to decide which layout to use
	 * @param pFileStructure file structure
	 *
	 */
	public LayoutDetail(final String pLayoutName,
	        		   final RecordDetail[] pRecords,
	        		   final String pDescription,
	        		   final int pLayoutType,
	        		   final byte[] pRecordSep,
	        		   final String pEolIndicator,
	        		   final String pFontName,
	        		   final RecordDecider pRecordDecider,
	        		   final int pFileStructure) 
	{
		this(	pLayoutName, pRecords, pDescription, pLayoutType, pRecordSep, pEolIndicator, 
				pFontName, pRecordDecider, pFileStructure, null, false, -1);
	}
	
	public LayoutDetail(final String pLayoutName,
 		   final RecordDetail[] pRecords,
 		   final String pDescription,
 		   final int pLayoutType,
 		   final byte[] pRecordSep,
 		   final String pEolIndicator,
 		   final String pFontName,
 		   		 RecordDecider pRecordDecider,
 		   final int pFileStructure,
 		   final IRecordPositionOption rpOpt,
 		         boolean  initToSpaces,
 		         int recordLength) {
	    super();

        int i, j;
        boolean first = true;
        //int lastSize = -1;


	    this.layoutName    = pLayoutName;
		this.records       = pRecords;
		this.description   = pDescription;
		this.layoutType    = pLayoutType;
		this.recordSep     = pRecordSep;
		this.fontName      = pFontName;
		//this.decider       = pRecordDecider;
		this.fileStructure = CommonBits.translateFileStructureToNotAskFont(pFileStructure);
		this.recordCount   = pRecords.length;
//		this.setDecider(pRecordDecider);
		

		if (fontName == null) {
		    fontName = "";
		}
		this.multiByteCharset = Conversion.isMultiByte(fontName);
		byte[] t = Conversion.getBytes(" ", fontName);
		if (t.length == 1) {
			this.spaceByte = t[0];
		} else {
			this.spaceByte = 0;
		}
		
		initByte = initToSpaces? spaceByte: 0;

		while (recordCount > 0 && pRecords[recordCount - 1] == null) {
		    recordCount -= 1;
		}

		if (recordSep == null) {
			if ("".equals(fontName)) {
				recordSep = Constants.SYSTEM_EOL_BYTES;
			} else {
				recordSep = CommonBits.getEolBytes(null, "", fontName);
			}
			
			recordSep = CommonBits.getEolBytes(recordSep, pEolIndicator, fontName);
		}

		if (Constants.DEFAULT_STRING.equals(pEolIndicator)
		||  pRecordSep == null) {
		    eolString = System.getProperty("line.separator");
		    if (recordSep != null && recordSep.length < eolString.length()) {
		    	eolString = Conversion.toString(recordSep, pFontName);
		    }
		} else {
		    eolString = Conversion.toString(pRecordSep, pFontName);
		}

	    if (recordCount >= 1) {
	        int numFields;
	        for (j = 0; (! binaryField) && j < recordCount; j++) {
	            numFields =  pRecords[j].getFieldCount();
	            for (i = 0; (! binaryField) && i < numFields; i++) {
	            	binaryField = pRecords[j].isBinary(i);
	            }
	        }
	    }

		switch (pLayoutType) {
			case Constants.rtGroupOfBinaryRecords:
			case Constants.rtFixedLengthRecords:
			case Constants.rtBinaryRecord:
			    binary = true;
			break;
//			case Constants.rtBinaryRecord:
            case Constants.rtGroupOfRecords:
			case Constants.rtRecordLayout:
				binary = binaryField;
			break;
			default:
		}

		boolean csv = false;
		boolean hasFilePosRecords = false;
		
		delimiter = CsvCharDetails.newDelimDefinition("\\t", fontName);
		
	    for (j = 0; j < recordCount; j++) {
	    	RecordDetail record =  pRecords[j];
	    	hasFilePosRecords = hasFilePosRecords || record.getRecordPositionOption() != null;
	    	if ((record.getRecordType() == Constants.rtDelimitedAndQuote
			          || record.getRecordType() == Constants.rtDelimited)) {
	    		csv = true;
	    	}
	    	if (record.getFieldCount() > 0) {
		    	treeStructure = treeStructure || (record.getParentRecordIndex() >= 0);
		        CsvCharDetails recordDelimiter = record.getDelimiterDetails();
				if ((record.getRecordType() == Constants.rtDelimitedAndQuote
		          || record.getRecordType() == Constants.rtDelimited)
		        &&  (!delimiter.equals(recordDelimiter))) {
//		        	fixedLength = false;
		            if (first) {
		                delimiter = recordDelimiter;
		                first = false;
		            } else if (! delimiter.equals(recordDelimiter)) {
		                throw new RuntimeException(
		                        	"only one field delimiter may be used in a Detail-Group "
		                        +   "you have used \'" + delimiter
		                        +   "\' and \'"
		                        +  recordDelimiter.jrDefinition() + "\'"
		                );
		            }
		        }
	    	}
	    }
		

	    //List<E>
	    int maxSize = 0;
	    int minSize = recordCount > 0 ? Integer.MAX_VALUE: 0;
		for (i = 0; i < recordCount; i++) {
			maxSize = java.lang.Math.max(maxSize, records[i].getLength());
			minSize = java.lang.Math.min(minSize, records[i].getMinumumPossibleLength());
		}
		maxPossibleLength = recordLength>= 0 ? recordLength : maxSize;
		minPossibleLength = minSize;

	    this.headerTrailerRecords = hasFilePosRecords;
	    csvLayout = csv;
	    
//	    this.setDecider(pRecordDecider);
		if (pRecordDecider != null && pRecordDecider instanceof IRecordDeciderX) {
			if (pRecordDecider instanceof SingleFieldDecider) {
				try {
					pRecordDecider = (RecordDecider) ((SingleFieldDecider) pRecordDecider).clone();
				} catch (CloneNotSupportedException e) {
				}
			}
			((IRecordDeciderX) pRecordDecider).setLayout(this);
		}
		this.decider       = pRecordDecider;
	}


	/**
	 * get a specified field
	 *
	 * @param layoutIdx the specific record layout to be used
	 * @param fieldIdx field index required
	 * @return the required field
	 */
	public FieldDetail getField(final int layoutIdx, final int fieldIdx) {
		return records[layoutIdx].getField(fieldIdx);
	}

	
	/**
	 * Get the field Description array
	 *
	 * @param layoutIdx layout that we want the description for
	 * @return all the descriptions
	 */
	public String[] getFieldDescriptions(final int layoutIdx, int columnsToSkip) {
	    if (layoutIdx >= recordCount) {
	        return null;
	    }
	    RecordDetail rec = records[layoutIdx];
		String[] ret = new String[rec.getFieldCount() - columnsToSkip];
		int i, idx;

		for (i = 0; i < rec.getFieldCount() - columnsToSkip; i++) {
		    idx = getAdjFieldNumber(layoutIdx, i + columnsToSkip);
			ret[i] = rec.getField(idx).getDescription();
			if (ret[i] == null || "".equals(ret[i])) {
			    ret[i] = rec.getField(idx).getName();
			}
		}

		return ret;
	}


	/**
	 * Get the records Description.
	 *
	 * @return The description
	 */
	public String getDescription() {
		return description;
	}


	/**
	 * Get the record-layout name
	 *
	 * @return record layout name
	 */
	public String getLayoutName() {
		return layoutName;
	}

	/**
	 * Get all the record Details.
	 *
	 * @return all the record layouts
	 * @deprecated use getRecordsAsList or getRecord instead
	 */
	public RecordDetail[] getRecords() {
		return records;
	}
	
	public List<RecordDetail> getRecordsAsList() {
		return Collections.unmodifiableList(
						Arrays.asList(records)
		);
	}

	/**
	 * Add a record to the layout
	 * @param record new record
	 */
	public void addRecord(RecordDetail record) {
	    if (recordCount >= records.length) {
	    	RecordDetail[] temp = records;
	        records = new RecordDetail[recordCount + 5];
	        System.arraycopy(temp, 0, records, 0, temp.length);
	        recordCount = temp.length;
	    }
	    records[recordCount] = record;
	    recordCount += 1;
	}


	/**
	 * get a specific field number
	 *
	 * @param recordNum record number to retrieve
	 *
	 * @return a specific record layout
	 */
	public RecordDetail getRecord(int recordNum) {
	    if (recordNum < 0 || records.length == 0) {
	        return null;
	    }
		return records[recordNum];
	}

	/**
	 * Return record by record name
	 * @param recordName requested record name
	 * @return requested record.
	 */
	public RecordDetail getRecord(String recordName) {
		if (recordName == null) {
			throw new RuntimeException("Record name can not be null");
		}
		for (RecordDetail rec : records) {
			 if (recordName.equalsIgnoreCase(rec.getRecordName())) {
				 return rec;
			 }
		}
		throw new RuntimeException("Record: " + recordName + " was not found");
	}
	
	/**
	 * get number of records in the layout
	 *
	 *
	 * @return the number of records in the layout
	 */
	public int getRecordCount() {
		return recordCount;
	}


	/**
	 * get record type
	 *
	 * @return the Record Type
	 */
	public int getLayoutType() {
		return layoutType;
	}



	/**
	 * Get the record Seperator bytes
	 *
	 * @return Record Seperator
	 */
	public byte[] getRecordSep() {
		return recordSep;
	}


	/**
	 * wether it is a binary record
	 *
	 * @return wether it is a binary record
	 */
    public boolean isBinary() {
        return binary ;
    }


	@Override
	public boolean useByteRecord() {
		
		switch (fileStructure) {
		case Constants.IO_BIN_TEXT:
		case Constants.IO_FIXED_LENGTH:
			return true;
		}
		return binary;
	}

	/**
	 * @return the headerTrailerRecords
	 */
	public final boolean hasHeaderTrailerRecords() {
		return headerTrailerRecords;
	}
	
	public SpecialRecordIds getPositionRecordId() {
		int h=-1, m=-1, t=-1;
		for (int i = 0; i < recordCount; i++) {
			if (getRecord(i).getRecordPositionOption() == Options.RP_FIRST_RECORD_IN_FILE) {
				h = i;
			} else if (getRecord(i).getRecordPositionOption() == Options.RP_MIDDLE_RECORDS) {
				m = i;
			} else if (getRecord(i).getRecordPositionOption() == Options.RP_LAST_RECORD_IN_FILE) {
				t = i;
			}
		}	
		return new SpecialRecordIds(h, m, t);
	}
	/**
	 * @return the binaryField
	 */
    public final boolean hasBinaryField() {
		return binaryField;
	}

    /**
     * Get the Charset Name (ie Font name)
     *
     * @return Charset Name (ie Font name)
     */
    public String getFontName() {
        return fontName;
    }

    

//    @Override
//	public String getQuote() {
//		return records[0].getQuote();
//	}
//

	@Override
	public CsvCharDetails getQuoteDetails() {
		return records[0].getQuoteDefinition();
	}

	/**
     * Get the seperator String
     *
     * @return end of line string
     */
    public String getEolString() {
        return eolString;
    }


    /**
     * Get the maximum length of the Layout
     *
     * @return the maximum length
     */
    public int getMaximumRecordLength() {
    	return maxPossibleLength;
//        int i;
//        int maxSize = 0;
//		for (i = 0; i < recordCount; i++) {
//			maxSize = java.lang.Math.max(maxSize, records[i].getLength());
//		}
//
//		return maxSize;
    }

    public int getMinimumRecordLength() {
    	return minPossibleLength;
//        int i;
//        int maxSize = 0;
//		for (i = 0; i < recordCount; i++) {
//			maxSize = java.lang.Math.max(maxSize, records[i].getLength());
//		}
//
//		return maxSize;
    }

    /**
     * Return the file structure
     *
     * @return file structure
     */
    public int getFileStructure() {
        int ret;// = fileStructure;

        if (fileStructure == Constants.IO_NAME_1ST_LINE &&  isBinCSV()) {
        	ret = Constants.IO_BIN_NAME_1ST_LINE;
        } else if (fileStructure > Constants.IO_TEXT_LINE) {
        	ret = fileStructure;
        } else if (fileStructure == Constants.IO_TEXT_LINE) {
			ret = checkTextType();
        } else if (getLayoutType() == Constants.rtGroupOfBinaryRecords
               &&  recordCount > 1) {
		    ret = Constants.IO_BINARY_IBM_4680;
		} else if (isBinary()) {
		    ret = Constants.IO_FIXED_LENGTH;
		} else if ( isBinCSV()) {
			ret = Constants.IO_BIN_TEXT;
		} else {
			ret = checkTextType();
		}
       //System.out.println(" ~~ getFileStructure " + fileStructure + " " + ret);

		return ret;
    }

    private int checkTextType() {
    	int ret = fileStructure;
    	if ( isBinCSV()) {
			ret = Constants.IO_BIN_TEXT;
		} else if (multiByteCharset) {
    		return Constants.IO_UNICODE_TEXT;
		} else if (fontName != null && ! "".equals(fontName) && ! fontName.equals(Conversion.getDefaultSingleByteCharacterset())){
		    ret = Constants.IO_TEXT_LINE;
		} else {
			ret = Constants.IO_BIN_TEXT;
		}

    	return ret;
	}
    /**
     * Get the Index of a specific record (base on name)
     *
     * @param recordName record name being searched for
     *
     * @return index of the record
     */
    public int getRecordIndex(String recordName) {
        int ret = Constants.NULL_INTEGER;
        int i;

        if (recordName != null) {
            for (i = 0; i < recordCount; i++) {
                if (recordName.equalsIgnoreCase(records[i].getRecordName())) {
                    ret = i;
                    break;
                }
            }
        }
        return ret;
    }


    /**
     * Get the Record Decider class (if present)
     * @return Returns the record layout decider.
     */
    public RecordDecider getDecider() {
        return decider;
    }


//    /**
//	 * @param decider the decider to set
//	 */
//	protected final void setDecider(RecordDecider decider) {
//		this.decider = decider;
//		
//		if (decider != null && decider instanceof IRecordDeciderX) {
//			((IRecordDeciderX) decider).setLayout(this);
//		}
//	}


	/**
     * Get a fields value
     *
     * @param record record containg the field
     * @param type type to use when getting the field
     * @param field field to retrieve
     *
     * @return fields Value
     * 
     * @deprecated use getField on Line
     */
    @Deprecated 
    public Object getField(final byte[] record, int type, IFieldDetail field) {

    	//System.out.print(" ---> getField ~ 1");
        if (field.isFixedFormat()) {
            return TypeManager.getSystemTypeManager().getType(type) //field.getType())
					.getField(record, field.getPos(), field);
        }
        return getCsvField(record, type, field);
     }
    
    /**
     * @deprecated internal JRecord use !!!
     */
    @Deprecated 
    public final Object getCsvField(final byte[] record, int type, IFieldDetail field) {
        if (isBinCSV()) {
        	//System.out.print(" 3 ");
        	ICsvByteLineParser byteLineParser = CsvParserManagerByte.getInstance().get(field.getRecord().getRecordStyle());
        	String value = byteLineParser.getField(field.getPos() - 1, record, 
        			new CsvDefinition(delimiter, field.getQuoteDefinition()));

        	return formatField(field,  type, value);
        } else {
	        return formatCsvField(field,  type, Conversion.toString(record, field.getFontName()));
        }  	
    }

    /**
     * @deprecated internal JRecord use !!!
     */
    @Deprecated 
    public final Object formatCsvField(IFieldDetail field,  int type, String value) {
        ICsvCharLineParser parser = field.getRecord().getCharParser();
        String val = parser.getField(field.getPos() - 1,
        		value,
        		new CsvDefinition(delimiter, field.getQuoteDefinition()));

        return formatField(field,  type, val);
    }


    private Object formatField(IFieldDetail field,  int type, String value) {

        //System.out.print(" ~ " + delimiter + " ~ " + new String(record));

        if (value != null && ! "".equals(value)) {
        	byte[] rec = Conversion.getBytes(value, field.getFontName());
            FieldDetail fldDef
        		= new FieldDetail(field.getName(), "", type,
        		        		   field.getDecimal(), field.getFontName(),
        		        		   field.getFormat(), field.getParamater());

            updateRecordInfo(field, fldDef);

            fldDef.setPosLen(1, rec.length);

//            System.out.println(" ~ " + TypeManager.getSystemTypeManager().getType(type)
//					.getField(Conversion.getBytes(value, font),
//					          1,
//					          fldDef));
            return TypeManager.getSystemTypeManager().getType(type)
					.getField(rec,
					          1,
					          fldDef);
        }
        //System.out.println();

        return "";
    }

	/**
	 * @param field
	 * @param fldDef
	 */
	public void updateRecordInfo(IFieldDetail field, FieldDetail fldDef) {
		
		fldDef.setRecord(field.getRecord());
	}

    /**
     * Set a fields value
     *
     * @param record record containg the field
     * @param field field to retrieve
     * @param value value to set
     *
     * @return byte[] updated record
     *
     */
    @Deprecated
    public byte[] setField(byte[] record, IFieldDetail field, Object value)
    {
        return setField(record, field.getType(), field, value);
    }

    /**
     * Set a fields value
     *
     * @param record record containg the field
     * @param type type to use in the conversion
     * @param field field to retrieve
     * @param value value to set
     *
     * @return byte[] updated record
     *
     */
    @Deprecated
    public byte[] setField(byte[] record, int type, IFieldDetail field, Object value)
    {
        if (field.isFixedFormat()) {
            record = TypeManager.getSystemTypeManager().getType(type)
				.setField(record, field.getPos(), field, value);
        } else  {
            record = setCsvField(record, type, field, value);
        }
        //System.out.println(" ---> setField ~ Done");
        return record;
    }

    public byte[] setCsvField(byte[] record, int type, IFieldDetail field, Object value) {
        

        Type typeVal = TypeManager.getSystemTypeManager().getType(type);
        String s = typeVal.formatValueForRecord(field, value.toString());
        //System.out.println(" ---> setField ~ " + delimiter + " ~ " + s + " ~ " + new String(record));
        CsvDefinition csvDefinition = new CsvDefinition(
        		delimiter, field.getQuoteDefinition(), ICsvDefinition.NORMAL_SPLIT, -1, field.getFontName(), false);
        		
        		//delimiter, field.getQuoteDefinition());
    	ICsvByteLineParser byteLineParser = CsvParserManagerByte.getInstance()
    			.get(field.getRecord().getRecordStyle(), isBinCSV());
    	record = byteLineParser.setFieldByteLine(
					field.getPos() - 1,
        		typeVal.getFieldType(),
        		record,
        		csvDefinition, 
        		s);
//       if  (isBinCSV()) {
//         	//record = (new BinaryCsvParser(delimiter.asByte())).updateValue(record, field, s);
//        	ICsvByteLineParser byteLineParser = CsvParserManagerByte.getInstance().get(field.getRecord().getRecordStyle());
//        	record = byteLineParser.setFieldByteLine(
// 					field.getPos() - 1,
//            		typeVal.getFieldType(),
//            		record,
//            		csvDefinition, 
//            		s);
//        } else {
//            ICsvCharLineParser parser = CsvParserManagerChar.getInstance().get(field.getRecord().getRecordStyle());
//			String font = field.getFontName();
// 			String newLine = parser.setField(
// 					field.getPos() - 1,
//            		typeVal.getFieldType(),
//            		Conversion.toString(record, font),
//            		csvDefinition, 
//            		s);
//
//            record = Conversion.getBytes(newLine, font);
//        }
        
        //System.out.println(" ---> setField ~ Done");
        return record;
    }

    /**
     * Get a field for a supplied field-name
     *
     * @param fieldName name of the field being requested
     *
     * @return field definition for the supplied name
     */
    public IFieldDetail getFieldFromName(String fieldName) {
    	//IFieldDetail ret = null;
    	String key = fieldName.toUpperCase();

    	buildFieldNameMap();

    	return fieldNameMap.get(key);
     }
    
    public Set<String> getDuplicateFieldNames() {
    	buildFieldNameMap();
    	return duplicateFieldNames;
    }

    private void buildFieldNameMap() {

    	if (fieldNameMap == null) {
    		synchronized (this) {
    			if (fieldNameMap == null) {
		    		int i, j, k, size;
		    		IFieldDetail fld;
		    		String name, nameTmp;
		
		    		size = 0;
		    		for (i = 0; i < recordCount; i++) {
		    		    size += records[i].getFieldCount();
		    		}
		    		size = Math.max(16, (size * 4) / 3 + 4);
		
		    		HashMap<String, IFieldDetail> tmpFieldNameMap = new HashMap<String, IFieldDetail>(size);
		    		recordFieldNameMap  = new HashMap<String, IFieldDetail>(size);
		    		duplicateFieldNames = new HashSet<String>(10);
		
		    		for (i = 0; i < recordCount; i++) {
		     			for (j = 0; j < records[i].getFieldCount(); j++) {
		    			    fld = records[i].getField(j);
		    			    nameTmp = fld.getName();
		    			    name = nameTmp;
		    			    nameTmp = nameTmp + "~";
		    			    k = 1;
		    			    while (tmpFieldNameMap.containsKey(name.toUpperCase())) {
		    			    	name = nameTmp + k++;
		    			    }
					    	String ucFieldName;
		    			    if (k > 1 && ! duplicateFieldNames.contains((ucFieldName = fld.getName().toUpperCase()))) {
		    			    	IFieldDetail iFieldDetail = tmpFieldNameMap.get(ucFieldName);
		    			    	if (fld.getPos() != iFieldDetail.getPos()
		    			  		|| fld.getLen() != iFieldDetail.getLen()
		    			  		|| fld.getDecimal() != iFieldDetail.getDecimal()
		    			  		|| fld.getType() != iFieldDetail.getType()
		    			    	|| fld.getFormat() != iFieldDetail.getFormat()
		    			    	|| (fld.getParamater() != null && ! fld.getParamater().equals(iFieldDetail.getParamater()))) {
		    			    		duplicateFieldNames.add(ucFieldName);
		    			    	}
		    			    }
		    			    fld.setLookupName(name);
		    			    tmpFieldNameMap.put(name.toUpperCase(), fld);
		    				recordFieldNameMap.put(
		    						records[i].getRecordName() + "." + name.toUpperCase(),
		    						fld);
		    			}
		    			records[i].setNumberOfFieldsAdded(0);
		    		}
		    		fieldNameMap = tmpFieldNameMap;
    			}
    		}
     	} else if (this.isBuildLayout()) {
     		synchronized (this) {
	     		int j;
	     		IFieldDetail fld;
	
	       		for (int i = 0; i < recordCount; i++) {
	       			if (records[i].getNumberOfFieldsAdded() > 0) {
	       				for (j = 1; j <=  records[i].getFieldCount(); j++) {
	       	   			    fld = records[i].getField(records[i].getFieldCount() - j);
	//       	   			    System.out.println("Adding ... " + (records[i].getFieldCount() - j)
	//       	   			    		+ " " + fld.getName());
	        				fieldNameMap.put(fld.getName().toUpperCase(), fld);
	        				recordFieldNameMap.put(
	        						records[i].getRecordName() + "." + fld.getName().toUpperCase(),
	        						fld);
	
	      				}
	       	    		records[i].setNumberOfFieldsAdded(0);
	      			}
	       		}
     		}
    	}
    }

	/**
	 * get the field delimiter
	 * @return the field delimeter
	 */
    @Override
	public CsvCharDetails getDelimiterDetails() {
        return delimiter;
    }




	/**
	 * get the field delimiter
	 * @return the field delimeter
	 *
	 * @deprecated use getDelimiterDetails().asBytes()
	 */
    public byte[] getDelimiterBytes() {
        return delimiter.asBytes();
    }


    /**
     * set the delimiter
     * @param delimiter new delimiter
     */
    public void setDelimiter(String delimiter) {
    	CsvCharDetails delim = CsvCharDetails.newDelimDefinition(delimiter, fontName);
    	if (this.records != null) {
    		for (int i=0; i < records.length; i++) {
    			records[i].setDelimiter(delim);
    		}
    	}
		this.delimiter = delim;
	}


	/**
     /**
     * This is a function used by the RecordEditor to get a field using
     * recalculated columns. Basically for XML copybooks,
     * the End and FollowingText columns
     * are moved from from 2 and 3 to  the end of the record.
     * Function probably does not belong here but as good as any spot.
     *
     * @param layoutIdx  record index
     * @param fieldIdx field index
     * @return requested field
     */
    public FieldDetail getAdjField(int layoutIdx, int fieldIdx) {
        return getRecord(layoutIdx)
                       .getField(getAdjFieldNumber(layoutIdx, fieldIdx));
    }


    /**
     * This is a function used by the RecordEditor to recalculate
     * columns. Basically for XML copybooks, the End and FollowingText
     * columns are moved from from 2 and 3 to  the end of the record.
     * Function probably does not belong here but as good as any spot.
     *
     * @param recordIndex record index
     * @param inColumn input column
     * @return adjusted column
     */
    public int getAdjFieldNumber(int recordIndex, int inColumn) {

	    int ret = inColumn;

	    //System.out.println("~~ " + inColumn + " " + ret + " "
	    //        + layout.getRecord(layoutIndex).getRecordName() + " " + layout.getRecord(layoutIndex).getFieldCount());

	    if (ret > 0 && getFileStructure() == Constants.IO_XML_BUILD_LAYOUT) {
	        int len = getRecord(recordIndex).getFieldCount();

            if (ret > len - 3) {
                ret -= len - 3;
            } else {
                ret += 2;
            }
	    }
	    return ret;
    }

    public int getUnAdjFieldNumber(int recordIndex, int inColumn) {

	    int ret = inColumn;

	    if (ret > 0 && getFileStructure() == Constants.IO_XML_BUILD_LAYOUT) {
	        int len = getRecord(recordIndex).getFieldCount();

            if (ret < 3) {
                ret += len - 3;
            } else {
                ret -= 2;
            }
	    }
	    return ret;
    }

    /**
     * Get a Map of all FieldNames - Field Definitions
     * @return Map of all FieldNames - Field Definitions
     */
    public Map<String, IFieldDetail> getFieldNameMap() {
    	buildFieldNameMap();
		return new HashMap<String, IFieldDetail>(fieldNameMap);
	}

    /**
     * Get a map keyed on RecordName.FieldName ~ Field Definition
     * @return Get a map keyed on RecordName.FieldName ~ Field Definition
     */
	public Map<String, IFieldDetail> getRecordFieldNameMap() {
		buildFieldNameMap();
		return new HashMap<String, IFieldDetail>(recordFieldNameMap);
	}

	/**
	 * Get Cobol Group Definition by the field name
	 * @param cobolGroupName Cobol Group Name
	 * @return List of Matching Cobol group definitions or Null
	 */
	public List<IItemDetails> getCobolGroupItems(String cobolGroupName) {
		updateCobolMaps();
		return cobolGroupName == null ? null : groupMap.get(cobolGroupName.toUpperCase());
	}

	/**
	 * Get Cobol Field/Group Definition by the field name
	 * @param cobolName Cobol Field/Group Name
	 * @return List of Matching Cobol Field/Group definitions or Null
	 */
	public List<IItemDetails> getCobolItems(String cobolName) {
		updateCobolMaps();
		return cobolName == null ? null : groupFieldMap.get(cobolName.toUpperCase());
	}

	private void updateCobolMaps() {
		if (groupMap == null) {
			synchronized (this) {
				if (groupMap == null) {
					groupMap = new HashMap<String, List<IItemDetails>>();
					groupFieldMap = new HashMap<String, List<IItemDetails>>();
					
					for (int i = 0; i < recordCount; i++) {
						records[i].updateNameCobolItemMap(groupMap, groupFieldMap);
					}
				}
			}
		}
	}

	/**
	 * @return the spaceByte
	 */
	public final byte getSpaceByte() {
		return spaceByte;
	}


	/**
	 * @return the initByte
	 */
	public final byte getInitByte() {
		return initByte;
	}

	@Override
	public final boolean isCsvLayout() {
		return csvLayout;
	}


	/**
     * Wether this is an XML Layout
     * @return is it an XML layout
     */
    public final boolean isXml() {
        return fileStructure == Constants.IO_XML_USE_LAYOUT
            || fileStructure == Constants.IO_XML_BUILD_LAYOUT;
    }

    /**
     * Wether it is ok to add Attributes to this layout
     * @return Wether it is ok to add Attributes to this layout
     */
    public final boolean isOkToAddAttributes() {
    	return fileStructure == Constants.IO_XML_BUILD_LAYOUT;
    }

    /**
     * determine wether the layout is built or not
     * @return determine wether the layout is built or not
     */
    public final boolean isBuildLayout() {
    	return fileStructure == Constants.IO_XML_BUILD_LAYOUT
    	    || fileStructure == Constants.IO_NAME_1ST_LINE;
    }


	/**
	 * check if a Tree Structure has been defined for the layout
	 * i.e. is there a hierarchy between the various layouts
	 * @return wether there is a Tree Structure defined
	 */
	public final boolean hasTreeStructure() {
		return treeStructure;
	}

	public boolean isBinCSV() {
		return	   delimiter.isBin() || getQuoteDetails().isBin();
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
	public final IFieldDetail getGroupField(String...fieldNames) {
		if (fieldNames == null || fieldNames.length == 0) {
			return null;
		}

		List<IFieldDetail> fldsFound = new ArrayList<IFieldDetail>();
		int idx = getRecordIndex(fieldNames[0]);
		if (idx >= 0 && fieldNames.length > 0) {
			return records[idx].getGroupFieldX(1, fieldNames);
		} else {
			String fldName = fieldNames[fieldNames.length-1];
			for (RecordDetail r : records) {
				fldsFound.addAll(r.getGroupFields(fieldNames));
			}
			switch (fldsFound.size()) {
			case 0: break;
			case 1: 
				return fldsFound.get(0);
			default:
				IFieldDetail fld = RecordDetail.checkForFldMatch(fldsFound, fieldNames);
				if (fld == null) {
					throw new RecordException("Found multiple fields named " + fldName + "; there should be only one");
				} else {
					return fld;
				}
			}
		}
		

		StringBuilder b = new StringBuilder();
		for (String s : fieldNames) {
			b.append('.').append(s);
		}
		throw new RecordException("No Field Found: " + b);
	}
//	
//	private IFieldDetail checkFld(IFieldDetail fld, IFieldDetail newFld, String fldName) {
//		if ( fld == null) {
//			return newFld;
//		} else if (newFld == null) {
//			return fld;
//		} else {
//			throw new RuntimeException("Found multiple fields named " + fldName + "; there should be only one");
//		}
//
//	}
}

