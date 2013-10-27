package net.sf.JRecord.External;

import java.util.ArrayList;
import java.util.HashMap;

import net.sf.JRecord.Common.BasicKeyedField;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.External.Def.AbstractConversion;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;

/**
 * This class holds conversions from external representations of types / formats etc to the internal
 * representations
 *
 *
 * @author Bruce Martin
 *
 */
public final class ExternalConversion {

	public static int USE_DEFAULT_DB = -1;
	private static AbstractConversion standardConversion ;

	private static String invalidFileChars = null;
	private static String replacementChar = " ";


	private static String[] names = new String [15] ;
	private static int[] keys = new int[15];
	static {
		int i = 0;
		keys[i] = Constants.rtBinaryRecord;		 names[i++] = "BinaryRecord";
		keys[i] = Constants.rtRecordLayout;		 names[i++] = "RecordLayout";
		keys[i] = Constants.rtDelimited;	 	 names[i++] = "Delimited";
		keys[i] = Constants.rtDelimitedAndQuote; names[i++] = "DelimitedAndQuote";
		keys[i] = Constants.RT_XML;				 names[i++] = "XML";
		keys[i] = Constants.rtGroupOfRecords; 	 names[i++] = "GroupOfRecords";
		keys[i] = Constants.rtGroupOfBinaryRecords; names[i++] = "GroupOfBinaryRecords";
		keys[i] = Constants.rtFixedLengthRecords;   names[i++] = "FixedLengthRecords";
		keys[i] = Constants.NULL_INTEGER;		 names[i++] = null;
	}


/*	private static HashMap<String, Integer>[] typeConv = new HashMap[Constants.NUMBER_OF_COPYBOOK_SOURCES];
	private static HashMap<String, Integer>[] formatConv = new HashMap[Constants.NUMBER_OF_COPYBOOK_SOURCES];
	static {
		for (int i = 0; i < typeConv.length; i++) {
			typeConv[i]   = null;
			formatConv[i] = null;
		}
	}*/

	/**
	 * Convert a String (Type Name) to a TypeId
	 * @param dbIdx Database id
	 * @param typeStr Type represented as a char
	 * @return type value
	 */
	public final static int getType(int dbIdx, String typeStr) {
		int type = Constants.NULL_INTEGER;

		try {
			type = Integer.parseInt(typeStr);
		} catch (Exception e) {	}

		if (type < 0) {
			type = getStandardConversion().getType(dbIdx,typeStr);
		}

		if (type < 0) {
			type = Type.ftChar;
		}

		return type;
	}

	/**
	 * Convert a RecordType name to RecordType Id
	 * @param dbIdx DB index (used in recordEditor)
	 * @param typeStr RecordType name
	 * @return Record Type Id
	 */
	public final static int getRecordType(int dbIdx, String typeStr) {
		int type = Constants.NULL_INTEGER;

	   	for (int i = 0; i < keys.length && keys[i] != Constants.NULL_INTEGER; i++) {
    		if (names[i].equalsIgnoreCase(typeStr)) {
    			return keys[i];
    		}
    	}

		try {
			type = Integer.parseInt(typeStr);
		} catch (Exception e) {	}

		return type;
	}


	/**
	 * Get the RecordStyle Id from its name
	 *
	 * @param dbIdx DB index
	 * @param styleStr RecordStyle name
	 * @return RecordStyle
	 */
	public final static int getRecordStyle(int dbIdx, String styleStr) {
		int type = Constants.NULL_INTEGER;

		try {
			type = Integer.parseInt(styleStr);
		} catch (Exception e) {	}

		return type;
	}


	/**
	 * Get File Structure from its name
	 *
	 * @param dbIdx DB index (RecordEditor)
	 * @param fileStructureStr File Structure name
	 *
	 * @return file Structure
	 */
	public final static int getFileStructure(int dbIdx, String fileStructureStr) {
		int fileStructure = Constants.NULL_INTEGER;

		fileStructure = LineIOProvider.getInstance().getStructure(fileStructureStr);

		if (fileStructure == Constants.NULL_INTEGER) {
			try {
				fileStructure = Integer.parseInt(fileStructureStr);
			} catch (Exception e) {	}
		}

		return fileStructure;
	}

	/**
	 * Convert a Type to a Type Name
	 * @param dbIdx DB index
	 * @param type Type Id
	 * @return Type name
	 */
	public final static String getTypeAsString(int dbIdx, int type) {
		String typeStr = "";

		try {
			typeStr = getStandardConversion().getTypeAsString(dbIdx, type);
		}  catch (Exception e) {
			System.out.println(">> " + dbIdx + ": " + type );
			e.printStackTrace();
		}

		return typeStr;
	}


	/**
	 * Convert a Format Name to a Format Id (Formats are used by the Record Editor)
	 *
	 * @param dbIdx Database id
	 * @param formatStr Format represented as a char
	 * @return Format value
	 */
	public final static int getFormat(int dbIdx, String formatStr) {
		int format = Constants.NULL_INTEGER;

		try {
			format = Integer.parseInt(formatStr);
		} catch (Exception e) {	}

		if (format < 0) {
			format = getStandardConversion().getFormat(dbIdx,formatStr);
		}

		if (format < 0) {
			format = 0;
		}

		return format;
	}


	/**
	 * Convert a Format Id to a Format Name
	 * @param dbIdx Database index
	 * @param format format id
	 * @return Format Name
	 */
	public final static String getFormatAsString(int dbIdx, int format) {
		String formatStr = "";


		if (format > 0) {
			formatStr = getStandardConversion().getFormatAsString(dbIdx,format);
		} else {
			formatStr = "";
		}

		return formatStr;
	}


	/**
	 * Convert File Structure to File Structure name
	 *
	 * @param dbIdx DB index
	 * @param fileStructure file Structure Id
	 * @return FileStructure name
	 */
	public final static String getFileStructureAsString(int dbIdx, int fileStructure) {
		String fileStructureStr = "";

		fileStructureStr = LineIOProvider.getInstance().getStructureName(fileStructure);

		if ("".equals(fileStructureStr) && fileStructure >= 0) {
			try {
				fileStructureStr = Integer.toString(fileStructure);
			} catch (Exception e) {	}
		}

		return fileStructureStr;
	}


	/**
	 * Convert Record Style to its equivalent name
	 * @param dbIdx DB index
	 * @param fileStructure file-Structure Id
	 * @return File-Structure name
	 */
	public final static String getRecordStyleAsString(int dbIdx, int fileStructure) {
		String recordStyleStr = "";

		try {
			recordStyleStr = Integer.toString(fileStructure);
		} catch (Exception e) {	}

		if (fileStructure < 0) {
			recordStyleStr = "";
		}

		return recordStyleStr;
	}

	/**
	 * Convert Record-Type id to Record-Type name
	 *
	 * @param dbIdx database Index
	 * @param recordType Record-Type id
	 *
	 * @return Record-Type Name
	 */
	public final static String getRecordTypeAsString(int dbIdx, int recordType) {
		String recordTypeStr = "";

    	for (int i = 0; i < keys.length && keys[i] != Constants.NULL_INTEGER; i++) {
    		if (keys[i] == recordType) {
    			return names[i];
    		}
    	}

		try {
			recordTypeStr = Integer.toString(recordType);
		} catch (Exception e) {	}

		if (recordType < 0) {
			recordTypeStr = "";
		}

		return recordTypeStr;
	}



	/**
	 * set the standard conversion
	 * @param newStandardConversion the standardConversion to set
	 */
	public final static void setStandardConversion(AbstractConversion newStandardConversion) {
		standardConversion = newStandardConversion;
	}


	/**
	 * Change characters that are invalid in file names to a valid character
	 * @param name filename
	 * @return fixed filename
	 */
    public static final String copybookNameToFileName(String name) {

    	StringBuilder b = new StringBuilder(name);
    	if (invalidFileChars != null) {
    		for (int i = 0; i < invalidFileChars.length(); i++) {
    			Conversion.replace(b, invalidFileChars.substring(i, i+1), replacementChar);
    		}
    	}
		Conversion.replace(b, "\t", " ");
		Conversion.replace(b, "\n", " ");

		return b.toString();
    }

    /**
     * Get all Type Ids / Type Names
     * @return Type Ids / Type Names
     */
    public final static ArrayList<BasicKeyedField> getTypes(int idx) {
		TypeManager manager = TypeManager.getInstance();
    	AbstractConversion conv = getStandardConversion();
    	ArrayList<BasicKeyedField> ret = new ArrayList<BasicKeyedField>(40);
    	BasicKeyedField fld;
    	int t;
    	String s;

    	for (int i = 0; i < manager.getNumberOfTypes(); i++) {
    		t = manager.getTypeId(i);
    		s = conv.getTypeAsString(idx, t);
    		if (s != null && ! "".equals(s)) {
    			fld = new BasicKeyedField();
    			fld.key = t;
    			fld.name = s;
    			fld.valid = Boolean.valueOf(conv.isValid(idx, t));
    			ret.add(fld);
    		}
    	}

    	return ret;
    }

    /**
     * Get all Record-Type Id's / Record-Type Names
     * @return Record-Type Id's / Record-Type Names
     */
    public final static ArrayList<BasicKeyedField> getRecordTypes() {
    	ArrayList<BasicKeyedField> ret = new ArrayList<BasicKeyedField>();
    	BasicKeyedField fld;

    	for (int i = 0; i < names.length && ! "".equals(names[i]); i++) {
 			fld = new BasicKeyedField();
			fld.key = keys[i];
			fld.name = names[i];
			ret.add(fld);
    	}

    	return ret;
    }

	/**
	 * Define the invalid File name characters and there replacement Chars
	 *
	 * @param invalidFileCharacters the invalidFileChars to set
	 * @param replacementCharacter Characters to replace the invalid Characters
	 */
	public final static void setInvalidFileChars(String invalidFileCharacters, String replacementCharacter) {
		invalidFileChars = invalidFileCharacters;

		if (replacementChar != null) {
			replacementChar = replacementCharacter;
		}
	}


	public final static String fixDirectory(String directory) {
		String ret = directory;

		if (! ("".equals(ret))) {
			if (ret.endsWith("*")) {
				ret = ret.substring(0, ret.length()-2);
			}
			if (! (ret.endsWith("/") || ret.endsWith("\\"))) {
				ret = ret + Constants.FILE_SEPERATOR;
			}
		}
		return ret;
	}

	public final static String fixFileName(String filename) {
		String ret = filename;

		if (ret != null &&! ("".equals(ret))) {
			StringBuilder b = new StringBuilder(filename);
			Conversion.replace(b, ":", " ");
			Conversion.replace(b, "*", " ");
			ret = b.toString();
		}
		return ret;
	}

	/**
	 * Get the Conversion class
	 * @return Conversion class
	 */
	private static AbstractConversion getStandardConversion() {

		if (standardConversion == null) {
			standardConversion = new BasicConversion();
		}
		return standardConversion;
	}


	/**
	 * Basic conversion class for use by JRecord
	 * @author Bruce Martin
	 *
	 */
	private static class BasicConversion implements AbstractConversion {

		private String[] typeNames ;
		private HashMap<String, Integer> typeNumbers;

		/**
		 * Basic Type / Format conversion (for use in JRecord; RecordEditor has
		 * its own (database based conversion).
		 */
		public BasicConversion() {
			TypeManager manager = TypeManager.getInstance();
			typeNames = new String[manager.getNumberOfTypes()];
			typeNumbers = new HashMap<String, Integer>(manager.getNumberOfTypes() * 2);

			for (int i=0; i < typeNames.length; i++) {
				typeNames[i] = "";
			}

			setName(Type.ftChar  , "Char");
			setName(Type.ftNumAnyDecimal  , "NumAnyDecimal");
			setName(Type.ftPositiveNumAnyDecimal  , "PositiveNumAnyDecimal");
			setName(Type.ftCharRightJust      , "Char (right justified)");
			setName(Type.ftCharNullTerminated , "Char Null terminated");
			setName(Type.ftCharNullPadded     , "Char Null padded");
			setName(Type.ftHex                , "Hex Field");
			setName(Type.ftNumLeftJustified   , "Num (Left Justified)");
			setName(Type.ftNumRightJustified  , "Num (Right Justified space padded)");
			setName(Type.ftNumRightJustifiedPN, "Num (Right Justified space padded) +/- sign");
			setName(Type.ftNumRightJustCommaDp, "Num (Right Just space padded, \",\" Decimal)");
			setName(Type.ftNumRightJustCommaDpPN, "Num (Right Just space padded, \",\" Decimal) +/- sign");
			setName(Type.ftNumZeroPadded      , "Num (Right Justified zero padded)");
			setName(Type.ftNumZeroPaddedPN    , "Num (Right Justified zero padded +/- sign)");
			setName(Type.ftAssumedDecimal         , "Num Assumed Decimal (Zero padded)");
			setName(Type.ftAssumedDecimalPositive , "Num Assumed Decimal (+ve)");
			setName(Type.ftNumZeroPaddedPositive  , "Num (Right Justified zero padded positive)");
			setName(Type.ftNumCommaDecimal        , "Zero Padded Number decimal=\",\"");
			setName(Type.ftNumCommaDecimalPN      , "Zero Padded Number decimal=\",\" sign=+/-");
			setName(Type.ftNumCommaDecimalPositive, "Num (Right Justified zero padded positive)");


			setName(Type.ftSignSeparateLead   , "Num Sign Separate Leading");
			setName(Type.ftSignSeparateTrail  , "Num Sign Separate Trailing");
			setName(Type.ftDecimal            , "Decimal");
			setName(Type.ftBinaryInt          , "Binary Integer");
			setName(Type.ftBinaryIntPositive  , "Binary Integer (only +ve)");
			setName(Type.ftPostiveBinaryInt   , "Postive Binary Integer");
			setName(Type.ftFloat              , "Float");
			setName(Type.ftDouble             , "Double");
			setName(Type.ftBit  , "Bit");
			setName(Type.ftPackedDecimal         , "Mainframe Packed Decimal (comp-3)");
			setName(Type.ftPackedDecimalPostive  , "Mainframe Packed Decimal (+ve)");
			setName(Type.ftZonedNumeric  , "Mainframe Zoned Numeric");
			typeNumbers.put("Binary Integer Big Edian (Mainframe, AIX etc)".toLowerCase(), Integer.valueOf(Type.ftBinaryBigEndian ));
			setName(Type.ftBinaryBigEndian  , "Binary Integer Big Endian (Mainframe, AIX etc)");
			setName(Type.ftBinaryBigEndianPositive  , "Binary Integer Big Endian (only +ve)");
			setName(Type.ftPositiveBinaryBigEndian  , "Positive Integer Big Endian");
			setName(Type.ftFjZonedNumeric  , "Fujitsu Zoned Numeric");

			setName(Type.ftRmComp, "Rm Cobol Comp");
			setName(Type.ftRmCompPositive  , "Rm Cobol Comp (+ve)");
			setName(Type.ftCheckBoxBoolean , "Check Box (Boolean)");

			setName(Type.ftDate  , "Date - Format in Parameter field");
			setName(Type.ftDateYMD  , "Date - YYMMDD");
			setName(Type.ftDateYYMD  , "Date - YYYYMMDD");
			setName(Type.ftDateDMY  , "Date - DDMMYY");
			setName(Type.ftDateDMYY  , "Date - DDMMYYYY");
			setName(Type.ftCheckBoxTrue  , "Check Box True / Space");
			setName(Type.ftCheckBoxYN  , "Checkbox Y/N");
			setName(Type.ftCheckBoxTF  , "Checkbox T/F");
			setName(Type.ftCsvArray  , "CSV array");
			setName(Type.ftXmlNameTag  , "XML Name Tag");
			setName(Type.ftMultiLineEdit  , "Edit Multi Line field");

			setName(Type.ftCharRestOfFixedRecord  , "Char Rest of Fixed Length");
			setName(Type.ftCharRestOfRecord  , "Char Rest of Record");

		}

		/**
		 * Set The type name
		 * @param type type Id
		 * @param name Type Name
		 */
		private void setName(int type, String name) {
			typeNames[TypeManager.getInstance().getIndex(type)] = name;
			typeNumbers.put(name.toLowerCase(), Integer.valueOf(type));
		}


		@Override
		public int getFormat(int idx, String format) {
			return 0;
		}

		@Override
		public String getFormatAsString(int idx, int format) {
			// TODO Auto-generated method stub
			return Integer.toString(format);
		}

		@Override
		public int getType(int idx, String type) {
			String key = type.toLowerCase();
			if (typeNumbers.containsKey(key)) {
				return (typeNumbers.get(key)).intValue();
			}
			return 0;
		}

		@Override
		public String getTypeAsString(int idx, int type) {
			String s = typeNames[TypeManager.getInstance().getIndex(type)];
			if (s == null || "".equals(s)) {
				s = Integer.toString(type);
			}
			return s;
		}

		/* (non-Javadoc)
		 * @see net.sf.JRecord.External.Def.AbstractConversion#isValid(int, int)
		 */
		@Override
		public boolean isValid(int idx, int type) {
			String s = typeNames[TypeManager.getInstance().getIndex(type)];
			return s != null && ! "".equals(s);
		}

	}
}
