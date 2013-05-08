package net.sf.JRecord.Numeric;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Types.Type;

/**
 * Standard Cobol Type to JRecord Type conversion class.
 *
 * @author Bruce Martin
 *
 */public class BasicConvert implements Convert {

    private int identifier;

    private int binId;
    private boolean usePositiveInteger;

    private int defaultVbFileStructure = Constants.IO_DEFAULT;

    // Using Object instead of BasicNumericDefinition to avoid dependency on cb2xml.
    // It allows the class to be used in RecordEditor Edit Properties without cb2xml or with an old
    // cb2xml. Most user's of the RecordEditor probably do not use Cobol so why make the dependancy.
    private Object numericDefinition;

    private String name;

    public BasicConvert(int id, String name, int binaryId, int[] binarySizes, boolean usePositive) {
    	this(id, name, binaryId, binarySizes, null, usePositive, 4, 8);
    }

    public BasicConvert(int id, String binName, int binaryId, int[] binarySizes, int[] SynchronizeAt,
    		boolean usePositive, int floatSynchronize, int doubleSynchronize) {

    	name = binName;
    	try {
    		numericDefinition = new net.sf.cb2xml.def.BasicNumericDefinition(
    				binName, binarySizes, SynchronizeAt, usePositive, floatSynchronize, doubleSynchronize
    		);
    	} catch (NoClassDefFoundError e) {
			System.out.println("Class Not Found: " + e.getMessage());
 		}
    	identifier = id;


        binId = binaryId;

    }



    /**
     * This method will convert a Cobol Definition into a JRecord type Id
     * @param usage Cobol usage (i.e. Comp etc)
     * @param picture Cobol picture
     * @param signed wether it is a signed field
     * @return JRecord type code
     */
    public int getTypeIdentifier(String usage, String picture, boolean signed,
			boolean signSeperate, String signPosition) {
    	int lType = -121;

    	picture = picture.toUpperCase();
    	boolean positive = ! (signed || picture.startsWith("S"));

    	if (picture.startsWith("-9(") || picture.startsWith("+++9") || picture.startsWith("+(2)9")) {
    		lType = -121;
    	}
        if ("computational".equals(usage)
        || "computational-4".equals(usage)
        || "computational-5".equals(usage)
        || "computational-6".equals(usage)
        || "binary".equals(usage)) {
        	if (binId == Convert.FMT_MAINFRAME
           	||  binId == Convert.FMT_FUJITSU
           	||  binId == Convert.FMT_BIG_ENDIAN) {
                 lType = Type.ftBinaryBigEndian;
                 if (positive) {
                	 lType = Type.ftBinaryBigEndianPositive;
	                 if (usePositiveInteger) {
	                	 lType = Type.ftPositiveBinaryBigEndian;
	            	 }
                 }
            } else {
                lType = Type.ftBinaryInt;
                if (positive) {
                    lType = Type.ftBinaryIntPositive;
                    if (usePositiveInteger) {
                    	lType = Type.ftPostiveBinaryInt;
                    }
                }
            }
        } else if ("computational-3".equals(usage)) {
            lType = Type.ftPackedDecimal;
            if (positive) {
            	lType = Type.ftPackedDecimalPostive;
            }
        } else if ("computational-1".equals(usage)) {
            lType = Type.ftFloat;
        } else if ("computational-2".equals(usage)) {
            lType = Type.ftDouble;
        } else if (! CommonCode.checkPictureNumeric(picture, '.')) {
        	return Type.ftChar;
        } else if (picture.indexOf('9') >= 0
//        		&& picture.indexOf('V') < 0
//        		&& picture.indexOf('Z') < 0
//        		&& picture.indexOf(',') < 0
//        		&& (! picture.startsWith("S"))
        		&& (picture.startsWith("-") || picture.startsWith("+") || picture.startsWith("9"))
        		&& CommonCode.checkPicture(picture, '9', '.')
        ) {
        	if (picture.startsWith("-")) {
        		lType = Type.ftNumZeroPadded;
        	} else if (picture.startsWith("9")) {
        		lType = Type.ftNumZeroPaddedPositive;
        	} else if (picture.startsWith("+")) {
        		lType = Type.ftNumZeroPaddedPN;
        	} else {
        		lType = chkRest(lType, usage, picture, signed, signSeperate, signPosition);
        	}
         } else {
     		lType = chkRest(lType, usage, picture, signed, signSeperate, signPosition);
        }
        return lType;
    }

    private int chkRest(int lType, String usage, String picture, boolean signed,
			boolean signSeperate, String signPosition) {
    	if (picture.startsWith("+") && CommonCode.checkPicture(picture, '+', '.')) {
    		lType = Type.ftNumRightJustifiedPN;
    	} else if (picture.indexOf('Z') >= 0
    			||  picture.indexOf('-') >= 0
    	    	||  picture.indexOf('+') >= 0
    			||  picture.indexOf('.') >= 0) {
    		lType = Type.ftNumRightJustified;
    	} else {
    		lType = CommonCode.commonTypeChecks(binId, usage, picture, signed, signSeperate, signPosition);
    	}

    	return lType;
    }



    /**
     * Get the conversion Identifier
     * @return conversion Identifier
     */
    public int getIdentifier() {
        return identifier;
    }


    /**
     * Get the binary id to use
     * @return actual binary Id
     */
    public int getBinaryIdentifier() {
    	return binId;
    }

     /**
      * wether positive numbers should be represented by positive integers
      * @return if positive integers are use
      */
    public boolean isPositiveIntAvailable() {
    	 return usePositiveInteger;
     }

	/**
	 * @see net.sf.JRecord.Numeric.Convert#getNumericDefinition()
	 */
	@Override
	public Object getNumericDefinition() {
		return numericDefinition;
	}

	/**
	 * @return the name
	 */
	public final String getName() {
		return name;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Numeric.Convert#getFileStructure(boolean, boolean)
	 */
	@Override
	public int getFileStructure(boolean multipleRecordLengths, boolean binary) {
		if (multipleRecordLengths && binary) {
			return defaultVbFileStructure;
		}
		return Constants.IO_DEFAULT;
	}

	/**
	 * @param defaultFileStructure the defaultFileStructure to set
	 */
	public Convert setDefaultVbFileStructure(int defaultFileStructure) {
		this.defaultVbFileStructure = defaultFileStructure;
		return this;
	}
}
