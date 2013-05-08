package net.sf.JRecord.Numeric;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Types.Type;
import net.sf.cb2xml.def.BasicNumericDefinition;
import net.sf.cb2xml.def.NumericDefinition;

/**
 * Basic Definition of Binary Types Sizes for Cobol:
 *
 * Typical Usage:
 *
 * BasicNumericDefinition("Mainframe", BasicNumericDefinition.MAINFRAME_SIZES, BasicNumericDefinition.MAINFRAME_SYNC, false, 4, 4)
 * BasicNumericDefinition("Fujitsu", BasicNumericDefinition.FUJITSU_SIZES, BasicNumericDefinition.FUJITSU_SYNC, true, 4, 8)
 * BasicNumericDefinition("Open Cobol", BasicNumericDefinition.OPEN_COBOL_SIZES, BasicNumericDefinition.OPEN_COBOL_SYNC, true, 4, 8)
 * BasicNumericDefinition("Open Cobol bs2000", BasicNumericDefinition.BS2000_SIZES, BasicNumericDefinition.BS2000_SYNC, true, 4, 8)
 * BasicNumericDefinition("Open Cobol MVS", BasicNumericDefinition.OPEN_COBOL_MVS_SIZES,
 *                                    BasicNumericDefinition.OPEN_COBOL_MVS_SYNC, true, 4, 8)
 * BasicNumericDefinition("Open Cobol Micro Focus", BasicNumericDefinition.MICROFOCUS_SIZES,
 *                                     BasicNumericDefinition.MICROFOCUS_SIZES_SYNC, , true, 1, 1)
 *
 *
 * @author bm
 *
 */
public class GenericNumericDefinition implements NumericDefinition, Convert {

	public static int PIC9   = 0;
	public static int BINARY = 2;
	public static int COMP_0 = 2;
	public static int COMP_1 = 3;
	public static int COMP_2 = 4;
	public static int COMP_3 = 5;
	public static int COMP_4 = 6;
	public static int COMP_5 = 7;
	public static int COMP_6 = 8;
	public static int COMP_7 = 9;
	public static int COMP_8 = 10;

	// initialise for mainframe
	private int[] compSizesUsed ;
	private int[] syncSizesUsed, syncDigitsAvailable ;
	private int[] digitsAvailable ;
	private int[] positiveDigitsAvailable ;
	private int[] positiveSyncDigitsAvailable ;
	private int[] syncPos ;


	private int[] posTypes;
	private int[] types;

	private int floatSync =4;
	private int doubleSync = 8;

    private final int identifier;

    private final int binId;

	private String name;


    private int defaultVbFileStructure = Constants.IO_DEFAULT;


	public GenericNumericDefinition(int id, int binaryId,
			String encodingName,
			int[] sizes, int[] syncSizes, int[] syncPosition,
			int[] stdTypes, int[] positiveTypes,
			int floatSyncAt,  int doubleSyncAt) {
		int idx;
		int[] tmpPositive = new int[sizes.length];
		int[] tmpSyncPositive;
		compSizesUsed = new int[sizes.length];



    	identifier = id;
        binId = binaryId;
		types = stdTypes;
		posTypes = positiveTypes;
		if (posTypes == null) {
			posTypes = types;
		}

		name = encodingName;

		syncPos = BasicNumericDefinition.DEFAULT_SYNC;
		if (syncPosition != null) {
			syncPos = syncPosition;
		}

		digitsAvailable = new int[sizes.length];
		for (int i = 0; i < sizes.length; i++) {
			 compSizesUsed[i] = sizes[i];
			 //System.out.println("Size " + i + " " + sizes[i]);
			 idx = Math.min(BasicNumericDefinition.MAX_COMP_SIZE.length, sizes[i]) - 1;
			 digitsAvailable[i] = BasicNumericDefinition.MAX_COMP_SIZE[idx];
			 tmpPositive[i] = BasicNumericDefinition.MAX_POSITIVE_COMP_SIZE[idx];
		}

		tmpSyncPositive = tmpPositive;
		syncSizesUsed = compSizesUsed;
		if (syncSizes != null) {
			syncSizesUsed = new int[syncSizes.length];
			syncDigitsAvailable = new int[syncSizes.length];
			tmpPositive = new int[syncSizes.length];
			for (int i = 0; i < syncSizes.length; i++) {
				 syncSizesUsed[i]
				               = syncSizes[i];
				 idx = Math.min(BasicNumericDefinition.MAX_COMP_SIZE.length, syncSizesUsed[i]) - 1;
				 syncDigitsAvailable[i] = BasicNumericDefinition.MAX_COMP_SIZE[idx];
				 tmpSyncPositive[i] = BasicNumericDefinition.MAX_POSITIVE_COMP_SIZE[idx];
			}
		}

		positiveDigitsAvailable = digitsAvailable;
		if (posTypes != types) {
			positiveDigitsAvailable = tmpPositive;
			positiveSyncDigitsAvailable = tmpSyncPositive;
		}

		floatSync = floatSyncAt;
		doubleSync =doubleSyncAt;
	}

    public String getName() {
    	return name;
    }


	public int getBinarySize(String usage, int numDigits, boolean positive, boolean sync) {
		int storageLength = numDigits;
		boolean pos = positive;
		switch (getTypeCode(usage, positive)) {
		case Type.ftFloat:
        	storageLength = 4;
        	break;
		case Type.ftDouble:
	       	storageLength = 8;
	       	break;
		case Type.ftPackedDecimal:
		case Type.ftPackedDecimalPostive:
            storageLength = (numDigits) / 2 + 1;
            break;
		case Type.ftDecimal:
            storageLength = (numDigits) / 2;
            break;
		case Type.ftRmComp:
			storageLength = numDigits + 1;
			break;
		case Type.ftBinaryBigEndianPositive:
		case Type.ftBinaryIntPositive:
			pos = false;
			// deliberate fall through
		case Type.ftBinaryBigEndian:
		case Type.ftBinaryInt:
		case Type.ftPositiveBinaryBigEndian:
		case Type.ftPostiveBinaryInt:
			if (sync) {
				storageLength = getBinSizes(numDigits, pos, syncSizesUsed, syncDigitsAvailable, positiveSyncDigitsAvailable);
			} else {
				storageLength = getBinSizes(numDigits, pos, compSizesUsed, digitsAvailable, positiveDigitsAvailable);
			}
//        	storageLength = compSizesUsed[compSizesUsed.length - 1];
//        	for (int i = 0; i < digitsAvailable.length - 1; i++) {
//        		if (digitsAvailable[i] >= numDigits
//        		|| (positive && positiveDigitsAvailable[i] >= numDigits)) {
//        			storageLength = compSizesUsed[Math.max(0, i)];
//        			break;
//        		}
//        	}

        }

		System.out.println(" >> SL " + usage + " " + numDigits + " " + positive + " " +
				getTypeCode(usage, positive) + " >> " + storageLength);
        return storageLength;
	}

	protected final int getBinSizes(int numDigits, boolean positive,
			int[] compSizes,
			int[] digits,
			int[] positiveDigits) {
    	int storageLength = compSizes[compSizes.length - 1];
    	for (int i = 0; i < digits.length - 1; i++) {
    		if (digits[i] >= numDigits
    		|| (positive && positiveDigits[i] >= numDigits)) {
    			storageLength = compSizes[Math.max(0, i)];
    			break;
    		}
    	}

    	return storageLength;
	}

	public int getSyncAt(String usage, int actualLength) {
		int syncOn = 1;
		if  (isBinaryInt(usage)) {
			switch (actualLength) {
				case (1) : syncOn = syncPos[0];	break;
				case (2) : syncOn = syncPos[1];	break;
				case (3) :
				case (4) : syncOn = syncPos[2];	break;
				default  : syncOn = syncPos[3];
			}
		} else {
			switch (getTypeCode(usage, false)) {
			case Type.ftFloat:
				syncOn = floatSync;
				break;
			case Type.ftDouble:
				syncOn = doubleSync;
			}
		}
		return syncOn;
	}


	public int chkStorageLength(int storageLength, String usage) {
		int ret = storageLength;
		if (storageLength == 0) {
			switch (getTypeCode(usage, false)) {
			case Type.ftFloat:
				ret = 10;
				break;
			case Type.ftDouble:
				ret = 18;
			}
		}

		return ret;
	}

	private boolean isBinaryInt(String usage) {
		boolean ret = false;
		switch (getTypeCode(usage, false)) {
		case Type.ftBinaryBigEndian:
		case Type.ftBinaryInt:
		case Type.ftBinaryBigEndianPositive:
		case Type.ftBinaryIntPositive:
		case Type.ftPositiveBinaryBigEndian:
		case Type.ftPostiveBinaryInt:
			ret = true;
		}

		return ret;
	}



    /**
     * Get the conversion Identifier
     * @return conversion Identifier
     */
	@Override
    public int getIdentifier() {
        return identifier;
    }


    /**
     * Get the binary id to use
     * @return actual binary Id
     */
	@Override
    public int getBinaryIdentifier() {
    	return binId;
    }

	@Override
	public Object getNumericDefinition() {
		return this;
	}

	@Override
	public int getTypeIdentifier(String usage, String picture, boolean signed,
			boolean signSeperate, String signPosition) {
		int code = getBinCode(usage);
		int ret = 0;

		picture = picture.toUpperCase();
		if (code >= 0) {
			ret = getTypeCode(code, ! signed);
		} else if (picture.indexOf('Z') >= 0
               ||  picture.indexOf('-') >= 0
               ||  picture.indexOf('+') >= 0
               ||  picture.indexOf('.') >= 0) {
        	ret = Type.ftNumRightJustified;
        } else {
        	ret = CommonCode.commonTypeChecks(binId, usage, picture, signed, signSeperate, signPosition);
        }

		return ret;
	}

	public int getTypeCode(String usage, boolean positive) {
		return getTypeCode(getBinCode(usage), positive);
	}


	public int getTypeCode(int code, boolean positive) {
		int ret = types[code];
		if (positive) {
			ret = posTypes[code];
		}

		return ret;
	}

	protected final int getBinCode(String usage) {
		int ret = PIC9;
		if ("computational".equals(usage)) {
			ret = COMP_0;
		} else if ("computational-1".equals(usage)) {
			ret = COMP_1;
		} else if ("computational-2".equals(usage)) {
			ret = COMP_2;
		} else if ("computational-3".equals(usage)) {
			ret = COMP_3;
		} else if ("computational-4".equals(usage)) {
			ret = COMP_4;
		} else if ("computational-5".equals(usage)) {
			ret = COMP_5;
		} else if ("computational-6".equals(usage)) {
			ret = COMP_6;
		} else if ("computational-7".equals(usage)) {
			ret = COMP_7;
		} else if ("computational-8".equals(usage)) {
			ret = COMP_8;
		} else if ("binary".equals(usage)) {
			ret = BINARY;
		}

		return ret;
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
