/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
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
      
package net.sf.JRecord.Numeric;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Types.Type;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.NumericDefinition;

/**
 * Standard Cobol Type to JRecord Type conversion class.
 *
 * @author Bruce Martin
 *
 */public class BasicConvert implements Convert {

    private final int identifier;

    private final int binId;
    private final boolean usePositiveInteger;

    private int defaultVbFileStructure = Constants.IO_DEFAULT;

    // Using Object instead of BasicNumericDefinition to avoid dependency on cb2xml.
    // It allows the class to be used in RecordEditor Edit Properties without cb2xml or with an old
    // cb2xml. Most user's of the RecordEditor probably do not use Cobol so why make the dependancy.
    private NumericDefinition numericDefinition;

    private String name;

    public BasicConvert(int id, String name, int binaryId, int[] binarySizes, boolean usePositive) {
    	this(id, name, binaryId, binarySizes, null, usePositive, 4, 8);
    }

    public BasicConvert(int id, String binName, int binaryId, int[] binarySizes, int[] SynchronizeAt,
    		boolean usePositive, int floatSynchronize, int doubleSynchronize) {

    	this.name = binName;
    	this.usePositiveInteger = usePositive;
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
        if (Cb2xmlConstants.COMP.equalsIgnoreCase(usage)
        || Cb2xmlConstants.COMP_4.equalsIgnoreCase(usage) 
        || Cb2xmlConstants.COMP_5.equalsIgnoreCase(usage)
        || Cb2xmlConstants.COMP_6.equalsIgnoreCase(usage)
        || Cb2xmlConstants.BINARY.equalsIgnoreCase(usage)) {
        	if (binId == ICopybookDialects.FMT_MAINFRAME
           	||  binId == ICopybookDialects.FMT_FUJITSU
           	||  binId == ICopybookDialects.FMT_BIG_ENDIAN) {
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
        } else if (Cb2xmlConstants.COMP_3.equalsIgnoreCase(usage) || Cb2xmlConstants.PACKED_DECIMAL.equalsIgnoreCase(usage)) {
            lType = Type.ftPackedDecimal;
            if (positive) {
            	lType = Type.ftPackedDecimalPostive;
            }
        } else if (Cb2xmlConstants.COMP_1.equalsIgnoreCase(usage)) {
            lType = Type.ftFloat;
        } else if (Cb2xmlConstants.COMP_2.equalsIgnoreCase(usage)) {
            lType = Type.ftDouble;
        } else if (! CommonCode.checkPictureNumeric(picture, '.')) {
        	return Type.ftChar;
        } else if (picture.indexOf('9') >= 0
//        		&& picture.indexOf('V') < 0
//        		&& picture.indexOf('Z') < 0
//        		&& picture.indexOf(',') < 0
//        		&& (! picture.startsWith("S"))
        		&& (picture.startsWith("-") || picture.startsWith("+") || picture.startsWith("9")
        				|| picture.endsWith("-") || picture.endsWith("+"))
        		&& CommonCode.checkPicture(picture, '9', '.', 'V')
        ) {
        	if (picture.startsWith("-")) {
        		lType = Type.ftNumZeroPadded;
        		if (picture.indexOf('V') >= 0) {
        			lType = Type.ftSignSeparateLead;
//         		} else if (picture.indexOf('.') >= 0) {
//         			lType = Type.ftSignSepLeadActualDecimal;
         		}
        	} else if (picture.startsWith("+")) {
        		lType = Type.ftNumZeroPaddedPN ;
          		if (picture.indexOf('V') >= 0) {
        			lType = Type.ftSignSeparateLead;
//         		} else if (picture.indexOf('.') >= 0) {
//         			lType = Type.ftSignSepLeadActualDecimal;
         		}
        	} else if (picture.endsWith("-") || picture.endsWith("+")) {
       			lType = Type.ftSignSeparateTrail;
       			if (picture.indexOf('.') >= 0) {
         			lType = Type.ftSignSepTrailActualDecimal;
         		}
        	} else if (picture.startsWith("9") && (picture.indexOf('V') < 0)) {
        		lType = Type.ftNumZeroPaddedPositive;
        	} else {
        		lType = chkRest(lType, usage, picture, signed, signSeperate, signPosition);
        	}
         } else {
//        	 System.out.println(">" + picture + "< " + picture.indexOf('9') + " " + picture.startsWith("9") + " " + picture.endsWith("+"));
     		lType = chkRest(lType, usage, picture, signed, signSeperate, signPosition);
        }
        return lType;
    }

    private int chkRest(int lType, String usage, String picture, boolean signed,
			boolean signSeperate, String signPosition) {
    	if (picture.startsWith("+") && CommonCode.checkPicture(picture, '+', '.', 'V')) {
    		lType = Type.ftNumRightJustifiedPN;
    	} else if (picture.indexOf('Z') >= 0
    			||  picture.indexOf('-') >= 0
    	    	||  picture.indexOf('+') >= 0
    			||  picture.indexOf('.') >= 0) {
    		lType = Type.ftNumRightJustified;
    	} else {
    		lType = CommonCode.commonTypeChecks(identifier, usage, picture, signed, signSeperate, signPosition);
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
	public NumericDefinition getNumericDefinition() {
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
