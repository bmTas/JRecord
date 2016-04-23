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

import net.sf.JRecord.Types.Type;


/**
 * This class converts an existing @see {@link Convert} into an equivalent {@link Convert} 
 * but with  comma's used instead of decimal points. 
 * @author Bruce Martin
 *
 */
public class CommaDecimal implements Convert {

	private final Convert baseConversion;
	private final int id;



	/**
	 * This class converts an existing @see {@link Convert} into an equivalent {@link Convert} 
	 * but with  comma's used instead of decimal points
	 * 
	 * @param identifier conversion identifier
	 * @param baseConversion conversion on which this conversion is based.
	 */
	public CommaDecimal(int identifier, Convert baseConversion) {
		super();
		this.id = identifier;
		this.baseConversion = baseConversion;
	}

	/**
	 * @return the "Convert" this class is based on
	 * @see net.sf.JRecord.Numeric.Convert#getNumericDefinition()
	 */
	@Override
	public Object getNumericDefinition() {
		return baseConversion.getNumericDefinition();
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.Numeric.Convert#getTypeIdentifier(java.lang.String, java.lang.String, boolean, boolean, java.lang.String)
	 */
	@Override
	public int getTypeIdentifier(String usage, String picture, boolean signed,
			boolean signSeperate, String signPosition) {

		picture = picture.toUpperCase();

		if (usage.indexOf("comp") >= 0 || picture.length() <= 2 || picture.indexOf(',') <= 0) {

		} else if (picture.indexOf('9') >= 0
        		&& (picture.startsWith("-") || picture.startsWith("+") || picture.startsWith("9"))
                && CommonCode.checkPicture(picture, '9', ',', ',')
        ) {
        	if (picture.startsWith("-")) {
        		return Type.ftNumCommaDecimal;
        	} else if (picture.startsWith("9")) {
        		return Type.ftNumCommaDecimalPositive;
        	} else if (picture.startsWith("+")) {
        		return Type.ftNumCommaDecimalPN;
        	}
		} else if (  (picture.startsWith("-") && CommonCode.checkPicture(picture, '-', ',', ','))
				  || (picture.startsWith("Z") && CommonCode.checkPicture(picture, 'Z', ',', ','))
		) {
				return Type.ftNumRightJustCommaDp;
		} else if (picture.startsWith("+")
				&& CommonCode.checkPicture(picture, '+', ',', ',')
		) {
				return Type.ftNumRightJustCommaDpPN;
		}
		return baseConversion.getTypeIdentifier(usage, picture, signed, signSeperate, signPosition);
	}

	/**
	 * @see net.sf.JRecord.Numeric.Convert#getIdentifier()
	 */
	@Override
	public int getIdentifier() {
		return id;
	}

	/**
	 * @return binary indentifier
	 * @see net.sf.JRecord.Numeric.Convert#getBinaryIdentifier()
	 */ @Override
	public int getBinaryIdentifier() {
		return baseConversion.getBinaryIdentifier();
	}

	/**
	 * @param multipleRecordLengths
	 * @param binary
	 * @return calculate the file structure (file organisation)
	 * @see net.sf.JRecord.Numeric.Convert#getFileStructure(boolean, boolean)
	 */ @Override
	public int getFileStructure(boolean multipleRecordLengths, boolean binary) {
		return baseConversion.getFileStructure(multipleRecordLengths, binary);
	}

	/**
	 * @return get the conversion name
	 * @see net.sf.JRecord.Numeric.Convert#getName()
	 */ @Override
	public String getName() {
		return baseConversion.getName() + " Decimal_Point=','";
	}

}
