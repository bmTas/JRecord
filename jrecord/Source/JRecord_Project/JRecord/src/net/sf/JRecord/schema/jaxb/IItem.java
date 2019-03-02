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

package net.sf.JRecord.schema.jaxb;

import java.util.List;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.cgen.def.IArrayAnyDimension;
import net.sf.JRecord.schema.IArrayItemCheck;

public interface IItem {

	public static final int TYPE_GROUP = 1;
	public static final int TYPE_FIELD = 2;
	public static final int TYPE_ARRAY = 3;

	public abstract List<? extends IItem> getChildItems();

	/**
	 * Gets the value of the assumedDigits property.
	 * 
	 * @return
	 *     possible object is
	 *     {@link Integer }
	 *     
	 */
	public abstract Integer getAssumedDigits();

	/**
	 * Gets the value of the dependingOn property.
	 * 
	 * @return
	 *     possible object is
	 *     {@link String }
	 *     
	 */
	public abstract String getDependingOn();

	/**
	 * Gets the value of the displayLength property.
	 * 
	 */
	public abstract int getDisplayLength();

	/**
	 * Gets the value of the level property.
	 * 
	 * @return
	 *     possible object is
	 *     {@link String }
	 *     
	 */
	public abstract String getLevel();

	/**
	 * Gets the value of the name property.
	 * 
	 * @return
	 *     possible object is
	 *     {@link String }
	 *     
	 */
	public abstract String getName();

	/**
	 * Gets the value of the numeric property.
	 * 
	 * @return
	 *     possible object is
	 *     {@link Boolean }
	 *     
	 */
	public abstract Boolean isNumeric();

	/**
	 * Gets the value of the occurs property.
	 * 
	 * @return
	 *     possible object is
	 *     {@link Integer }
	 *     
	 */
	public abstract Integer getOccurs();

	/**
	 * Gets the value of the occursMin property.
	 * 
	 * @return
	 *     possible object is
	 *     {@link Integer }
	 *     
	 */
	public abstract Integer getOccursMin();

	/**
	 * Gets the value of the picture property.
	 * 
	 * @return
	 *     possible object is
	 *     {@link String }
	 *     
	 */
	public abstract String getPicture();

	/**
	 * Gets the value of the position property.
	 * 
	 */
	public abstract int getPosition();

	/**
	 * Get position for array field
	 * @param indexs
	 * @return
	 */
	public abstract int getPosition(int[] indexs);

	/**
	 * Gets the value of the redefined property.
	 * 
	 * @return
	 *     possible object is
	 *     {@link String }
	 *     
	 */
	public abstract String getRedefined();

	/**
	 * Gets the value of the redefines property.
	 * 
	 * @return
	 *     possible object is
	 *     {@link String }
	 *     
	 */
	public abstract String getRedefines();

	/**
	 * Gets the value of the scale property.
	 * 
	 * @return
	 *     possible object is
	 *     {@link Integer }
	 *     
	 */
	public abstract Integer getScale();

	/**
	 * Gets the value of the signPosition property.
	 * 
	 * @return
	 *     possible object is
	 *     {@link String }
	 *     
	 */
	public abstract String getSignPosition();

	/**
	 * Gets the value of the signSeparate property.
	 * 
	 * @return
	 *     possible object is
	 *     {@link Boolean }
	 *     
	 */
	public abstract Boolean isSignSeparate();

	/**
	 * Gets the value of the signed property.
	 * 
	 * @return
	 *     possible object is
	 *     {@link Boolean }
	 *     
	 */
	public abstract Boolean isSigned();

	/**
	 * Gets the value of the storageLength property.
	 * 
	 */
	public abstract int getStorageLength();

	/**
	 * Gets the value of the sync property.
	 * 
	 * @return
	 *     possible object is
	 *     {@link Boolean }
	 *     
	 */
	public abstract Boolean isSync();

	/**
	 * Gets the value of the usage property.
	 * 
	 * @return
	 *     possible object is
	 *     {@link String }
	 *     
	 */
	public abstract String getUsage();

	/**
	 * Gets the value of the value property.
	 * 
	 * @return
	 *     possible object is
	 *     {@link String }
	 *     
	 */
	public abstract String getValue();

	/**
	 * @return the itemType
	 */
	public abstract int getItemType();

	/**
	 * @return the fieldDefinition
	 */
	public abstract IFieldDetail getFieldDefinition();

	/**
	 * @return the arrayDefinition
	 */
	public abstract IArrayAnyDimension getArrayDefinition();

	/**
	 * @return the nameToUse
	 */
	public abstract String getNameToUse();

	/**
	 * @return the fieldName
	 */
	public abstract String getFieldName();

	/**
	 * @return the fieldRedefined
	 */
	public abstract boolean isFieldRedefined();

	public abstract IArrayItemCheck getArrayValidation();

	/**
	 * temporary measure for <b>internal</b> use only. It <b>will be removed</b>.
	 * @return
	 * @deprecated temporary measure for <b>internal</b> use only. It <b>will be removed</b>.
	 */
	@Deprecated
	public int getSaveIndex();

	/**
	 * temporary measure for <b>internal</b> use only.  It <b>will be removed</b>.
	 * @return
	 * @deprecated temporary measure for <b>internal</b> use only. It <b>will be removed</b>.
	 */
	@Deprecated
	public int getODArraySizeIdx();

}