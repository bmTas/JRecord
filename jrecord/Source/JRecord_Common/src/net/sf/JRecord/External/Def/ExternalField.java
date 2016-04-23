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
      
package net.sf.JRecord.External.Def;



/**
 * External (ie read from a file/ DB) Field definition
 *
 *
 * @author Bruce Martin
 *
 */
public class ExternalField extends AbstractUpdatableRecord {

  private int position;
  private int length;
  private String name;
  private String description;
  private int type;
  private int decimal;
  private int cellFormat;
  private String parameter;
  private String defaultValue;
  private String cobolName;
  private int subKey;
  private final DependingOnDtls dependOnDtls;

  private String group = "";



  /**
   * create an empty External field
   *
   */
  public ExternalField () {
      super(true);

      position = 0;
      length = 0;
      name = "";
      description = "";
      type = 0;
      decimal = 0;
      cellFormat = 0;
      parameter = "";
      defaultValue = "";
      cobolName = "";
      subKey = 0;
      dependOnDtls = null;
  }




  /**
   * External (ie read from a file/ DB) Field definition
   * @param pPos starting position of the field in the record
   * @param pLen length of the field
   * @param pName field name
   * @param pDescription field description
   * @param pType field type
   * @param pDecimal number of decimal places (for numeric fields)
   * @param pCellFormat cell format
   * @param pParameter parameter for Type / cell format
   * @param pDefault default value (not used)
   * @param pCobolName Cobol Field name
   * @param pSubKey DB sub key
   */
  public ExternalField (
                    final int pPos
                  , final int pLen
                  , final String pName
                  , final String pDescription
                  , final int pType
                  , final int pDecimal
                  , final int pCellFormat
                  , final String pParameter
                  , final String pDefault
                  , final String pCobolName
                  , final  int pSubKey
                  ) {
	  this(pPos, pLen, pName, pDescription, pType, pDecimal, pCellFormat, pParameter, pDefault, pCobolName, pSubKey, null);
  }
  
  public ExternalField (
          final int pPos
        , final int pLen
        , final String pName
        , final String pDescription
        , final int pType
        , final int pDecimal
        , final int pCellFormat
        , final String pParameter
        , final String pDefault
        , final String pCobolName
        , final  int pSubKey
        , final DependingOnDtls dependDtls
        ) {


      super(false);

      position = pPos;
      length = pLen;
      name = pName;
      description = pDescription;
      type = pType;
      decimal = pDecimal;
      cellFormat = pCellFormat;
      parameter = pParameter;
      defaultValue = pDefault;
      cobolName = pCobolName;
      subKey = pSubKey;
      dependOnDtls = dependDtls;
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
   * Clone as ExternalField
   * @return cloned version of record
   */
  public ExternalField fullClone() {

      ExternalField ret = null;
      try {
          ret =  (ExternalField) super.clone();
      } catch (Exception e) {
          ret = new ExternalField(
                  position
                  , length
                  , name
                  , description
                  , type
                  , decimal
                  , cellFormat
                  , parameter
                  , defaultValue
                  , cobolName
                  , subKey
          );
      }
      return ret;
  }




  /**
   * This method gets the vaule of Pos
   * @return poistion of the field in the record
   */
  public int getPos() {
      return position;
  }

  /**
   * This method sets the vaule of Pos
   *
   * @param val value to be assigned to Pos
   */
  public void setPos(int val) {

      if ((val != position) || (updateStatus == NULL_INT_VALUE)) {
           position = val;
           updateStatus = UPDATED;
      }
  }

  /**
   * This method gets the vaule of Len
   * @return field length
   */
  public int getLen() {
      return length;
  }

  /**
   *  This method sets the vaule of Len
   *
   * @param val value to be assigned to Len
   */
  public void setLen(int val) {

      if ((val != length) || (updateStatus == NULL_INT_VALUE)) {
           length = val;
           updateStatus = UPDATED;
      }
  }

  /**
   * This method gets the vaule of Name
   * @return field name
   */
  public String getName() {
      return name;
  }

  /**
   *  This method sets the value of Name
   *
   * @param val value to be assigned to Name
   */
  public void setName(String val) {

      if ((val == null || "".equals(val))
      && (name == null || "".equals(name))) {
          return;
      }

      if ((val == null) || (! val.equals(name)) || (updateStatus == NULL_INT_VALUE)) {
           name = val;
           updateStatus = UPDATED;
      }
  }

  /**
   * This method gets the vaule of Description
   * @return Field description
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
   * This method gets the vaule of Type
   * @return field type
   */
  public int getType() {
      return type;
  }

  /**
   *  This method sets the vaule of Type
   *
   * @param val value to be assigned to Type
   */
  public void setType(int val) {

      if ((val != type) || (updateStatus == NULL_INT_VALUE)) {
           type = val;
           updateStatus = UPDATED;
      }
  }

  /**
   * This method gets the vaule of Decimal
   * @return number of decimal places in the field
   */
  public int getDecimal() {
      return decimal;
  }

  /**
   *  This method sets the vaule of Decimal
   *
   * @param val value to be assigned to Decimal
   */
  public void setDecimal(int val) {

      if ((val != decimal) || (updateStatus == NULL_INT_VALUE)) {
           decimal = val;
           updateStatus = UPDATED;
      }
  }

  /**
   * This method gets the vaule of CellFormat
   * @return cell format details
   */
  public int getCellFormat() {
      return cellFormat;
  }

  /**
   *  This method sets the vaule of CellFormat
   *
   * @param val value to be assigned to CellFormat
   */
  public void setCellFormat(int val) {

      if ((val != cellFormat) || (updateStatus == NULL_INT_VALUE)) {
           cellFormat = val;
           updateStatus = UPDATED;
      }
  }

  /**
   * This method gets the vaule of Parameter
   * @return get type / cell format parameter
   */
  public String getParameter() {
      return parameter;
  }

  /**
   *  This method sets the vaule of Parameter
   *
   * @param val value to be assigned to Parameter
   */
  public void setParameter(String val) {

      if ((val == null || "".equals(val))
      && (parameter == null || "".equals(parameter))) {
          return;
      }

      if ((val == null) || (! val.equals(parameter)) || (updateStatus == NULL_INT_VALUE)) {
           parameter = val;
           setUpdateStatus(UPDATED);
      }
  }

  /**
   * This method gets the vaule of Default
   * @return get default value
   */
  public String getDefault() {
      return defaultValue;
  }

  /**
   *  This method sets the vaule of Default
   *
   * @param val value to be assigned to Default
   */
  public void setDefault(String val) {

      if ((val == null || "".equals(val))
      && (defaultValue == null || "".equals(defaultValue))) {
          return;
      }

      if ((val == null) || (! val.equals(defaultValue)) || (updateStatus == NULL_INT_VALUE)) {
           defaultValue = val;
           updateStatus = UPDATED;
      }
  }

  /**
   * This method gets the vaule of CobolName
   * @return Cobol name
   */
  public String getCobolName() {
      return cobolName;
  }

  /**
   *  This method sets the vaule of CobolName
   *
   * @param val value to be assigned to CobolName
   */
  public void setCobolName(String val) {

      if ((val == null || "".equals(val))
      && (cobolName == null || "".equals(cobolName))) {
          return;
      }

      if ((val == null) || (! val.equals(cobolName)) || (updateStatus == NULL_INT_VALUE)) {
           cobolName = val;
           updateStatus = UPDATED;
      }
  }

  /**
   * This method gets the vaule of SubKey
   * @return DB Sub key
   */
  public int getSubKey() {
      return subKey;
  }

  /**
   *  This method sets the vaule of SubKey
   *
   * @param val value to be assigned to SubKey
   */
  public void setSubKey(int val) {

      if ((val != subKey) || (updateStatus == NULL_INT_VALUE)) {
           subKey = val;
           updateStatus = UPDATED;
      }
  }




/**
 * @return the group
 */
public final String getGroup() {
	return group;
}




/**
 * @return the dependOnDtls
 */
public final DependingOnDtls getDependOnDtls() {
	return dependOnDtls;
}




/**
 * @param group the group to set
 */
public final void setGroup(String group) {
	this.group = group;
}
}
