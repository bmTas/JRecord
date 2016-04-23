/*
 * @Author Bruce Martin
 * Created on 23/03/2007
 *
 * Purpose:
 * This interface descibes a class that will convert
 * a Cobol Numeric Type into a JRecord.type identifier
 */
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

/**
 * This interface describes a class that will convert
 * a Cobol Type (picture / usage) into a JRecord.type identifier
 *
 * @author Bruce Martin
 *
 */
public interface Convert extends ICopybookDialects {

    /**
     * Get the Binary Definition details
     *
     * @return Get the Binary Definition details
     */
    public abstract Object getNumericDefinition();
//  public abstract net.sf.cb2xml.def.NumericDefinition getNumericDefinition();
//  using Object instead of net.sf.cb2xml.def.NumericDefinition to avoid dependency on cb2xml when
//  it is otherwise not needed

    /**
     * This method will convert a
     * @param usage Cobol usage clause i.e. Comp Comp-3 etc
     * @param picture picture Cobol picture - s9(6)v99.
     * @param signed - wether it is a signed field
     * @return Jrecord Type Code
     */
    public abstract int getTypeIdentifier(String usage, String picture, boolean signed, boolean signSeperate, String signPosition);

    /**
     * Get the conversion Identifier
     * @return conversion Identifier
     */
    public abstract int getIdentifier();

    /**
     * Get the binary id to use
     * @return actual binary Id
     */
    public abstract int getBinaryIdentifier();

    /**
     * Get the file structure
     *
     * @param multipleRecordLengths wether there are multiple records
     * @param binary wether it is a binary file
     *
     * @return File Structure to use.
     */
    public abstract int getFileStructure(boolean multipleRecordLengths, boolean binary);

    /**
     * Get the name
     * @return the Name
     */
    public String getName();
}
