/*
 * @Author Bruce Martin
 * Created on 20/04/2007
 *
 * Purpose:
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
      
package net.sf.JRecord.Common;

/**
 * XML Constants used in JRecord
 *
 * @author Bruce Martin
 *
 */
public interface XmlConstants {

	public static final int NAME_INDEX = 0;
    public static final int END_INDEX = 1;
    public static final int FOLLOWING_TEXT_INDEX = 2;

    public static final String XML_START_DOCUMENT = "XML Start_Document";
    public static final String XML_COMMENT        = "XML Comment";
    public static final String XML_DTD            = "XML DTD";
    public static final String XML_CDATA          = "XML CDATA";
    public static final String XML_REFERENCE      = "XML Entity Reference";

    public static final String XML_NAME           = "Xml~Name";

    public static final String STANDALONE         = "Xml~Standalone";
    public static final String ENCODING           = "Xml~Encoding";
    public static final String VERSION            = "Xml~Version";
    public static final String XML_TEXT           = "Xml~Text";
    public static final String FOLLOWING_TEXT     = "Following~Text";


    public static final String ATTRIBUTE_PREFIX   = "";
    public static final String END_ELEMENT        = "Xml~End";
    public static final String PREFIX             = "Xml~Prefix";
    public static final String NAMESPACE          = "Xml~Namespace";

}
