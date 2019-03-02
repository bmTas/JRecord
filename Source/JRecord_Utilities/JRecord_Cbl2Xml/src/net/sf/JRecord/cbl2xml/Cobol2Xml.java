/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Cbl2Xml
 *    
 *    Sub-Project purpose: Convert Cobol Data files to / from Xml
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

package net.sf.JRecord.cbl2xml;

import java.io.InputStream;
import java.io.Reader;

import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalGroupSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.Option.JRecordConstantVars;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.cbl2xml.def.Icb2xml2Xml;
import net.sf.JRecord.cbl2xml.impl.Cobol2GroupXml;
import net.sf.JRecord.cbl2xml.impl.ConvertOptions;
import net.sf.JRecord.cbl2xml.impl.RecordParent;
import net.sf.JRecord.cbl2xml.impl.RecordSelect;
import net.sf.JRecord.schema.ArrayElementChecks;
import net.sf.JRecord.schema.jaxb.impl.AddPlusToNumeric;

/**
 * This class creates the Cobol <==> Xml and cb2xml <==> Xml builders
 * 
 * @author Bruce Martin
 *
 */
public class Cobol2Xml  {
	public static final JRecordConstantVars JR_CONSTANTS = JRecordConstantVars.INSTANCE;
	public static final ArrayElementChecks  ARRAY_CHECK_BUILDER = ArrayElementChecks.INSTANCE;
	

	public static ICobol2Xml newCobol2Xml(String cobolCopybook) {
		return Cobol2GroupXml.newCobol2Xml(cobolCopybook);
	}
	
	
	public static ICobol2Xml newCobol2Xml(InputStream cobolCopybook, String copybookName)  {
		return Cobol2GroupXml.newCobol2Xml(cobolCopybook, copybookName);
	}

	public static ICobol2Xml newCobol2Xml(Reader cobolCopybookReader, String copybookName) {
		return Cobol2GroupXml.newCobol2Xml(cobolCopybookReader, copybookName);
	}

	
	public static Icb2xml2Xml newCb2Xml2Xml(String cobolCopybook) {
		return Cobol2GroupXml.newCb2Xml2Xml(cobolCopybook);
	}
	
	
	public static Icb2xml2Xml newCb2Xml2Xml(InputStream cobolCopybook, String copybookName) {
		return Cobol2GroupXml.newCb2Xml2Xml(cobolCopybook, copybookName);
	}


	protected static Icb2xml2Xml newXmlConverter(ConvertOptions opts) {
		Icb2xml2Xml cbl2xml;
		if (opts.useCobol) {
			cbl2xml = newCobol2Xml(opts.cobolCopybook)
								.setDialect(opts.dialect);
		} else {
			cbl2xml = newCb2Xml2Xml(opts.cb2xmlCopybook);
		}
		setOpts(cbl2xml, opts);
	
		return cbl2xml;
	}



	private static void setOpts(Icb2xml2Xml cbl2xml, ConvertOptions opts) {

		cbl2xml
				.setFont(opts.font)
				.setFileOrganization(opts.fileOrganisation)
				.setDropCopybookNameFromFields(opts.dropCopybookName)
				.setXmlMainElement(opts.mainXmlTag)
				.setTagFormat(opts.tagFormat)
				.setSplitCopybook(opts.split);

		if (ConvertOptions.FORMAT_USE_PLUS_FOR_NUMERICS.equalsIgnoreCase(opts.formatText)) {
			cbl2xml.setFormatField(AddPlusToNumeric.INSTANCE);
		}
		
		for (RecordSelect rs : opts.recordSelect) {
			cbl2xml.setRecordSelection(rs.recordName, Cobol2Xml.newFieldSelection(rs.fieldName, rs.value));
		}

		for (RecordParent rp : opts.recordParents) {
			cbl2xml.setRecordParent(rp.recordName, rp.parentName);
		}
	}
	
	
	/**
	 * Create a Record-Selection based on field / value test
	 * @param fieldName name of the field to be checked
	 * @param value value to be tested against
	 * @return requested check
	 */
    public static ExternalFieldSelection newFieldSelection(String fieldName, String value) {
    	ExternalFieldSelection r = new ExternalFieldSelection(fieldName, value);
    	r.setCaseSensitive(false);
    	return r;
    }
    
    public static ExternalFieldSelection newFieldSelection(String fieldName, String op, String value) {
    	ExternalFieldSelection r = new ExternalFieldSelection(fieldName, value, op);
    	r.setCaseSensitive(false);
    	return r;
    }
   
    public static ExternalGroupSelection<ExternalSelection> newAnd(ExternalSelection... selections) {
    	return ExternalGroupSelection.newAnd(selections);
    }
    
    public static ExternalGroupSelection<ExternalSelection> newOr(ExternalSelection... selections) {
    	return ExternalGroupSelection.newOr(selections);
    }
}
