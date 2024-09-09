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

package net.sf.JRecord.External;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.base.BaseCobolItemLoader;
import net.sf.JRecord.External.cb2xml.CobolCopybookReader;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.cb2xml.analysis.Copybook;
import net.sf.cb2xml.copybookReader.ICobolCopybookTextSource;
import net.sf.cb2xml.def.Cb2xmlConstants;


/**
 * @author Bruce Martin
 */
public class CobolCopybookLoader extends BaseCobolItemLoader<ExternalRecord> 
implements ICopybookLoaderCobol  {

	public CobolCopybookLoader() {
		super(true, new ExternalRecordBuilder(), new CobolCopybookReader());
	}
	
	/**
     * Insert an XML Dom Copybook into the Copybook DB
     *
     * @param copyBookName Copy Book file Name
     * @param splitCopybook whether to split a copy book on a redefine / 01
     * @param dbIdx Database Index
     * @param font font name to use
     * @param binaryFormat binary format to use
     * @param systemId System Identifier
     * @param log log where any messages should be written
     *
     * @return return the record that has been read in
     */
    public final ExternalRecord loadCopyBook(
    								InputStream inputStream, //Document copyBookXml,
    		                             String copyBookName,
            						  		int splitCopybook,
            						  		int dbIdx,
                  						  final String font,
                						  final int binaryFormat,
                						  final int systemId,
                						  final AbsSSLogger log)
    				{
    	try {
			return loadCopyBook(inputStream, copyBookName, splitCopybook, dbIdx, font, CommonBits.getDefaultCobolTextFormat(), binaryFormat, systemId, log);
		} catch (IOException e) {
			throw new RecordException("IO Exception: " + e, e);

		}
    }

   /**
     * Whether cb2xml is available (needed for converting a Cobol Copybook
     * to an XML Dom representation
     *
     * @return whether cb2xml is available on the class path
     */
    public static boolean isAvailable() {
        // Since cb2xml is not a part of the JRecord distribution because it's
        // there is not need to check
        // the loader is available every time.
        return true;
    }

    public ExternalRecord loadCopyBook(
    		ICobolCopybookTextSource copybookReader,
            final int splitCopybookOption, final int dbIdx, final String font,
            final int copybookFormat,
            final int cobolDialect,
            final int systemId,
            final AbsSSLogger log) {
    	
    	Copybook copybook = net.sf.JRecord.External.Def.Cb2Xml.getCopybook(
    			copybookReader, cobolDialect, false, Cb2xmlConstants.FREE_FORMAT, getStackSize());
		return loadCopybook(copybook, Conversion.getCopyBookId(copybookReader.getCopybookName()), splitCopybookOption, dbIdx, font, cobolDialect, systemId);

    }
    

	@Override
	public CobolCopybookLoader doClone() {
		try {
			return (CobolCopybookLoader) clone();
		} catch (CloneNotSupportedException e) {
			throw new RuntimeException(e);
		}
	}
}
