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

package net.sf.JRecord.IO.builders;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.External.ICopybookLoaderStream;
import net.sf.JRecord.def.IO.builders.ICobolMultiCopybookIOBuilder;
import net.sf.JRecord.def.IO.builders.IFileIOBuilder;
import net.sf.JRecord.def.IO.builders.Icb2xmlMultiFileIOBuilder;

public class CblIOBuilderMultiSchema extends CblIOBuilderMultiSchemaBase<CblIOBuilderMultiSchema>
implements ICobolMultiCopybookIOBuilder, Icb2xmlMultiFileIOBuilder, IFileIOBuilder {

	/**
	 * Create a IOBuilder for a file based Copybook (schema)
	 * @param copybookFilename copybook file name
	 * @param loader copybook (schema) loader
	 * @param cobolDialect
	 */
	public CblIOBuilderMultiSchema(String copybookFilename, ICopybookLoaderStream loader, int cobolDialect) {
		super(Conversion.getCopyBookId(copybookFilename), loader, cobolDialect);

		copybooks.add(new CreateExternalFromFile(this, copybookFilename));
	}


	/**
	 * Create a IOBuilder for a Stream based Copybook (schema)
	 * 
	 * @param copybookStream copybook stream
	 * @param copybookName name of the copybook
	 * @param loader copybook (schema) loader
	 * @param cobolDialect Cobol Dialect
	 * @throws IOException Any IO Error that occured
	 */
	public CblIOBuilderMultiSchema(InputStream copybookStream, String copybookName, ICopybookLoaderStream loader, int cobolDialect) {
		super(copybookName, loader, cobolDialect);

		copybooks.add(new CreateExternalFromStream(this, copybookStream, copybookName));
	} 

	/**
	 * Create Multicopybook builder
	 * @param copybookName name of the copybook
	 * @param loader copybook (schema) loader
	 */
	public CblIOBuilderMultiSchema(String copybookname, ICopybookLoaderStream loader) {
		super(copybookname, loader);
	}
}
