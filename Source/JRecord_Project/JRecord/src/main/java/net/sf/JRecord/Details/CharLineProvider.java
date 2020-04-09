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

package net.sf.JRecord.Details;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Conversion;

public class CharLineProvider implements LineProvider {

	@Override
	public AbstractLine getLine(LayoutDetail recordDescription) {

		if (recordDescription.isCsvLayout() && CommonBits.useCsvLine()) {
			return new CsvLine(recordDescription);
		}
		return new CharLine(recordDescription, "");
	}

	@Override
	public AbstractLine getLine(LayoutDetail recordDescription, String linesText) {

		if (recordDescription.isCsvLayout() && CommonBits.useCsvLine()) {
			return new CsvLine(recordDescription, linesText);
		}
		return new CharLine(recordDescription, linesText);
	}

	@Override
	public AbstractLine getLine(LayoutDetail recordDescription, byte[] lineBytes) {
		// TODO Auto-generated method stub
		return getLine(recordDescription, 
				Conversion.toString(lineBytes, recordDescription.getFontName()));
	}

}
