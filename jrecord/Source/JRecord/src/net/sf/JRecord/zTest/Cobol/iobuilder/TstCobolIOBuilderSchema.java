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

package net.sf.JRecord.zTest.Cobol.iobuilder;

import java.io.FileInputStream;
import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.cb2xml.def.Cb2xmlConstants;
import junit.framework.TestCase;

public class TstCobolIOBuilderSchema extends TestCase {


	IFieldDetail[][] test01 = {
		{
			bldType("BldrTst-Record-Type", 1, 1, 0, 0, ""),
			bldType("BldrTst-comp-l2", 2, 2, 0, 35, ""),
			bldType("BldrTst-comp-l4", 4, 2, 0, 35, ""),
			bldType("BldrTst-comp-l6", 6, 4, 0, 35, ""),
			bldType("BldrTst-comp-l8", 10, 4, 0, 35, ""),
			bldType("BldrTst-comp-l9", 14, 4, 0, 35, ""),
			bldType("BldrTst-Record-Type", 18, 1, 0, 0, ""),
			bldType("BldrTst-comp-5-l2", 19, 2, 0, 35, ""),
			bldType("BldrTst-comp-5-l4", 21, 2, 0, 35, ""),
			bldType("BldrTst-comp-5-l6", 23, 4, 0, 35, ""),
			bldType("BldrTst-comp-5-l8", 27, 4, 0, 35, ""),
			bldType("BldrTst-comp-5-l9", 31, 4, 0, 35, ""),
			bldType("BldrTst-Record-Type", 35, 1, 0, 0, ""),
			bldType("BldrTst-char-field", 36, 4, 0, 0, ""),
			bldType("BldrTst-Zoned", 40, 4, 0, 32, ""),
		},
	};
	
	IFieldDetail[][] test02 = {
			{
				bldType("BldrTst-Record-Type", 1, 1, 0, 0, ""),
				bldType("BldrTst-comp-l2", 2, 1, 0, 35, ""),
				bldType("BldrTst-comp-l4", 3, 2, 0, 35, ""),
				bldType("BldrTst-comp-l6", 5, 3, 0, 35, ""),
				bldType("BldrTst-comp-l8", 8, 4, 0, 35, ""),
				bldType("BldrTst-comp-l9", 12, 4, 0, 35, ""),
				bldType("BldrTst-Record-Type", 16, 1, 0, 0, ""),
				bldType("BldrTst-comp-5-l2", 17, 1, 0, 15, ""),
				bldType("BldrTst-comp-5-l4", 18, 2, 0, 15, ""),
				bldType("BldrTst-comp-5-l6", 20, 3, 0, 15, ""),
				bldType("BldrTst-comp-5-l8", 23, 4, 0, 15, ""),
				bldType("BldrTst-comp-5-l9", 27, 4, 0, 15, ""),
				bldType("BldrTst-Record-Type", 31, 1, 0, 0, ""),
				bldType("BldrTst-char-field", 32, 4, 0, 0, ""),
				bldType("BldrTst-Zoned", 36, 4, 0, 46, ""),
			},
		};

	IFieldDetail[][] test03 = {
			{
				bldType("BldrTst-Record-Type", 1, 1, 0, 0, ""),
				bldType("BldrTst-comp-l2", 2, 1, 0, 35, ""),
				bldType("BldrTst-comp-l4", 3, 2, 0, 35, ""),
				bldType("BldrTst-comp-l6", 5, 3, 0, 35, ""),
				bldType("BldrTst-comp-l8", 8, 4, 0, 35, ""),
				bldType("BldrTst-comp-l9", 12, 4, 0, 35, ""),
			},
			{
				bldType("BldrTst-Record-Type", 1, 1, 0, 0, ""),
				bldType("BldrTst-comp-5-l2", 2, 1, 0, 15, ""),
				bldType("BldrTst-comp-5-l4", 3, 2, 0, 15, ""),
				bldType("BldrTst-comp-5-l6", 5, 3, 0, 15, ""),
				bldType("BldrTst-comp-5-l8", 8, 4, 0, 15, ""),
				bldType("BldrTst-comp-5-l9", 12, 4, 0, 15, ""),
			},
			{
				bldType("BldrTst-Record-Type", 1, 1, 0, 0, ""),
				bldType("BldrTst-char-field", 2, 4, 0, 0, ""),
				bldType("BldrTst-Zoned", 6, 4, 0, 46, ""),
			},
		};


	/**
	 * Testing The following:
	 * 
	 *  * Copybook read correctly
	 *  * dialect = ICopybookDialects.FMT_MAINFRAME works
	 *  * parameter FileOrganisation (FileStructure) works 
	 *  * parameter works
	 *  * parameter DropCopybookname works
	 *  * parameter CopybookFileFormat works
	 *  * Can read copybook from stream
	 *  
	 *  
	 */
	public void test01() throws RecordException, IOException {
		TstData data = new TstData("test01", Cb2xmlConstants.USE_STANDARD_COLUMNS, ICopybookDialects.FMT_MAINFRAME, 
				Constants.IO_FIXED_LENGTH_RECORDS, CopybookLoader.SPLIT_NONE, "", false,
				test01);
		
		tstFileBldr(data);
		tstFileBldrStream(data);
		tstFileBldrLongLine(data);   // Test parameter CopybookFileFormat
		

		TstData data1 = new TstData("test01a", Cb2xmlConstants.USE_STANDARD_COLUMNS, ICopybookDialects.FMT_MAINFRAME, 
					Constants.IO_FIXED_LENGTH_RECORDS, CopybookLoader.SPLIT_NONE, "cp037", true,
					dropFromFieldName(test01, "BldrTst-".length()));
			
		tstFileBldr(data1);

		
		TstData data2 = new TstData("test01b", Cb2xmlConstants.USE_STANDARD_COLUMNS, ICopybookDialects.FMT_MAINFRAME, 
				Constants.IO_VB, CopybookLoader.SPLIT_NONE, "", false,
				test01);
		
		tstFileBldr(data2);
		TstData data3 = new TstData("test01b", Cb2xmlConstants.USE_STANDARD_COLUMNS, ICopybookDialects.FMT_MAINFRAME, 
				Constants.IO_VB_GNU_COBOL, CopybookLoader.SPLIT_NONE, "", false,
				test01);
		
		tstFileBldr(data3);
	}
	
	
	/**
	 * Testing The following:
	 * 
	 *  * Copybook read correctly
	 *  * dialect = ICopybookDialects.FMT_OC_MICRO_FOCUS works
	 *  * Can read copybook from stream
	 *  * Long-line option works
	 */
	public void test02() throws RecordException, IOException {
		TstData data = new TstData("test02", Cb2xmlConstants.USE_STANDARD_COLUMNS, ICopybookDialects.FMT_OC_MICRO_FOCUS, 
				Constants.IO_FIXED_LENGTH_RECORDS, CopybookLoader.SPLIT_NONE, "", false,
				test02);
		
		tstFileBldr(data);
		tstFileBldrStream(data);
		tstFileBldrLongLine(data);
		
		TstData data1 = new TstData("test02a", Cb2xmlConstants.USE_STANDARD_COLUMNS, ICopybookDialects.FMT_OC_MICRO_FOCUS, 
				Constants.IO_FIXED_LENGTH_RECORDS, CopybookLoader.SPLIT_NONE, "cp037", true,
				dropFromFieldName(test02, "BldrTst-".length()));
		
		tstFileBldr(data1);
		tstFileBldrStream(data1);
	}

	/**
	 * Checking split option works
	 * Checking Reading Cobol Copybook from stream works
	 * Checking Long-line option works
	 */
	public void test03() throws RecordException, IOException {
		TstData data = new TstData("test03", Cb2xmlConstants.USE_STANDARD_COLUMNS, ICopybookDialects.FMT_OC_MICRO_FOCUS, 
				Constants.IO_FIXED_LENGTH_RECORDS, CopybookLoader.SPLIT_HIGHEST_REPEATING, "", false,
				test03);
		
		tstFileBldr(data);
		tstFileBldrStream(data);
		tstFileBldrLongLine(data);
	}
	
	private void tstFileBldrLongLine(TstData data) throws RecordException, IOException {
		String copybookFileName = this.getClass().getResource("BldrTstLongLine.cbl").getFile();
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance().newIOBuilder(copybookFileName, data.dialect);
		tstFileBldrLongLine(data, ioBuilder);
		
		ioBuilder = JRecordInterface1.COBOL.newIOBuilder(copybookFileName).setDialect(data.dialect);
		tstFileBldrLongLine(data, ioBuilder);
	}


	/**
	 * @param data
	 * @param ioBuilder
	 * @throws RecordException
	 * @throws IOException
	 */
	private void tstFileBldrLongLine(TstData data, ICobolIOBuilder ioBuilder)
			throws RecordException, IOException {
		int hold = data.copybookFileFormat;
		
		data.copybookFileFormat = Cb2xmlConstants.USE_LONG_LINE;
		tstBldr(ioBuilder, data);
		
		data.copybookFileFormat = hold;
	}
	private void tstFileBldr(TstData data) throws RecordException, IOException {
		String copybookFileName = this.getClass().getResource("BldrTst.cbl").getFile();
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance().newIOBuilder(copybookFileName, data.dialect);
		tstBldr(ioBuilder, data);
		
		ioBuilder = JRecordInterface1.COBOL.newIOBuilder(copybookFileName).setDialect(data.dialect);
		tstBldr(ioBuilder, data);
	}

	private void tstFileBldrStream(TstData data) throws RecordException, IOException {
		String copybookFileName = this.getClass().getResource("BldrTst.cbl").getFile(); 
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance().newIOBuilder(new FileInputStream(copybookFileName), "BldrTst", data.dialect);
		tstBldr(ioBuilder, data);
		
		ioBuilder = JRecordInterface1.COBOL.newIOBuilder(new FileInputStream(copybookFileName), "BldrTst").setDialect(data.dialect);
		tstBldr(ioBuilder, data);
	}

	private void tstBldr(ICobolIOBuilder ioBuilder, TstData data) throws RecordException, IOException {
		ioBuilder
			.setCopybookFileFormat(data.copybookFileFormat)
			.setDropCopybookNameFromFields(data.dropCopybookNameFromFields)
			.setFileOrganization(data.fileOrganization)
			.setFont(data.font)
			.setSplitCopybook(data.splitCopybook);
		
		
		LayoutDetail schema = ioBuilder.getLayout();
		RecordDetail rec;
		IFieldDetail fld, ef;
		String t1, t2;
		
		assertEquals(data.testId, data.fileOrganization, schema.getFileStructure());
		assertEquals(data.testId, data.font, schema.getFontName());
		assertEquals(data.testId, data.expectedFields.length, schema.getRecordCount());
		
		for (int i = 0; i < schema.getRecordCount(); i++) {
			t1 = data.testId + ": " + i;
			rec = schema.getRecord(i);
			
			assertEquals(t1, data.expectedFields[i].length, rec.getFieldCount());

			for (int j = 0; j < rec.getFieldCount(); j++) {
				t2 = t1 + ", " + j;
				fld = rec.getField(j);
				ef =  data.expectedFields[i][j];
				assertEquals(t2, ef.getName(), fld.getName());
				assertEquals(t2, ef.getPos(), fld.getPos());
				assertEquals(t2, ef.getLen(), fld.getLen());
				assertEquals(t2, ef.getDecimal(), fld.getDecimal());
				assertEquals(t2, ef.getType(), fld.getType());
				assertEquals(t2, ef.getFontName(), fld.getFontName());
			}
		}
	}

	
	
	private IFieldDetail[][] dropFromFieldName(IFieldDetail[][] flds, int len) {
		IFieldDetail[][] ret = new IFieldDetail[flds.length][];
		IFieldDetail f;
		
		for (int i = 0; i < flds.length; i++) {
			ret[i] = new IFieldDetail[flds[i].length];
			for (int j = 0; j < flds[i].length; j++) {
				f = flds[i][j];
				ret[i][j] = new FieldDetail(f.getName().substring(len), "", f.getType(), f.getDecimal(), "cp037", 0, "");
				ret[i][j].setPosLen(f.getPos(), f.getLen());
			}
		}
		
		return ret;
	}
	
	private FieldDetail bldType(String name, int pos, int len, int decimal, int type, String font) {
		FieldDetail fd = new FieldDetail(name, "", type, decimal, font, 0, "");
		fd.setPosLen(pos, len);
		
		return fd;
	}
	
	private static class TstData {
		String testId;
		int copybookFileFormat, dialect, fileOrganization, splitCopybook;
		
		String font;
		boolean dropCopybookNameFromFields;
		
		IFieldDetail[][] expectedFields;
		
		protected TstData(
				String testId,
				int copybookFileFormat, int dialect,
				int fileOrganization, int splitCopybook, String font,
				boolean dropCopybookNameFromFields,
				IFieldDetail[][] expectedFields) {
			super();
			
			this.testId = testId;
			this.copybookFileFormat = copybookFileFormat;
			this.dialect = dialect;
			this.fileOrganization = fileOrganization;
			this.splitCopybook = splitCopybook;
			this.font = font;
			this.dropCopybookNameFromFields = dropCopybookNameFromFields;
			this.expectedFields = expectedFields;
		}
		
		
	}
}
