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

import java.io.ByteArrayInputStream;
import java.io.IOException;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.IO.builders.CblIOBuilderBase;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.IIOBuilder;
import net.sf.cb2xml.def.Cb2xmlConstants;


/**
 * Very basic check to ensure Attributes are set correctly
 * 
 * @author Bruce Martin
 *
 */
public class TstCobolIOBuilder01 extends TestCase {
	private static final String[] ATTR_NAMES = {
		"dialect",
		"splitCopybook",
		"copybookFileFormat",
		"fileOrganization",
		"font",
		"dropCopybookNameFromFields",
		"defaultFileOrganization",
		"defaultFont"
	};
	
	private int[] dialects = {
			ICopybookDialects.FMT_MAINFRAME, ICopybookDialects.FMT_FS2000, ICopybookDialects.FMT_FS2000_BE,
			ICopybookDialects.FMT_FUJITSU, ICopybookDialects.FMT_OC_MICRO_FOCUS,
			ICopybookDialects.FMT_GNU_COBOL, ICopybookDialects.FMT_GNU_COBOL_BE,
			ICopybookDialects.FMT_GNU_COBOL_MVS,
	};
//	int splitCopybook = CopybookLoader.SPLIT_NONE;
//	String font = "";
//	int copybookFileFormat = 1; // actually Cb2xmlConstants.USE_STANDARD_COLUMNS;
//	int fileOrganization = Constants.NULL_INTEGER;
//	boolean dropCopybookNameFromFields = false;
	
	@SuppressWarnings({ "deprecation", "rawtypes" })
	public void testDefaultValues1() {
		Object[] attrs = {
				ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
				Constants.NULL_INTEGER, Conversion.DEFAULT_ASCII_CHARSET, Boolean.FALSE,
				null		
		};
		checkAttributes(new CblIoBldr(ICopybookDialects.FMT_MAINFRAME).getAttrs(), attrs);
		checkAttributes(
				((CblIOBuilderBase)JRecordInterface1.COBOL
						.newIOBuilder("")
							.setDialect(ICopybookDialects.FMT_MAINFRAME)
				).getAllAttributes(), 
				attrs);
		checkAttributes(
				((CblIOBuilderBase)JRecordInterface1.COBOL
						.newIOBuilder("")
				).getAllAttributes(), 
				attrs);
		checkAttributes(
				((CblIOBuilderBase)JRecordInterface1.COBOL
						.newMultiCopybookIOBuilder("")
							.setDialect(ICopybookDialects.FMT_MAINFRAME)
				).getAllAttributes(), 
				attrs);
		checkAttributes(
				((CblIOBuilderBase)JRecordInterface1.COBOL
						.newMultiCopybookIOBuilder("")
							.addCopyBook("")
							.setDialect(ICopybookDialects.FMT_MAINFRAME)
				).getAllAttributes(), 
				attrs);
		checkAttributes(
				((CblIOBuilderBase)JRecordInterface1.COBOL
						.newMultiCopybookIOBuilder("")
				).getAllAttributes(), 
				attrs);
	}

	@SuppressWarnings({ "deprecation", "rawtypes" })
	public void testDefaultValues2() {
		int[] fileStructures = {
				Constants.IO_STANDARD_UNICODE_TEXT_FILE, Constants.IO_FIXED_LENGTH_CHAR,
				Constants.IO_UNICODE_CSV, Constants.IO_UNICODE_CSV_NAME_1ST_LINE,
				Constants.IO_UNICODE_NAME_1ST_LINE, Constants.IO_UNICODE_TEXT
		};
		
		for (int fs : fileStructures) {
			Object[] attrs = {
					ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
					fs, "", Boolean.FALSE, null			
			};
			CblIoBldr cblIoBldr = new CblIoBldr(ICopybookDialects.FMT_MAINFRAME);
			cblIoBldr.setFileOrganization(fs);
			checkAttributes(cblIoBldr.getAttrs(), attrs);
			checkAttributes(
					((CblIOBuilderBase)JRecordInterface1.COBOL
							.newIOBuilder("")
								.setFileOrganization(fs)
								.setDialect(ICopybookDialects.FMT_MAINFRAME)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)JRecordInterface1.COBOL
							.newIOBuilder("")
								.setFileOrganization(fs)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)JRecordInterface1.COBOL
							.newMultiCopybookIOBuilder("")
								.setFileOrganization(fs)
								.setDialect(ICopybookDialects.FMT_MAINFRAME)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)JRecordInterface1.COBOL
							.newMultiCopybookIOBuilder("")
								.addCopyBook("")
								.setFileOrganization(fs)
								.setDialect(ICopybookDialects.FMT_MAINFRAME)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)JRecordInterface1.COBOL
							.newMultiCopybookIOBuilder("")
								.setFileOrganization(fs)
					).getAllAttributes(), 
					attrs);
			for (int d : dialects) {
				Object[] attrs1 = {
						d, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
						fs, "", Boolean.FALSE, null			
				};
				cblIoBldr = new CblIoBldr(d);
				cblIoBldr.setFileOrganization(fs);
				checkAttributes(cblIoBldr.getAttrs(), attrs1);
				checkAttributes(
						((CblIOBuilderBase)JRecordInterface1.COBOL
								.newIOBuilder("")
									.setFileOrganization(fs)
									.setDialect(d)
						).getAllAttributes(), 
						attrs1);

				checkAttributes(
						((CblIOBuilderBase)JRecordInterface1.COBOL
								.newMultiCopybookIOBuilder("")
									.setFileOrganization(fs)
									.setDialect(d)
						).getAllAttributes(), 
						attrs1);
				checkAttributes(
						((CblIOBuilderBase)JRecordInterface1.COBOL
								.newMultiCopybookIOBuilder("")
									.addCopyBook("")
									.setFileOrganization(fs)
									.setDialect(d)
						).getAllAttributes(), 
						attrs1);

			}
		}
	}
	

	@SuppressWarnings({ "deprecation", "rawtypes" })
	public void testDefaultValues3() {
		int[] fileStructures = {
				Constants.IO_BIN_TEXT, Constants.IO_BIN_CSV,
				Constants.IO_CONTINOUS_NO_LINE_MARKER, Constants.IO_CSV,
				Constants.IO_NAME_1ST_LINE, 
				Constants.IO_FIXED_LENGTH, Constants.IO_FIXED_LENGTH_RECORDS,
				Constants.IO_VB, Constants.IO_VB_DUMP, Constants.IO_VB_FUJITSU,
				Constants.IO_VB_GNU_COBOL
		};
		
		for (int fs : fileStructures) {
			Object[] attrs = {
					ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
					fs, Conversion.DEFAULT_ASCII_CHARSET, Boolean.FALSE,  null			
			};
			CblIoBldr cblIoBldr = new CblIoBldr(ICopybookDialects.FMT_MAINFRAME);
			cblIoBldr.setFileOrganization(fs);
			checkAttributes(cblIoBldr.getAttrs(), attrs);
			checkAttributes(
					((CblIOBuilderBase)JRecordInterface1.COBOL
							.newIOBuilder("")
								.setFileOrganization(fs)
								.setDialect(ICopybookDialects.FMT_MAINFRAME)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)JRecordInterface1.COBOL
							.newIOBuilder("")
								.setFileOrganization(fs)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)JRecordInterface1.COBOL
							.newMultiCopybookIOBuilder("")
								.setFileOrganization(fs)
								.setDialect(ICopybookDialects.FMT_MAINFRAME)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)JRecordInterface1.COBOL
							.newMultiCopybookIOBuilder("")
								.setFileOrganization(fs)
					).getAllAttributes(), 
					attrs);
			for (int d : dialects) {
				Object[] attrs1 = {
						d, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
						fs, Conversion.DEFAULT_ASCII_CHARSET, Boolean.FALSE, null			
				};
				cblIoBldr = new CblIoBldr(d);
				cblIoBldr.setFileOrganization(fs);
				checkAttributes(cblIoBldr.getAttrs(), attrs1);
				checkAttributes(
						((CblIOBuilderBase)JRecordInterface1.COBOL
								.newIOBuilder("")
									.setFileOrganization(fs)
									.setDialect(d)
						).getAllAttributes(), 
						attrs1);

				checkAttributes(
						((CblIOBuilderBase)JRecordInterface1.COBOL
								.newMultiCopybookIOBuilder("")
									.setFileOrganization(fs)
									.setDialect(d)
						).getAllAttributes(), 
						attrs1);
			}
		}
	}

	@SuppressWarnings({ "deprecation", "rawtypes" })
	public void testDefaultValues21() {
		Object[] attrs = {
				ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
				Constants.NULL_INTEGER, Conversion.DEFAULT_ASCII_CHARSET, Boolean.FALSE,
				null
		};
		ByteArrayInputStream is = new ByteArrayInputStream(new byte[]{});
		checkAttributes(
				((CblIOBuilderBase)JRecordInterface1.COBOL
						.newIOBuilder(is, "")
							.setDialect(ICopybookDialects.FMT_MAINFRAME)
				).getAllAttributes(), 
				attrs);
		checkAttributes(
				((CblIOBuilderBase)JRecordInterface1.COBOL
						.newMultiCopybookIOBuilder("")
								.addCopyBook(is, "")
							.setDialect(ICopybookDialects.FMT_MAINFRAME)
				).getAllAttributes(), 
				attrs);
		checkAttributes(
				((CblIOBuilderBase)JRecordInterface1.COBOL
						.newMultiCopybookIOBuilder("")
								.addCopyBook(is, "")
				).getAllAttributes(), 
				attrs);
		checkAttributes(
				((CblIOBuilderBase)JRecordInterface1.COBOL
						.newIOBuilder(is, "")
				).getAllAttributes(), 
				attrs);
	}

	@SuppressWarnings({ "deprecation", "rawtypes" })
	public void testDefaultValues22() {
		int[] fileStructures = {
				Constants.IO_STANDARD_UNICODE_TEXT_FILE, Constants.IO_FIXED_LENGTH_CHAR,
				Constants.IO_UNICODE_CSV, Constants.IO_UNICODE_CSV_NAME_1ST_LINE,
				Constants.IO_UNICODE_NAME_1ST_LINE, Constants.IO_UNICODE_TEXT
		};
		ByteArrayInputStream is = new ByteArrayInputStream(new byte[]{});
		
		for (int fs : fileStructures) {
			Object[] attrs = {
					ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
					fs, "", Boolean.FALSE, null		
			};
			CblIoBldr cblIoBldr = new CblIoBldr(ICopybookDialects.FMT_MAINFRAME);
			cblIoBldr.setFileOrganization(fs);
			checkAttributes(cblIoBldr.getAttrs(), attrs);
			checkAttributes(
					((CblIOBuilderBase)JRecordInterface1.COBOL
							.newIOBuilder(is, "")
								.setFileOrganization(fs)
								.setDialect(ICopybookDialects.FMT_MAINFRAME)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)JRecordInterface1.COBOL
							.newMultiCopybookIOBuilder("")
									.addCopyBook(is, "")
								.setFileOrganization(fs)
								.setDialect(ICopybookDialects.FMT_MAINFRAME)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)JRecordInterface1.COBOL
							.newMultiCopybookIOBuilder("")
									.addCopyBook(is, "")
								.setFileOrganization(fs)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)JRecordInterface1.COBOL
							.newIOBuilder(is, "")
								.setFileOrganization(fs)
					).getAllAttributes(), 
					attrs);
			for (int d : dialects) {
				Object[] attrs1 = {
						d, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
						fs, "", Boolean.FALSE, null			
				};
				cblIoBldr = new CblIoBldr(d);
				cblIoBldr.setFileOrganization(fs);
				checkAttributes(cblIoBldr.getAttrs(), attrs1);
				checkAttributes(
						((CblIOBuilderBase)JRecordInterface1.COBOL
								.newIOBuilder(is, "")
									.setFileOrganization(fs)
									.setDialect(d)
						).getAllAttributes(), 
						attrs1);
				checkAttributes(
						((CblIOBuilderBase)JRecordInterface1.COBOL
								.newMultiCopybookIOBuilder("")
										.addCopyBook(is, "")
									.setFileOrganization(fs)
									.setDialect(d)
						).getAllAttributes(), 
						attrs1);

			}
		}
	}
	

	@SuppressWarnings({ "deprecation", "rawtypes" })
	public void testDefaultValues23() {
		int[] fileStructures = {
				Constants.IO_BIN_TEXT, Constants.IO_BIN_CSV,
				Constants.IO_CONTINOUS_NO_LINE_MARKER, Constants.IO_CSV,
				Constants.IO_NAME_1ST_LINE, 
				Constants.IO_FIXED_LENGTH, Constants.IO_FIXED_LENGTH_RECORDS,
				Constants.IO_VB, Constants.IO_VB_DUMP, Constants.IO_VB_FUJITSU,
				Constants.IO_VB_GNU_COBOL,
		};
		ByteArrayInputStream is = new ByteArrayInputStream(new byte[]{});
		
		for (int fs : fileStructures) {
			Object[] attrs = {
					ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
					fs, Conversion.DEFAULT_ASCII_CHARSET, Boolean.FALSE,  null	
			};
			CblIoBldr cblIoBldr = new CblIoBldr(ICopybookDialects.FMT_MAINFRAME);
			cblIoBldr.setFileOrganization(fs);
			checkAttributes(cblIoBldr.getAttrs(), attrs);
			checkAttributes(
					((CblIOBuilderBase)JRecordInterface1.COBOL
							.newIOBuilder(is, "")
								.setFileOrganization(fs)
								.setDialect(ICopybookDialects.FMT_MAINFRAME)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)JRecordInterface1.COBOL
							.newMultiCopybookIOBuilder("")
									.addCopyBook(is, "")
								.setFileOrganization(fs)
								.setDialect(ICopybookDialects.FMT_MAINFRAME)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)JRecordInterface1.COBOL
							.newMultiCopybookIOBuilder("")
									.addCopyBook(is, "")
								.setFileOrganization(fs)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)JRecordInterface1.COBOL
							.newIOBuilder(is, "")
								.setFileOrganization(fs)
					).getAllAttributes(), 
					attrs);
			
			for (int d : dialects) {
				Object[] attr1 = {
						d, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
						fs, Conversion.DEFAULT_ASCII_CHARSET, Boolean.FALSE,  null			
				};
				cblIoBldr = new CblIoBldr(d);
				cblIoBldr.setFileOrganization(fs);
				checkAttributes(cblIoBldr.getAttrs(), attr1);
				checkAttributes(
						((CblIOBuilderBase)JRecordInterface1.COBOL
								.newIOBuilder(is, "")
									.setFileOrganization(fs)
									.setDialect(d)
						).getAllAttributes(), 
						attr1);
				checkAttributes(
						((CblIOBuilderBase)JRecordInterface1.COBOL
								.newMultiCopybookIOBuilder("")
										.addCopyBook(is, "")
									.setFileOrganization(fs)
									.setDialect(d)
						).getAllAttributes(), 
						attr1);
			
			}
		}
	}

	public void testAttrs01() {
		Object[] attrs = {
				ICopybookDialects.FMT_GNU_COBOL, CopybookLoader.SPLIT_01_LEVEL,  Cb2xmlConstants.USE_COLS_6_TO_80,
				Constants.IO_BIN_CSV, "cp037", Boolean.TRUE
		};
		setAndCheckAttributes(attrs);
	}
	
	
	public void testAttrs02() {
		Object[] attrs = {
				ICopybookDialects.FMT_FUJITSU, CopybookLoader.SPLIT_REDEFINE,  Cb2xmlConstants.USE_LONG_LINE,
				Constants.IO_FIXED_LENGTH, "ibm037", Boolean.FALSE
				
		};
		setAndCheckAttributes(attrs);
	}
	
	
	public void testAttrs03() {
		Object[] attrs = {
				ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
				Constants.NULL_INTEGER, "", Boolean.FALSE
		};
		setAndCheckAttributes(attrs);
	}

	@SuppressWarnings("deprecation")
	private void setAndCheckAttributes(Object[] attrs) {
		CblIoBldr bldr = new CblIoBldr((Integer)attrs[0]);
		bldr.setSplitCopybook((Integer)attrs[1]);
		bldr.setCopybookFileFormat((Integer)attrs[2]);
		bldr.setFileOrganization((Integer)attrs[3]);
		bldr.setFont(attrs[4].toString());
		bldr.setDropCopybookNameFromFields((Boolean)attrs[5]);

		checkAttributes(attrs, bldr.getAttrs());
		
		@SuppressWarnings("unchecked")
		CblIOBuilderBase<IIOBuilder> bld = (CblIOBuilderBase<IIOBuilder>) JRecordInterface1.COBOL. newIOBuilder("xyz");
		bld.setDialect((Integer)attrs[0]);
		bld.setSplitCopybook((Integer)attrs[1]);
		bld.setCopybookFileFormat((Integer)attrs[2]);
		bld.setFileOrganization((Integer)attrs[3]);
		bld.setFont(attrs[4].toString());
		bld.setDropCopybookNameFromFields((Boolean)attrs[5]);

		checkAttributes(attrs, bld.getAllAttributes());

	}

	private void checkAttributes(Object[] expected, Object[] attrs) {
		for (int i = 0; i < expected.length; i++) {
			assertEquals( 
					ATTR_NAMES[i],
					expected[i], 
					attrs[i]);
		}
	}
	
	private static class CblIoBldr extends CblIOBuilderBase<IIOBuilder> implements IIOBuilder {

		protected CblIoBldr(int dialect) {
			super(dialect);
		}


		/* (non-Javadoc)
		 * @see net.sf.JRecord.IO.builders.CblIOBuilderBase#getExternalRecordImpl()
		 */
		@Override
		protected ExternalRecord getExternalRecordImpl() throws RecordException, IOException {
			return null;
		}


		@SuppressWarnings("deprecation")
		public Object[] getAttrs() { 
			return super.getAllAttributes();
		}


//		@Override
//		public IFileIOBuilder setDefaultFont(String defaultFont) {
//			// TODO Auto-generated method stub
//			return super.setDefaultFont(defaultFont);
//		} 
//
//
//		@Override
//		public IFileIOBuilder setDefaultFileOrganisation(int fileOrganisation) {
//			super.setDefaultFileOrganization(fileOrganisation); //   setDefautlFileOrganization(fileOrganisation)
//			return this;
//		}
		
		
	}
}
