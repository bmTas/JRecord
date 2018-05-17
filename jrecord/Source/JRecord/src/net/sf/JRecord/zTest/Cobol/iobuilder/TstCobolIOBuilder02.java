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
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.RecordDecider;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zTest.Common.CommonCodeFields;
import net.sf.JRecord.zTest.Common.TstConstants;
import junit.framework.TestCase;

public class TstCobolIOBuilder02 extends TestCase {

 //   private static final String[] JAVA_TYPE_NAME = new String[Type.LAST_SYSTEM_TYPE];

    private static final String COPYBOOK_NAME = TstConstants.COBOL_DIRECTORY + "Numeric.cbl";
    private static final String RIGHTJUST_COPYBOOK = TstConstants.COBOL_DIRECTORY + "RightJust.cbl";

    
	private static final String[] FONTS = {
		Conversion.DEFAULT_ASCII_CHARSET,
		"cp037"
	};
	
	private static final int[] DIALECTS = {
		ICopybookDialects.FMT_MAINFRAME,
		ICopybookDialects.FMT_FUJITSU,
		ICopybookDialects.FMT_GNU_COBOL,
		ICopybookDialects.FMT_OC_MICRO_FOCUS,
	};
	
	private static final int[] FILE_STRUCTURES = {
		Constants.IO_FIXED_LENGTH,
		Constants.IO_VB,
		Constants.IO_VB_DUMP,
		Constants.IO_VB_GNU_COBOL,
	};
	
	
	private static final FieldDetail[] RIGHT_JUST_FIELDS = {	
			createField("Field-Right-Justified", 1, 20, Type.ftCharRightJust),
			createField("Field-Justified", 21, 20, Type.ftCharRightJust),
			createField("Field-1", 41, 20, Type.ftChar),
	};

	private static final FieldDetail[][] FIELDS = {{
			createField("Numeric-comp-fld-2", 1, 2, Type.ftBinaryBigEndian),
			createField("Numeric-comp-fld-4", 3, 2, Type.ftBinaryBigEndian),
			createField("Numeric-comp-fld-5", 5, 4, Type.ftBinaryBigEndian),
			createField("Numeric-comp-fld-6", 9, 4, Type.ftBinaryBigEndian),
			createField("Numeric-comp-fld-8", 13, 4, Type.ftBinaryBigEndian),
			createField("Numeric-comp-5-fld-2", 17, 2, Type.ftBinaryBigEndian),
			createField("Numeric-comp-5-fld-4", 19, 2, Type.ftBinaryBigEndian),
//			createField("pcomp-fld-2", 21, 2, Type.ftBinaryBigEndianPositive),
//			createField("pcomp-fld-4", 23, 2, Type.ftBinaryBigEndianPositive),
			createField("pcomp-fld-2", 21, 2, Type.ftPositiveBinaryBigEndian),
			createField("pcomp-fld-4", 23, 2, Type.ftPositiveBinaryBigEndian),
			createField("pic9-fld-5", 25, 5, Type.ftZonedNumeric),
		}, {
			createField("Numeric-comp-fld-2", 1, 2, Type.ftBinaryBigEndian),
			createField("Numeric-comp-fld-4", 3, 2, Type.ftBinaryBigEndian),
			createField("Numeric-comp-fld-5", 5, 4, Type.ftBinaryBigEndian),
			createField("Numeric-comp-fld-6", 9, 4, Type.ftBinaryBigEndian),
			createField("Numeric-comp-fld-8", 13, 4, Type.ftBinaryBigEndian),
			createField("Numeric-comp-5-fld-2", 17, 2, Type.ftBinaryInt),
			createField("Numeric-comp-5-fld-4", 19, 2, Type.ftBinaryInt),
			createField("pcomp-fld-2", 21, 2, Type.ftPositiveBinaryBigEndian),
			createField("pcomp-fld-4", 23, 2, Type.ftPositiveBinaryBigEndian),

			createField("pic9-fld-5", 25, 5, Type.ftFjZonedNumeric),
		}, {
			createField("Numeric-comp-fld-2", 1, 1, Type.ftBinaryBigEndian),
			createField("Numeric-comp-fld-4", 2, 2, Type.ftBinaryBigEndian),
			createField("Numeric-comp-fld-5", 4, 4, Type.ftBinaryBigEndian),
			createField("Numeric-comp-fld-6", 8, 4, Type.ftBinaryBigEndian),
			createField("Numeric-comp-fld-8", 12, 4, Type.ftBinaryBigEndian),
			createField("Numeric-comp-5-fld-2", 16, 1, Type.ftBinaryInt),
			createField("Numeric-comp-5-fld-4", 17, 2, Type.ftBinaryInt),
			createField("pcomp-fld-2", 19, 1, Type.ftPositiveBinaryBigEndian),
			createField("pcomp-fld-4", 20, 2, Type.ftPositiveBinaryBigEndian),
			createField("pic9-fld-5", 22, 5, Type.ftGnuCblZonedNumeric), //Type.ftFjZonedNumeric),
		}, {
			createField("Numeric-comp-fld-2", 1, 1, Type.ftBinaryBigEndian),
			createField("Numeric-comp-fld-4", 2, 2, Type.ftBinaryBigEndian),
			createField("Numeric-comp-fld-5", 4, 3, Type.ftBinaryBigEndian),
			createField("Numeric-comp-fld-6", 7, 3, Type.ftBinaryBigEndian),
			createField("Numeric-comp-fld-8", 10, 4, Type.ftBinaryBigEndian),
			createField("Numeric-comp-5-fld-2", 14, 1, Type.ftBinaryInt),
			createField("Numeric-comp-5-fld-4", 15, 2, Type.ftBinaryInt),
			createField("pcomp-fld-2", 17, 1, Type.ftPositiveBinaryBigEndian),
			createField("pcomp-fld-4", 18, 2, Type.ftPositiveBinaryBigEndian),
			createField("pic9-fld-5", 20, 5, Type.ftGnuCblZonedNumeric), //Type.ftFjZonedNumeric),
		},
	};
	
	private static ICobolIOBuilder streamIoBuilder = null;
	private static ICobolIOBuilder readerIoBuilder = null;
	

	public void testRightJustified() throws IOException {
		CommonCodeFields.check(	
				JRecordInterface1.COBOL.newIOBuilder(RIGHTJUST_COPYBOOK),
				new CommonCodeFields.CblBldrOptions(ICopybookDialects.FMT_MAINFRAME, Constants.IO_FIXED_LENGTH, false, ""),
				RIGHT_JUST_FIELDS,
				"Right-Just"
				);

	}
	
	public void test01() throws IOException {
		tst(getFileIOBuilder(), false);
	}
	
	
	public void test02() throws IOException {
		tst(getFileIOBuilder(), true);
	}

	
	public void test21() throws IOException {
		tst(getStreamIOBuilder(), false);
	}
	
	
	public void test22() throws IOException {
		tst(getStreamIOBuilder(), true);
	}

	
	public void test23() throws IOException {
		streamIoBuilder = null;
		tst(getStreamIOBuilder(), true);
	}
	
	public void test31() throws IOException {
		tst(getReaderIOBuilder(), false);
	}
	
	
	public void test32() throws IOException {
		tst(getReaderIOBuilder(), true);
	}

	
	public void test33() throws IOException {
		readerIoBuilder = null;
		tst(getReaderIOBuilder(), true);
	}
	public void testRecordDecider() throws IOException {
		RecordDecider rd = new RecordDecider() {
			@Override public int getPreferedIndex(AbstractLine line) {
				return 0;
			}
		};
		
		ICobolIOBuilder ioBuilder = getFileIOBuilder()
				.setRecordDecider(rd);
		
		assertTrue(ioBuilder.getLayout().getDecider() == rd);
	}

	
	public void tst(ICobolIOBuilder bldr, boolean dropRecordName) throws IOException {
		FieldDetail[][] expected = getExpectedFields(dropRecordName);
		
		for (int j = 0; j < FONTS.length; j++) {
			for (int k = 0; k < FILE_STRUCTURES.length; k++) {
				int i = 0;
				for (int d : DIALECTS) {
					CommonCodeFields.check(	
							bldr,
							new CommonCodeFields.CblBldrOptions(d, FILE_STRUCTURES[k], dropRecordName, FONTS[j]),
							expected[i++],
							FONTS[j] + " ~ " + (i-1) + ", " + d
							);
				}
			}
		}
	}
	
	private static FieldDetail[][] getExpectedFields(boolean dropRecordName) {
		FieldDetail[][] ret = FIELDS;
		int l = "Numeric-".length();
		if (dropRecordName) {
			ret = new FieldDetail[FIELDS.length][];
			for (int i = 0; i < ret.length; i++) {
				ret[i] = new FieldDetail[FIELDS[i].length];
				for (int j = 0; j < ret[i].length; j++) {
					String n = FIELDS[i][j].getName();
					if (n.startsWith("Numeric-")) {
						n = n.substring(l);
					}
					ret[i][j] = FieldDetail.newFixedWidthField(
							n, FIELDS[i][j].getType(), 
							FIELDS[i][j].getPos(), FIELDS[i][j].getLen(), 0, "");
				}
			}
		}
		return ret;
	}
	
	
//
//   Generate Fields
//	
//	public void testWrite() throws IOException {
//		tstWrite(getFileIOBuilder(), false);
//	}
//	
//	public void tstWrite(ICobolIOBuilder bldr, boolean dropFields) throws IOException {
//		for (int d : DIALECTS) {
//			check(	bldr,
//					new CblBldrOptions(d, FILE_STRUCTURES[0], dropFields, FONTS[0]),
//                  null,
//					);
//		}
//	}


	
	private static FieldDetail createField(String name, int pos, int len, int type) {
		return FieldDetail.newFixedWidthField(name, type, pos, len, 0, "");
	}
	
	private static ICobolIOBuilder getFileIOBuilder() {
		return JRecordInterface1.COBOL.newIOBuilder(COPYBOOK_NAME);
	}
	
	private static ICobolIOBuilder getStreamIOBuilder() throws FileNotFoundException {
		if (streamIoBuilder == null) {
			streamIoBuilder = JRecordInterface1.COBOL.newIOBuilder(new FileInputStream(COPYBOOK_NAME), Conversion.getCopyBookId(COPYBOOK_NAME));
		}
		return streamIoBuilder;
	}


	private static ICobolIOBuilder getReaderIOBuilder() throws FileNotFoundException {
		if (readerIoBuilder == null) {
			readerIoBuilder = JRecordInterface1.COBOL.newIOBuilder(new FileReader(COPYBOOK_NAME), Conversion.getCopyBookId(COPYBOOK_NAME));
		}
		return readerIoBuilder;
	}
}
