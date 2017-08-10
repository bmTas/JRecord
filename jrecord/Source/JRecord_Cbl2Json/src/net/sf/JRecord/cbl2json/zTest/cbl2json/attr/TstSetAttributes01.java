package net.sf.JRecord.cbl2json.zTest.cbl2json.attr;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import junit.framework.TestCase;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.builders.CblIOBuilderBase;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.IIOBuilder;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.impl.Cobol2JsonImp;


/**
 * Very basic check to ensure CblIOAttributes set correctly
 * 
 * @author Bruce Martin
 *
 */
public class TstSetAttributes01 extends TestCase {
	private static final String[] ATTR_NAMES = {
		"dialect",
		"splitCopybook",
		"copybookFileFormat",
		"fileOrganization",
		"font",
		"dropCopybookNameFromFields",
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
				Constants.NULL_INTEGER, Conversion.DEFAULT_ASCII_CHARSET, Boolean.FALSE, null	
		};
		checkAttributes(
				((CblIOBuilderBase)Cobol2Json
						.newCobol2Json("")
							.setDialect(ICopybookDialects.FMT_MAINFRAME)
				).getAllAttributes(), 
				attrs);
		checkAttributes(
				((CblIOBuilderBase)Cobol2Json
						.newCobol2Json("")
				).getAllAttributes(), 
				attrs);
		checkAttributes(
				((CblIOBuilderBase)Cobol2Json
						.newCb2Xml2Json("")
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
			checkAttributes(
					((CblIOBuilderBase)Cobol2Json
							.newCobol2Json("")
								.setFileOrganization(fs)
								.setDialect(ICopybookDialects.FMT_MAINFRAME)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)Cobol2Json
							.newCobol2Json("")
								.setFileOrganization(fs)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)Cobol2Json
							.newCb2Xml2Json("")
								.setFileOrganization(fs)
					).getAllAttributes(), 
					attrs);
			for (int d : dialects) {
				Object[] attrs1 = {
						d, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
						fs, "", Boolean.FALSE, null		
				};
				checkAttributes(
						((CblIOBuilderBase)Cobol2Json
								.newCobol2Json("")
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
					fs, Conversion.DEFAULT_ASCII_CHARSET, Boolean.FALSE, null		
			};
			checkAttributes(
					((CblIOBuilderBase)Cobol2Json
							.newCobol2Json("")
								.setFileOrganization(fs)
								.setDialect(ICopybookDialects.FMT_MAINFRAME)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)Cobol2Json
							.newCobol2Json("")
								.setFileOrganization(fs)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)Cobol2Json
							.newCb2Xml2Json("")
								.setFileOrganization(fs)
					).getAllAttributes(), 
					attrs);
			for (int d : dialects) {
				Object[] attrs1 = {
						d, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
						fs, Conversion.DEFAULT_ASCII_CHARSET, Boolean.FALSE, null		
				};
				checkAttributes(
						((CblIOBuilderBase)Cobol2Json
								.newCobol2Json("")
									.setFileOrganization(fs)
									.setDialect(d)
						).getAllAttributes(), 
						attrs1);
			}
		}
	}

	@SuppressWarnings({ "deprecation", "rawtypes" })
	public void testDefaultValues21() throws IOException {
		Object[] attrs = {
				ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
				Constants.NULL_INTEGER, Conversion.DEFAULT_ASCII_CHARSET, Boolean.FALSE, null	
		};
		ByteArrayInputStream is = new ByteArrayInputStream(new byte[]{});
		checkAttributes(
				((CblIOBuilderBase)Cobol2Json
						.newCobol2Json(is, "") 
							.setDialect(ICopybookDialects.FMT_MAINFRAME)
				).getAllAttributes(), 
				attrs);
		checkAttributes(
				((CblIOBuilderBase)Cobol2Json
						.newCb2Xml2Json("")
				).getAllAttributes(), 
				attrs);
		checkAttributes(
				((CblIOBuilderBase)Cobol2Json
						.newCobol2Json(is, "")
				).getAllAttributes(), 
				attrs);
	}

	@SuppressWarnings({ "deprecation", "rawtypes" })
	public void testDefaultValues22() throws IOException {
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
			checkAttributes(
					((CblIOBuilderBase)Cobol2Json
							.newCobol2Json(is, "")
								.setFileOrganization(fs)
								.setDialect(ICopybookDialects.FMT_MAINFRAME)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)Cobol2Json
							.newCb2Xml2Json("")
								.setFileOrganization(fs)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)Cobol2Json
							.newCobol2Json(is, "")
								.setFileOrganization(fs)
					).getAllAttributes(), 
					attrs);
			for (int d : dialects) {
				Object[] attrs1 = {
						d, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
						fs, "", Boolean.FALSE, null	
				};
				checkAttributes(
						((CblIOBuilderBase)Cobol2Json
								.newCobol2Json(is, "")
									.setFileOrganization(fs)
									.setDialect(d)
						).getAllAttributes(), 
						attrs1);


			}
		}
	}
	

	@SuppressWarnings({ "deprecation", "rawtypes" })
	public void testDefaultValues23() throws IOException {
		int[] fileStructures = {
				Constants.IO_BIN_TEXT, Constants.IO_BIN_CSV,
				Constants.IO_CONTINOUS_NO_LINE_MARKER, Constants.IO_CSV,
				Constants.IO_NAME_1ST_LINE, 
				Constants.IO_FIXED_LENGTH, Constants.IO_FIXED_LENGTH_RECORDS,
				Constants.IO_VB, Constants.IO_VB_DUMP, Constants.IO_VB_FUJITSU,
				Constants.IO_VB_GNU_COBOL
		};
		ByteArrayInputStream is = new ByteArrayInputStream(new byte[]{});
		
		for (int fs : fileStructures) {
			Object[] attrs = {
					ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
					fs, Conversion.DEFAULT_ASCII_CHARSET, Boolean.FALSE, null		
			};
			checkAttributes(
					((CblIOBuilderBase)Cobol2Json
							.newCobol2Json(is, "")
								.setFileOrganization(fs)
								.setDialect(ICopybookDialects.FMT_MAINFRAME)
					).getAllAttributes(), 
					attrs);
				checkAttributes(
					((CblIOBuilderBase)Cobol2Json
							.newCb2Xml2Json("")
								.setFileOrganization(fs)
					).getAllAttributes(), 
					attrs);
			checkAttributes(
					((CblIOBuilderBase)Cobol2Json
							.newCobol2Json(is, "")
								.setFileOrganization(fs)
					).getAllAttributes(), 
					attrs);
			
			for (int d : dialects) {
				Object[] attr1 = {
						d, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
						fs, Conversion.DEFAULT_ASCII_CHARSET, Boolean.FALSE, null	
				};
				checkAttributes(
						((CblIOBuilderBase)Cobol2Json
								.newCobol2Json(is, "")
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
	
	
//	public void testAttrs02() {
//		Object[] attrs = {
//				ICopybookDialects.FMT_FUJITSU, CopybookLoader.SPLIT_REDEFINE,  Cb2xmlConstants.USE_LONG_LINE,
//				Constants.IO_FIXED_LENGTH, "ibm037", Boolean.FALSE
//				
//		};
//		setAndCheckAttributes(attrs);
//	}
	
	
	public void testAttrs03() {
		Object[] attrs = {
				ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
				Constants.NULL_INTEGER, "", Boolean.FALSE
		};
		setAndCheckAttributes(attrs);
	}

	@SuppressWarnings("deprecation")
	private void setAndCheckAttributes(Object[] attrs) {
		Cobol2JsonImp bldr = (Cobol2JsonImp) Cobol2Json.newCobol2Json("");

		bldr.setDialect((Integer)attrs[0]);
		bldr.setSplitCopybook((Integer)attrs[1]);
		bldr.setCopybookFileFormat((Integer)attrs[2]);
		bldr.setFileOrganization((Integer)attrs[3]);
		bldr.setFont(attrs[4].toString());
		bldr.setDropCopybookNameFromFields((Boolean)attrs[5]);

		checkAttributes(attrs, bldr.getAllAttributes());
		
		@SuppressWarnings({ "unchecked", "rawtypes" })
		CblIOBuilderBase bld = (CblIOBuilderBase<IIOBuilder>) Cobol2Json. newCobol2Json("xyz");
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
			assertEquals( ATTR_NAMES[i], expected[i], attrs[i]);
		}
	}
	
}
