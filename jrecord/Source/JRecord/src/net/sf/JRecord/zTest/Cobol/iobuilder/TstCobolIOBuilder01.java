package net.sf.JRecord.zTest.Cobol.iobuilder;

import java.io.IOException;

import junit.framework.TestCase;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.IO.builders.CblIOBuilderBase;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.cb2xml.def.Cb2xmlConstants;


/**
 * Very basic check to ensure CblIOAttributes set correctly
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
	};
	
//	int splitCopybook = CopybookLoader.SPLIT_NONE;
//	String font = "";
//	int copybookFileFormat = 1; // actually Cb2xmlConstants.USE_STANDARD_COLUMNS;
//	int fileOrganization = Constants.NULL_INTEGER;
//	boolean dropCopybookNameFromFields = false;
	
	public void testDefaultValues() {
		Object[] attrs = {
				ICopybookDialects.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
				Constants.NULL_INTEGER, "", Boolean.FALSE
				
		};
		checkAttributes(new CblIoBldr(ICopybookDialects.FMT_MAINFRAME).getAttrs(), attrs);
	}
	
	public void testAttrs01() {
		Object[] attrs = {
				ICopybookDialects.FMT_OPEN_COBOL, CopybookLoader.SPLIT_01_LEVEL,  Cb2xmlConstants.USE_COLS_6_TO_80,
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

	private void setAndCheckAttributes(Object[] attrs) {
		CblIoBldr bldr = new CblIoBldr((Integer)attrs[0]);
		bldr.setSplitCopybook((Integer)attrs[1]);
		bldr.setCopybookFileFormat((Integer)attrs[2]);
		bldr.setFileOrganization((Integer)attrs[3]);
		bldr.setFont(attrs[4].toString());
		bldr.setDropCopybookNameFromFields((Boolean)attrs[5]);

		checkAttributes(attrs, bldr.getAttrs());
	}

	private void checkAttributes(Object[] expected, Object[] attrs) {
		for (int i = 0; i < expected.length; i++) {
			assertEquals(ATTR_NAMES[i],expected[i], attrs[i]);
		}
	}
	
	private static class CblIoBldr extends CblIOBuilderBase {

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


		public Object[] getAttrs() {
			return super.getAllAttributes();
		}
	}
}
