package net.sf.JRecord.zTest.Cobol.iobuilder;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.def.IO.builders.ICobolMultiCopybookIOBuilder;

public class TstMultiCopybookRead06 extends TestCase {


	ICobolIOBuilder ioBld = null;

	public void testRead1() throws Exception {
		TstMultiCopybookCommon tst = new TstMultiCopybookCommon(false, getIoBuilder());
		
		tst.tstRead();
	}
	
	public void testRead2() throws Exception {
		TstMultiCopybookCommon tst = new TstMultiCopybookCommon(true, getIoBuilder());
		
		tst.tstRead();
	}

	
	private ICobolIOBuilder getIoBuilder() throws Exception {
		if (ioBld == null) {
			ioBld = loadRecordDefinition();
		}
		
		return ioBld;
	}
	
    /**
     * Load RecordLayout (schema) from the Cobol copybook
     * @throws Exception
     */
    private ICobolIOBuilder loadRecordDefinition() throws Exception{
    	
    	String copyFileName1 = this.getClass().getResource("RecordA.cbl").getFile();
    	String copyFileName2 = this.getClass().getResource("MultiRecordTest03.cbl").getFile();
    	String copyFileName3 = this.getClass().getResource("RecordB.cbl").getFile();
 
    	ICobolMultiCopybookIOBuilder IOBldr = JRecordInterface1.COBOL
    				.newMultiCopybookIOBuilder("MultiRecordTest")
    					.setDialect(ICopybookDialects.FMT_FUJITSU)
     					.setFileOrganization(Constants.IO_CONTINOUS_NO_LINE_MARKER)
     					.addCopyBook(copyFileName1)
     						.setRecordSelectionCurrentCopybook(
     								newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.REC_A_ID))
     					.addCopyBook(copyFileName2)
     						.setSplitCopybook(CopybookLoader.SPLIT_REDEFINE)
     					.addCopyBook(copyFileName3)
      						.setRecordSelectionCurrentCopybook(
      								newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.REC_B_ID))
 					;
    	
    		
    	IOBldr.setRecordSelection("Header-Record", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.HEADER_ID))
    	      .setRecordSelection("Detail-Record", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.DETAIL_ID))
    	      .setRecordSelection("Trailer-Record", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.TRAILER_ID));

    	return IOBldr;
    }
    
    private ExternalFieldSelection newFieldSelection(String fieldName, String value) {
    	ExternalFieldSelection r = new ExternalFieldSelection(fieldName, value);
    	r.setCaseSensitive(false);
    	return r;
    }
}
