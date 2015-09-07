package net.sf.JRecord.zTest.Cobol.iobuilder;

import java.io.FileInputStream;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.def.IO.builders.ICobolMultiCopybookIOBuilder;

public class TstMultiCopybookRead11 extends TestCase {


	ICobolIOBuilder ioBld = null, ioBld2 = null;

	public void testRead1() throws Exception {
		TstMultiCopybookCommon tst = new TstMultiCopybookCommon(false, getIoBuilder());
		
		tst.tstRead();
	}
	
	public void testRead2() throws Exception {
		TstMultiCopybookCommon tst = new TstMultiCopybookCommon(true, getIoBuilder());
		
		tst.tstRead();
	}

	public void testRead3() throws Exception {
		TstMultiCopybookCommon tst = new TstMultiCopybookCommon(false, getIoBuilder2());
		
		tst.tstRead();
	}
	
	public void testRead4() throws Exception {
		TstMultiCopybookCommon tst = new TstMultiCopybookCommon(true, getIoBuilder2());
		
		tst.tstRead();
	}
	
	private ICobolIOBuilder getIoBuilder() throws Exception {
		if (ioBld == null) {
			ioBld = loadRecordDefinition();
		}
		
		return ioBld;
	}
	
	private ICobolIOBuilder getIoBuilder2() throws Exception {
		if (ioBld2 == null) {
			ioBld2 = loadRecordDefinition2();
		}
		
		return ioBld2;
	}

    /**
     * Load RecordLayout (schema) from the Cobol copybook
     * @throws Exception
     */
    private ICobolIOBuilder loadRecordDefinition() throws Exception{
    	
    	String copyFileName2 = this.getClass().getResource("MultiRecordTest11.cbl").getFile();
 
    	ICobolIOBuilder IOBldr = JRecordInterface1.COBOL
				.newIOBuilder(copyFileName2)
     						.setSplitCopybook(CopybookLoader.SPLIT_HIGHEST_REPEATING);
    	return setupRecordDefinition(IOBldr);
    }
    
    /**
     * Load RecordLayout (schema) from the Cobol copybook
     * @throws Exception
     */
    private ICobolIOBuilder loadRecordDefinition2() throws Exception{
    	
    	String copyFileName2 = this.getClass().getResource("MultiRecordTest11.cbl").getFile();
 
    	ICobolIOBuilder IOBldr = JRecordInterface1.COBOL
				.newIOBuilder(new FileInputStream(copyFileName2), "MultiRecordTest")
      						.setSplitCopybook(CopybookLoader.SPLIT_HIGHEST_REPEATING);
    	return setupRecordDefinition(IOBldr);
    }


	/**
	 * @param IOBldr
	 * @return
	 */
	private ICobolIOBuilder setupRecordDefinition(
			ICobolIOBuilder IOBldr) {
		IOBldr			.setDialect(ICopybookDialects.FMT_FUJITSU)
     					.setFileOrganization(Constants.IO_CONTINOUS_NO_LINE_MARKER)
 					;
    	
    		
    	IOBldr.setRecordSelection("Header-Record", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.HEADER_ID))
    	      .setRecordSelection("Detail-Record", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.DETAIL_ID))
    	      .setRecordSelection("Trailer-Record", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.TRAILER_ID))
    	      .setRecordSelection("Detail-Record-A", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.REC_A_ID))
    	      .setRecordSelection("Detail-Record-B", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.REC_B_ID))
    	;

    	return IOBldr;
	}
    
    private ExternalFieldSelection newFieldSelection(String fieldName, String value) {
    	ExternalFieldSelection r = new ExternalFieldSelection(fieldName, value);
    	r.setCaseSensitive(false);
    	return r;
    }
}
