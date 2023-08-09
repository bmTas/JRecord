package net.sf.JRecord.zExamples.cobol.toCsv.test;

import java.io.IOException;

import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalGroupSelection;
import net.sf.JRecord.cbl2csv.imp.CobolToCsvBldr;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class MulRecCobol2Csv12 {

	public static void main(String[] args) throws IOException {
		/*  Proper definition:
		ICobolIOBuilder ioBldr = CobolToCsvBldr.newCobolIOBuilder("/home/bruce/work/Cobol2Csv_user/zUser/CopyBook.txt")
					.setFont("cp037")
					.setFileOrganization(IFileStructureConstants.IO_BIN_TEXT)
					.setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
	                .setRecordSelection("MFC1-HDR-REC", new ExternalFieldSelection("MFC1-HDR-RECORD-ID", "CISHDR"))
	                .setRecordSelection("MFC1-DTL-REC", new ExternalFieldSelection("MFC1-DTL-RECORD-ID", "CISDTL"))
	                .setRecordSelection("MFC1-NFU-REC", new ExternalFieldSelection("MFC1-NFU-RECORD-ID", "CISNFU"))
	                .setRecordSelection("MFC1-TRL-REC", new ExternalFieldSelection("MFC1-TRL-RECORD-ID", "CISTRL"))  
	         ;
	         
	         * Definition with or
	         */

		ICobolIOBuilder ioBldr = CobolToCsvBldr.newCobolIOBuilder("/home/bruce/work/Cobol2Csv_user/zUser/CopyBook.txt")
				.setFont("cp037")
				.setFileOrganization(IFileStructureConstants.IO_BIN_TEXT)
				.setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
                .setRecordSelection("MFC1-HDR-REC", new ExternalFieldSelection("MFC1-HDR-RECORD-ID", "CISHDR"))
                .setRecordSelection("MFC1-DTL-REC",
                		ExternalGroupSelection.newOr(
                						new ExternalFieldSelection("MFC1-DTL-RECORD-ID", "CISDTL"),
                						new ExternalFieldSelection("MFC1-NFU-RECORD-ID", "CISNFU")
                				))
                 .setRecordSelection("MFC1-TRL-REC", new ExternalFieldSelection("MFC1-TRL-RECORD-ID", "CISTRL"))  
         ;

		CobolToCsvBldr.newMultiRecordCsvBuilder()
					.setCsvHeader(true)
					.setSeparator(";")
					.setLineReader(ioBldr.newReader("/home/bruce/work/Cobol2Csv_user/zUser/0000_RIOT.C.D.JRIO1000.MFCORE.G0018V00"))
					.setOutputFile("/home/bruce/work/Cobol2Csv_user/zUser/j_Out.csv")
				.run();

	}

}
