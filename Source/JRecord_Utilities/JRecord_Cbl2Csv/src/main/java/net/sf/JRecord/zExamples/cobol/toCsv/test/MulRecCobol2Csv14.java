package net.sf.JRecord.zExamples.cobol.toCsv.test;

import java.io.IOException;

import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalGroupSelection;
import net.sf.JRecord.cbl2csv.imp.CobolToCsvBldr;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class MulRecCobol2Csv14 {

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

		String dir = "/home/bruce/work/Cobol2Csv_user/zUser/";
        ICobolIOBuilder ioBldr = CobolToCsvBldr.newCobolIOBuilder(dir + "shw_cpy.txt")
                .setFont("cp037")
                .setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
                .setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
                .setRecordSelection("ESCROW-HEADER", new ExternalFieldSelection("EH-REC-TYP", "0"))
                .setRecordSelection("ESCROW-RECORD",
                                ExternalGroupSelection.newOr(new ExternalFieldSelection("ESC-REC-TYP", "1"),
                                                             new ExternalFieldSelection("ESC-REC-TYP", "2")))
                .setRecordSelection("ESCROW-DISB-RECORD", new ExternalFieldSelection("ESC-DISB-KEY-2", "3"))
                .setRecordSelection("ESCROW-INSD-RECORD", new ExternalFieldSelection("ESC-INSD-KEY-2", "4"));

        CobolToCsvBldr.newMultiRecordCsvBuilder()
        		.setCsvHeader(true)
        		.setWriteRecordName(true)
        		.setSeparator(";")
                .setLineReader(ioBldr
                                .newReader(dir + "CLESCRW.DAT.20180608054607.END_new_1"))
                .setOutputFile(dir + "j_Out1.csv")
                //.addRecordDetails(recordName, writer)
                .setReportInvalidFields(true)
            .run();

	}

}
