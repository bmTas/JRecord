package example;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

import example.ioBuilderWithSchema.schema.RecordHeaderRecord;
import example.ioBuilderWithSchema.schema.RecordDetailRecord;
import example.ioBuilderWithSchema.schema.RecordTrailerRecord;
import example.ioBuilderWithSchema.schema.SchemaMultirecordtest;
	

/**
 * Read Cobol file using a Cobol Copybook (Multirecordtest).
 *
 * This Generated program is intended as an example of using JRecord
 * rather than a useful program (that compiles - it wont).
 * You should regard it as a starting point and modify 
 * it according to needs
 */
public final class ReadMultirecordtest {

    private String testDir        = "G:/temp/";
    private String salesFile      = testDir + "DTAR020.bin";

    private String copybookName   = "/F:/Work/EclipseWorkspaces/std_workspace/JRecord_CodeGen/bin/test/gen/MultiRecordTest.cbl"; 
 
    /**
     * Example of LineReader  classes
     */
    public ReadMultirecordtest() {
        super();

        AbstractLine saleRecord;
        int lineNum = 0;

        try {
            ICobolIOBuilder iob = JRecordInterface1.COBOL
                                       .newIOBuilder(copybookName)
                                           .setFont("")
                                           .setFileOrganization(Constants.IO_FIXED_LENGTH)
                                           .setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
                                           .setDropCopybookNameFromFields(false);  
            SchemaMultirecordtest schemaMultirecordtest
                    = new SchemaMultirecordtest(iob.getLayout()); 
            RecordHeaderRecord rHeaderRecord = schemaMultirecordtest.recordHeaderRecord;
            RecordDetailRecord rDetailRecord = schemaMultirecordtest.recordDetailRecord;
            RecordTrailerRecord rTrailerRecord = schemaMultirecordtest.recordTrailerRecord;
            AbstractLineReader reader = iob.newReader(salesFile);
 
            while ((saleRecord = reader.read()) != null) {
                lineNum += 1;
                if ("H".equalsIgnoreCase(saleRecord.getFieldValue(rHeaderRecord.fldRecordType).asString())) {
                   System.out.println(
                              saleRecord.getFieldValue(rHeaderRecord.fldRecordType).asString()
                      + " " + saleRecord.getFieldValue(rHeaderRecord.fldCreationDate).asString()
                      + " " + saleRecord.getFieldValue(rHeaderRecord.fldVersion).asString()
                   );
                }
                if ("D".equalsIgnoreCase(saleRecord.getFieldValue(rHeaderRecord.fldRecordType).asString())) {
                   System.out.println(
                              saleRecord.getFieldValue(rDetailRecord.fldRecordType).asString()
                      + " " + saleRecord.getFieldValue(rDetailRecord.fldField1).asString()
                      + " " + saleRecord.getFieldValue(rDetailRecord.fldField2).asString()
                      + " " + saleRecord.getFieldValue(rDetailRecord.fldField3).asString()
                   );
                }
                if ("T".equalsIgnoreCase(saleRecord.getFieldValue(rHeaderRecord.fldRecordType).asString())) {
                   System.out.println(
                              saleRecord.getFieldValue(rTrailerRecord.fldRecordType).asString()
                      + " " + saleRecord.getFieldValue(rTrailerRecord.fldRecordCount).asString()
                   );
                }
            }

            reader.close();
        } catch (Exception e) {
            System.out.println("~~> " + lineNum + " " + e);
            System.out.println();

            e.printStackTrace();
        }
    }
    
    public static void main(String[] args) {
        new ReadMultirecordtest();
    }
}

