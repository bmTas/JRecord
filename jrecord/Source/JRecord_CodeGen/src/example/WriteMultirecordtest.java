package example;

import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

import example.ioBuilderWithSchema.schema.RecordHeaderRecord;
import example.ioBuilderWithSchema.schema.RecordDetailRecord;
import example.ioBuilderWithSchema.schema.RecordTrailerRecord;
import example.ioBuilderWithSchema.schema.SchemaMultirecordtest;


/**
 * Write Cobol file using a Cobol Copybook (Multirecordtest).
 *
 * This Generated program is intended as an example of using JRecord
 * rather than a useful program (that compiles - it wont).
 * You should regard it as a starting point and modify 
 * it according to needs
 */
public final class WriteMultirecordtest {

    private String testDir        = "G:/temp/";
    private String salesFileOut   = testDir + "DTAR020out.bin";
    private String copybookName   = "/F:/Work/EclipseWorkspaces/std_workspace/JRecord_CodeGen/bin/test/gen/MultiRecordTest.cbl"; 

    private RecordHeaderRecord rHeaderRecord;
    private RecordDetailRecord rDetailRecord;
    private RecordTrailerRecord rTrailerRecord;
    
    /**
     * Example of LineReader  classes
     */
    public WriteMultirecordtest() {
        super();

        try {
            ICobolIOBuilder iob = JRecordInterface1.COBOL
                                       .newIOBuilder(copybookName)
                                           .setFont("")
                                           .setFileOrganization(Constants.IO_FIXED_LENGTH)
                                           .setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
                                           .setDropCopybookNameFromFields(false);  
            SchemaMultirecordtest schemaMultirecordtest
                    = new SchemaMultirecordtest(iob.getLayout()); 
            rHeaderRecord = schemaMultirecordtest.recordHeaderRecord;
            rDetailRecord = schemaMultirecordtest.recordDetailRecord;
            rTrailerRecord = schemaMultirecordtest.recordTrailerRecord;
            AbstractLineWriter writer = iob.newWriter(salesFileOut);
 
            writer.write(createHeaderRecord(iob, 111));
            writer.write(createDetailRecord(iob, 112));
            writer.write(createDetailRecord(iob, 212));
            writer.write(createDetailRecord(iob, 312));
            writer.write(createDetailRecord(iob, 412));
            writer.write(createDetailRecord(iob, 512));
            writer.write(createTrailerRecord(iob, 113));

            writer.close();
        } catch (Exception e) {
             System.out.println();

            e.printStackTrace();
        }
    }

    private AbstractLine createHeaderRecord(ICobolIOBuilder iob, int data) throws RecordException, IOException {
        AbstractLine l = iob.newLine();    
        
        l.getFieldValue(rHeaderRecord.fldRecordType).set(data);
        l.getFieldValue(rHeaderRecord.fldCreationDate).set(data);
        l.getFieldValue(rHeaderRecord.fldVersion).set(data);
    
        return l;
    }

    private AbstractLine createDetailRecord(ICobolIOBuilder iob, int data) throws RecordException, IOException {
        AbstractLine l = iob.newLine();    
        
        l.getFieldValue(rDetailRecord.fldRecordType).set(data);
        l.getFieldValue(rDetailRecord.fldField1).set(data);
        l.getFieldValue(rDetailRecord.fldField2).set(data);
        l.getFieldValue(rDetailRecord.fldField3).set(data);
    
        return l;
    }

    private AbstractLine createTrailerRecord(ICobolIOBuilder iob, int data) throws RecordException, IOException {
        AbstractLine l = iob.newLine();    
        
        l.getFieldValue(rTrailerRecord.fldRecordType).set(data);
        l.getFieldValue(rTrailerRecord.fldRecordCount).set(data);
    
        return l;
    }

    public static void main(String[] args) {
        new WriteMultirecordtest();
    }
}

