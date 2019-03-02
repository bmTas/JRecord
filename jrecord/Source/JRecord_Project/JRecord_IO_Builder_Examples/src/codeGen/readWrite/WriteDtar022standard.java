package codeGen.readWrite;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class WriteDtar022standard {

	public WriteDtar022standard() {
        String outputFileName  ="DTAR022out2.bin";
        String outCopybookName = getFilePath("DTAR022.cbl");

        try {
            FieldNamesDtar022.RecordDtar022 rDtar022 = FieldNamesDtar022.RECORD_DTAR022;
	        ICobolIOBuilder iobWrite = JRecordInterface1.COBOL
	                .newIOBuilder(outCopybookName)
	                   .setFont("cp037")
	                   .setFileOrganization(Constants.IO_FIXED_LENGTH)
	                   .setSplitCopybook(CopybookLoader.SPLIT_NONE)
	                       ;  
	        AbstractLineWriter writer = iobWrite.newWriter(outputFileName);
	        AbstractLine dtar022Line  = iobWrite.newLine();

            dtar022Line.getFieldValue(rDtar022.keycodeNo).set("223");
            dtar022Line.getFieldValue(rDtar022.theDate)  .set(22);
            dtar022Line.getFieldValue(rDtar022.deptNo)   .set(22);
            dtar022Line.getFieldValue(rDtar022.qtySold)  .set(5);
            dtar022Line.getFieldValue(rDtar022.salePrice).set("123.45");

            writer.write(dtar022Line);

            dtar022Line.getFieldValue(rDtar022.keycodeNo).set("654");
            dtar022Line.getFieldValue(rDtar022.theDate)  .set(65);
            dtar022Line.getFieldValue(rDtar022.deptNo)   .set(65);
            dtar022Line.getFieldValue(rDtar022.qtySold)  .set(12);
            dtar022Line.getFieldValue(rDtar022.salePrice).set("34.12");

            writer.write(dtar022Line);

        } catch (Exception e) {
            e.printStackTrace();
        }

	}

    
    private String getFilePath(String name) {
    	return this.getClass().getResource(name).getFile();

    }

    public static void main(String[] args) {
        new WriteDtar022standard();
    }

}
