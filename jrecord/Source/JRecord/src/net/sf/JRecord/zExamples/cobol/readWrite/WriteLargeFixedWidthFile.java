package net.sf.JRecord.zExamples.cobol.readWrite;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Numeric.Convert;
import net.sf.JRecord.zTest.Common.TstConstants;
import net.sf.cb2xml.def.Cb2xmlConstants;

/**
 * Read / Write Mainframe Cobol file using a Cobol Copybook
 * 
 * 
 * @author Bruce Martin
 *
 */
public final class WriteLargeFixedWidthFile {


    private String installDir     = TstConstants.SAMPLE_DIRECTORY;
    private String salesFile      = installDir + "DTAR020.bin";
    private String salesFileOut   = "F:\\Temp\\Large\\DTAR020_Large.cbl";
    private String copybookName   = TstConstants.COBOL_DIRECTORY + "DTAR020.cbl";

    /**
     * Example of LineReader / LineWrite classes
     */
    private WriteLargeFixedWidthFile() {
        super();

        AbstractLine saleRecord;
        List<AbstractLine> lines = new ArrayList<AbstractLine>(300);

        try {
            int fileStructure = Constants.IO_FIXED_LENGTH;
            CobolIoProvider ioProvider = CobolIoProvider.getInstance();
            AbstractLineReader reader  = ioProvider.getLineReader(
            		fileStructure, Convert.FMT_MAINFRAME, CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS, 
            		copybookName, salesFile
            );

            LayoutDetail l = reader.getLayout();

            AbstractLineWriter writer  = LineIOProvider.getInstance().getLineWriter(l);
            
            writer.open(new BufferedOutputStream(new FileOutputStream(salesFileOut), 256 * 256 * 64));

            while ((saleRecord = reader.read()) != null) {
            	lines.add(saleRecord);
            }
            reader.close();
            
            
            for (int i = 0; i < 300000000; i++) { 
                writer.write(lines.get(i % lines.size()));
                if (i % 1000000 == 0) {
                	System.out.print("\t" + (i /1000000));
                	 if (i % 10000000 == 0) System.out.println();
                }
            }

            
            writer.close();
        } catch (Exception e) {
            System.out.println();

            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
    	new WriteLargeFixedWidthFile();
    }
}
