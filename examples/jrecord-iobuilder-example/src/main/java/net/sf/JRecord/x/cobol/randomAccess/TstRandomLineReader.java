package net.sf.JRecord.x.cobol.randomAccess;

import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 * Test random reader
 * @author Bruce Martin
 *
 */
public class TstRandomLineReader {

	public static void main(String[] args) throws IOException {
	    String installDir     = TstConstants.SAMPLE_DIRECTORY;
	    String salesFile      = installDir + "DTAR020.bin";
	    String copybookName   = TstConstants.COBOL_DIRECTORY + "DTAR020.cbl";
		ICobolIOBuilder iob = JRecordInterface1.COBOL
		            .newIOBuilder(copybookName)
		            	.setDropCopybookNameFromFields(true)
		                .setFont("cp037");
		RandomLineReader reader = new RandomLineReader(salesFile, iob);
		int[] lineNums = {5, 10, 15, 20};
		
		for (int num : lineNums) {
			AbstractLine l = reader.read(num);
            System.out.println(l.getFieldValue("KEYCODE-NO").asString()
                    + " " + l.getFieldValue("QTY-SOLD").asString()
                    + " " + l.getFieldValue("SALE-PRICE").asString()
                    );
		}
		
		reader.close();
	}

}
