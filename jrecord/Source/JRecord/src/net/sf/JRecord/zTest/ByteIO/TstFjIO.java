package net.sf.JRecord.zTest.ByteIO;

import java.io.IOException;

import junit.framework.TestCase;
import net.sf.JRecord.ByteIO.FujitsuVbByteReader;
import net.sf.JRecord.ByteIO.FujitsuVbByteWriter;
import net.sf.JRecord.zTest.Common.TstConstants;


public class TstFjIO extends TestCase {

	   String fileName = TstConstants.TEMP_DIRECTORY + "fjTest1.tmp";
	   int recNo = 0;
	
	   public void testIO() throws Exception {
		   byte[] b1 = initBytes(64000);
		   byte[] b2 = initBytes(10);
		   byte[] b;
		   
		   FujitsuVbByteReader reader = new FujitsuVbByteReader();
		   FujitsuVbByteWriter writer = new FujitsuVbByteWriter();
		   
		   writer.open(fileName);
		   
		   writer.write(b1);
		   writer.write(b2);
		   writer.write(b1);
		   writer.write(b2);
		   writer.close();
		   
		   reader.open(fileName);
		   check(reader, b1);
		   check(reader, b2);
		   check(reader, b1);
		   check(reader, b2);
		   reader.close();
	   }
	   
	   private byte[] initBytes(int size) {
		   byte[] ret = new byte[size];
		   
		   for (int i = 0; i < size; i++) {
			   ret[i] = (byte) i;
		   }
		   
		   return ret;
	   }
	   
	   private void check(FujitsuVbByteReader reader, byte[] b) throws IOException {
		   byte[] in = reader.read();
		   
		   assertEquals("Length Check: " + recNo, b.length, in.length);
		   
		   for (int i = 0; i < b.length; i++) {
			   assertEquals("Record " + recNo + " Byte Number " + i, b[i], in[i]);
		   }
		   
		   recNo += 1;
	   }
}
