package net.sf.JRecord.zTest.cg.pojo;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import net.sf.JRecord.cgen.def.IReader;
import net.sf.JRecord.cgen.impl.io.IoBuilder;
import net.sf.JRecord.zTest.cg.pojo.data.LineDtar020Pojo;
import net.sf.JRecord.zTest.cg.pojo.io.IoBuilderDtar020;


class TestCodeGenPojo {
	private static final String[][] EXPECTED_LINES = {
			{"63604808","20","40118","170","1","4.87"},
			{"69684558","20","40118","280","1","19.00"},
			{"69684558","20","40118","280","-1","-19.00"},
			{"69694158","20","40118","280","1","5.01"},
			{"62684671","20","40118","685","1","69.99"},
			{"62684671","20","40118","685","-1","-69.99"},
			{"61664713","59","40118","335","1","17.99"},
			{"61664713","59","40118","335","-1","-17.99"},
			{"61684613","59","40118","335","1","12.99"},
			{"68634752","59","40118","410","1","8.99"},
			{"60694698","59","40118","620","1","3.99"},
			{"60664659","59","40118","620","1","3.99"},
			{"60614487","59","40118","878","1","5.95"},
			{"68654655","166","40118","60","1","5.08"},
			{"69624033","166","40118","80","1","18.19"},
			{"60604100","166","40118","80","1","13.30"},
			{"68674560","166","40118","170","1","5.99"},
	};

	private String dtar020Copybook = getFullFileName("DTAR020.cbl");
	private String dtar020DataFile = getFullFileName("DTAR020_tst1.bin");
	
	@Test
	void testReader() {
        IReader<LineDtar020Pojo> reader = null;
        int lineNum = 0;
        try {
            IoBuilder<LineDtar020Pojo> iob = IoBuilderDtar020.newIoBuilder(dtar020Copybook);
            LineDtar020Pojo line;

            reader = iob.newReader(dtar020DataFile);
            
            while ((line = reader.read()) != null) {
            	check(lineNum++, line);
                 
                //print(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }

	}

	@Test
	void testStream() throws IOException {
        IoBuilder<LineDtar020Pojo> iob = IoBuilderDtar020.newIoBuilder(dtar020Copybook);
        List<LineDtar020Pojo> lineList = iob.stream(dtar020DataFile)
           .collect(Collectors.toList());
        /*
            .forEach(line -> print(line));
         */
         
        for (int idx = 0; idx < lineList.size(); idx++) {
        	check(idx, lineList.get(idx));
        }
	}


	private void print(LineDtar020Pojo line) {
		System.out.println(
		       "{\"" + line.getKeycodeNo() + "\","
		      + "\"" + line.getStoreNo() + "\","
		      + "\"" + line.getDate() + "\","
		      + "\"" + line.getDeptNo() + "\","
		      + "\"" + line.getQtySold() + "\","
		      + "\"" + line.getSalePrice() + "\"},"
		   );
	}


	private void check(int index, LineDtar020Pojo line) {
		String[] expected = EXPECTED_LINES[index];
		int idx = 0;
		
		assertEquals(expected[idx++], "" + line.getKeycodeNo());
		assertEquals(expected[idx++], "" + line.getStoreNo());
		assertEquals(expected[idx++], "" + line.getDate());
		assertEquals(expected[idx++], "" + line.getDeptNo());
		assertEquals(expected[idx++], "" + line.getQtySold());
		assertEquals(expected[idx++], "" + line.getSalePrice());
	}
	
	
	//DTAR020_tst1.bin
	private static String getFullFileName(String filename) {
		return TestCodeGenPojo.class.getResource(filename).getFile();
	}
}
