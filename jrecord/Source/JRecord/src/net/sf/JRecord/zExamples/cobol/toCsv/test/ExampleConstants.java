package net.sf.JRecord.zExamples.cobol.toCsv.test;

import java.io.File;

public class ExampleConstants {

	public static final String TEMP_DIR = checkExists("G:\\Temp\\", System.getProperty("java.io.tmpdir"));
	
	
	private static String checkExists(String dir1, String dir2) {
		if (new File(dir1).exists()) {
			return dir1;
		}
		return dir2;
	}
			
			
}
