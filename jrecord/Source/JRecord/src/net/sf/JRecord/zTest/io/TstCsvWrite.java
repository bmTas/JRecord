package net.sf.JRecord.zTest.io;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.FileReader;
import java.util.StringTokenizer;

import junit.framework.TestCase;
import net.sf.JRecord.CsvParser.ICsvLineParser;
import net.sf.JRecord.CsvParser.ParserManager;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.RecordEditorXmlLoader;
import net.sf.JRecord.IO.TextLineReader;
import net.sf.JRecord.IO.TextLineWriter;
import net.sf.JRecord.zTest.Common.TstConstants;


public class TstCsvWrite  extends TestCase {
	String[][] lines = {
			{"69684558","20","40118","280","1","19.00"},
			{"69684558","20","40118","280","-1","-19.00"},
			{"69684558","20","40118","280","1","5.01"},
			{"69694158","20","40118","280","1","19.00"},
			{"69694158","20","40118","280","-1","-19.00"},
			{"69694158","20","40118","280","1","5.01"},
			{"63604808","20","40118","170","1","4.87"},
			{"62684671","20","40118","685","1","69.99"},
			{"62684671","20","40118","685","-1","-69.99"},
			{"64634429","20","40118","957","1","3.99"},
			{"66624458","20","40118","957","1","0.89"},
			{"63674861","20","40118","957","10","2.70"},
			{"65674532","20","40118","929","1","3.59"},
			{"64614401","59","40118","957","1","1.99"},
			{"64614401","59","40118","957","1","1.99"},
			{"61664713","59","40118","335","1","17.99"},
	};
	
	public void testCsvParser0()  throws Exception{
		writeFile(TstConstants.TEMP_DIRECTORY + "CsvFile0.txt", "0", "Char", false);
		writeFile(TstConstants.TEMP_DIRECTORY + "CsvFile0n.txt", "0", "Char", true);
	}
	
	
	public void testCsvParser1()  throws Exception{
		writeFile(TstConstants.TEMP_DIRECTORY + "CsvFile1.txt", "1", "Char", false);
		writeFile(TstConstants.TEMP_DIRECTORY + "CsvFile1n.txt", "1", "Char", true);
	}
	
	
	public void testCsvParser2()  throws Exception{
		writeFile(TstConstants.TEMP_DIRECTORY + "CsvFile2a.txt", "2", "Char", false);
	
		writeFile(TstConstants.TEMP_DIRECTORY + "CsvFile2b.txt", "2", 
				"Num (Right Justified zero padded)", false);
		
		writeFile(TstConstants.TEMP_DIRECTORY + "CsvFile2an.txt", "2", "Char", true);
		
		writeFile(TstConstants.TEMP_DIRECTORY + "CsvFile2bn.txt", "2", 
				"Num (Right Justified zero padded)", true);
	}

	
	
	public void testCsvParser3()  throws Exception{
		writeFile(TstConstants.TEMP_DIRECTORY + "CsvFile3.txt", "3", "Char", false);
		writeFile(TstConstants.TEMP_DIRECTORY + "CsvFile3n.txt", "3", "Char", true);
	}
	
	
	public void testCsvParser4()  throws Exception{
		writeFile(TstConstants.TEMP_DIRECTORY + "CsvFile4.txt", "4", "Char", false);
		writeFile(TstConstants.TEMP_DIRECTORY + "CsvFile4n.txt", "4", "Char", true);
	}
	
	
	public void testCsvParser5()  throws Exception{
		writeFile(TstConstants.TEMP_DIRECTORY + "CsvFile5a.txt", "5", "Char", false);
	
		writeFile(TstConstants.TEMP_DIRECTORY + "CsvFile5b.txt", "5", 
				"Num (Right Justified zero padded)", false);
		
		writeFile(TstConstants.TEMP_DIRECTORY + "CsvFile5an.txt", "5", "Char", true);
		
		writeFile(TstConstants.TEMP_DIRECTORY + "CsvFile5bn.txt", "5", 
				"Num (Right Justified zero padded)", true);
	}


	private void writeFile(String filename, String style, String type, boolean nameFirstLine)  throws Exception {
		LayoutDetail layout;
		
		layout = writeFile1(filename, style, type, nameFirstLine);
		checkCsvFile(filename, style, type, "forward", nameFirstLine, layout);

		layout = writeFile2(filename, style, type, nameFirstLine);
		checkCsvFile(filename, style, type, "backward", nameFirstLine, layout);
	}
	
	private LayoutDetail writeFile1(String filename, String style, String type, boolean namesFirstLine)  throws Exception {
		LayoutDetail layout = getLayout(style, type);
		Line line = new Line(layout);
		TextLineWriter writer = new TextLineWriter(namesFirstLine);
		
		writer.open(filename);
		for (int i = 0; i < lines.length; i++) {
			for (int j = 0; j < lines[i].length; j++) { 
				line.setField(0, j, lines[i][j]);
			}
			writer.write(line);
		}
		writer.close();	
		
		return layout;
	}
	
	private LayoutDetail writeFile2(String filename, String style, String type, boolean namesFirstLine)  throws Exception {
		LayoutDetail layout = getLayout(style, type);
		Line line = new Line(layout);
		TextLineWriter writer = new TextLineWriter(namesFirstLine);
		
		writer.open(filename);
		for (int i = 0; i < lines.length; i++) {
			for (int j = lines[i].length -1; j>= 0; j--) { 
				line.setField(0, j, lines[i][j]);
			}
			writer.write(line);
		}
		writer.close();	
		
		return layout;
	}
	

	
	private void checkCsvFile(String filename, String style, String type, String code, 
			boolean namesFirstLine, LayoutDetail layout)   throws Exception {
		
		BufferedReader reader = new BufferedReader(new FileReader(filename));
		StringTokenizer tok;
		String s;
		boolean isError;
		boolean isOk = true;
		boolean quoteCharFields = "2".equals(style) || "5".equals(style);

		
		if (namesFirstLine) {
			RecordDetail rec = layout.getRecord(0);
			ICsvLineParser parser = ParserManager.getInstance().get(rec.getRecordStyle());
			String quote = "";
			String name, name1;
			tok = new StringTokenizer(reader.readLine(), "\t");
			
			if (parser != null && parser.isQuoteInColumnNames()) {
	        	quote = rec.getQuote();
	        }
			
			for (int i = 0; i < rec.getFieldCount(); i++) {
				name = quote + rec.getField(i).getName() + quote;
				name1 = tok.nextToken();
				
				if (! name.equals(name1)) {
					if (isOk) {
						System.out.print("Error With Column Names: ");
					}
					System.out.print(i + " ! " + name + " <> " + name1 + " !   ");
					isOk = false;
				}
			}
			
			if (! isOk) {
				System.out.println();
				System.out.println();
			}
			
			boolean firstError = true;
			TextLineReader lr = new TextLineReader(null, true); 
			lr.open(filename, layout);
			lr.read();
			LayoutDetail layout2 = lr.getLayout();
			lr.close();
			for (int i = 0; i < rec.getFieldCount(); i++) {
				name = rec.getField(i).getName();
				name1 = layout2.getRecord(0).getField(i).getName();
				
				if (! name.equals(name1)) {
					if (firstError) {
						System.out.print("Error With Column Names 2 : ");
						firstError = false;
					}
					System.out.print(i + " ! " + name + " <> " + name1 + " !   ");
					isOk = false;
				}
			}
			
			if (! isOk) {
				System.out.println();
				System.out.println();
			}
		}
		
		for (int i = 0; i < lines.length; i++) {
			tok = new StringTokenizer(reader.readLine(), "\t");
			isError = false;
			for (int j = 0; j < lines[i].length; j++) { 
				s = tok.nextToken();
				if ((! quoteCharFields) || (j < 2 && ! "Char".equals(type))) {
					isError = checkStr(lines[i][j], s, i + ", " + j);
				} else {
					isError = checkStr( "\'" + lines[i][j] + "\'", s, i + ", " + j);
				}
			}
			if (isError) {
				System.out.println();
				isOk = false;
			}
		}
		
		assertTrue("Error in " + code
				+ "  Style: " + style + " , type: " + type, isOk);
		System.out.println("Tested " + code
				+ "  Style: " + style + " , type: " + type);
	}
	
	private boolean checkStr(String t, String s, String pos) {
		if (! t.equals(s)) {
			System.out.print(" Error: " + pos
					+ " --> "  + t + " ~ " + s);
			return true;
		}
		return false;
	}
	
	private LayoutDetail getLayout(String style, String type) throws Exception {
		String c = "<RECORD RECORDNAME=\"csv_DTAR020\" COPYBOOK=\"csv_DTAR020\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"CSV_NAME_1ST_LINE\""
			+ "       STYLE=\"" + style + "\" RECORDTYPE=\"GroupOfRecords\" LIST=\"Y\" QUOTE=\"'\" RecSep=\"default\">"
			+ "	<RECORDS>"
			+ "		<RECORD RECORDNAME=\"\" COPYBOOK=\"csv_DTAR020_\" DELIMITER=\"&lt;Tab&gt;\" FILESTRUCTURE=\"CSV_NAME_1ST_LINE\" STYLE=\"" + style + "\" "
			+ "	        	RECORDTYPE=\"Delimited\" LIST=\"N\" QUOTE=\"'\" RecSep=\"default\">"
			+ "			<FIELDS>"
			+ "				<FIELD NAME=\"keycode-no\" DESCRIPTION=\"keycode-no\" POSITION=\"1\" TYPE=\"" + type + "\"/>"
			+ "				<FIELD NAME=\"Store-No\"   DESCRIPTION=\"Store-No\"   POSITION=\"2\" TYPE=\"" + type + "\"/>"
			+ "				<FIELD NAME=\"Date\"       DESCRIPTION=\"Date\"       POSITION=\"3\" TYPE=\"Char\"/>"
			+ "				<FIELD NAME=\"Dept-No\"    DESCRIPTION=\"Dept-No\"    POSITION=\"4\" TYPE=\"Char\"/>"
			+ "				<FIELD NAME=\"Qty-Sold\"   DESCRIPTION=\"Qty-Sold\"   POSITION=\"5\" TYPE=\"Char\"/>"
			+ "				<FIELD NAME=\"Sale-Price\" DESCRIPTION=\"Sale-Price\" POSITION=\"6\" TYPE=\"Char\"/>"
			+ "			</FIELDS>"
			+ "		</RECORD>"
			+ "	</RECORDS>"
			+ "</RECORD>";
		
		ByteArrayInputStream bs = new ByteArrayInputStream(c.getBytes());
		
		RecordEditorXmlLoader loader = new RecordEditorXmlLoader();
       
		return loader.loadCopyBook(bs, "Csv Layout").asLayoutDetail();
	}
}
