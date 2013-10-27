package net.sf.JRecord.zTest.Parser;

import junit.framework.TestCase;
import net.sf.JRecord.CsvParser.CsvDefinition;
import net.sf.JRecord.CsvParser.StandardCsvLineParser;
import net.sf.JRecord.Types.Type;

public class TstStandardParser extends TestCase {

	private String[] lines0 = {"field1, field2, field3",
			 "f11,f21,f31",
			 "f12,f22,f32",
			 "f13,f23,f33",
			 "f14,f24,f34",
			 "f15,f25,f35",
			 "f16,f26,f36",
			 "f17,f26,f37",
	};

	private String[] lines1 = {"field1, field2, field3",
							 "f11,'pt1, pt2, pt3',f31",
							 "f12,'pt1, pt2, pt3''',f32",
							 "f13,'pt1, ''pt2'' , pt3',f33",
							 "f14,'pt1, ''pt2'' , ''pt3''',f34",
							 "f15,'pt1, ''pt2'', ''pt3'', pt4',f35",
							 "f16,'''f2a'','' f2b''',f36",
							 "'f17','pt1, ''pt2'' , ''pt3'', pt4','f37'",
	};
	private String[] lines2 = {"field1| field2| field3",
			 "f11|'pt1| pt2| pt3'|f31",
			 "f12|'pt1| pt2| pt3'''|f32",
			 "f13|'pt1| ''pt2'' | pt3'|f33",
			 "f14|'pt1| ''pt2'' | ''pt3'''|f34",
			 "f15|'pt1| ''pt2''| ''pt3''| pt4'|f35",
			 "f16|'''f2a''|'' f2b'''|f36",
			 "'f17'|'pt1| ''pt2'' | ''pt3''| pt4'|'f37'",
	};



	public void testGetField1() {
		StandardCsvLineParser p = new StandardCsvLineParser();
		String s;

		for (int i = 1; i < lines1.length; i++) {
			s = p.getField(0, lines1[i], new CsvDefinition("," , "'"));
			assertEquals("Error in " + i + "a, string was " + s, "f1" + i, s);
			s = p.getField(0, lines2[i], new CsvDefinition("|" , "'"));
			assertEquals("Error in " + i + "b, string was " + s, "f1" + i, s);
			s = p.getField(0, lines0[i], new CsvDefinition("," , ""));
			assertEquals("Error in " + i + "c, string was " + s, "f1" + i, s);

			s = p.getField(2, lines1[i], new CsvDefinition("," , "'"));
			assertEquals("Error in " + i + "d, string was " + s, "f3" + i, s);
			s = p.getField(2, lines2[i], new CsvDefinition("|" , "'"));
			assertEquals("Error in " + i + "e, string was " + s, "f3" + i, s);
			s = p.getField(2, lines0[i], new CsvDefinition("," , ""));
			assertEquals("Error in " + i + "f, string was " + s, "f3" + i, s);

		}
	}

	public void testGetField2() {
		StandardCsvLineParser p = new StandardCsvLineParser();
		String s;

		s = p.getField(1, lines1[1], new CsvDefinition("," , "'"));
		assertEquals("Error in 11a, string was " + s, "pt1, pt2, pt3", s);
		s = p.getField(1, lines2[1], new CsvDefinition("|" , "'"));
		assertEquals("Error in 11b, string was " + s, "pt1| pt2| pt3", s);


		s = p.getField(1, lines1[2], new CsvDefinition("," , "'"));
		assertEquals("Error in 12a, string was " + s, "pt1, pt2, pt3'", s);
		s = p.getField(1, lines2[2], new CsvDefinition("|" , "'"));
		assertEquals("Error in 12b, string was " + s, "pt1| pt2| pt3'", s);

		s = p.getField(1, lines1[3], new CsvDefinition("," , "'"));
		assertEquals("Error in 13a, string was " + s, "pt1, 'pt2' , pt3", s);
		s = p.getField(1, lines2[3], new CsvDefinition("|" , "'"));
		assertEquals("Error in 13b, string was " + s, "pt1| 'pt2' | pt3", s);

		s = p.getField(1, lines1[4], new CsvDefinition("," , "'"));
		assertEquals("Error in 14a, string was " + s, "pt1, 'pt2' , 'pt3'", s);
		s = p.getField(1, lines2[4], new CsvDefinition("|" , "'"));
		assertEquals("Error in 14b, string was " + s, "pt1| 'pt2' | 'pt3'", s);

		s = p.getField(1, lines1[5], new CsvDefinition("," , "'"));
		assertEquals("Error in 15a, string was " + s, "pt1, 'pt2', 'pt3', pt4", s);
		s = p.getField(1, lines2[5], new CsvDefinition("|" , "'"));
		assertEquals("Error in 15b, string was " + s, "pt1| 'pt2'| 'pt3'| pt4", s);

		s = p.getField(1, lines1[6], new CsvDefinition("," , "'"));
		assertEquals("Error in 16a, string was " + s, "'f2a',' f2b'", s);
		s = p.getField(1, lines2[6], new CsvDefinition("|" , "'"));
		assertEquals("Error in 16b, string was " + s, "'f2a'|' f2b'", s);

		s = p.getField(1, lines1[7], new CsvDefinition("," , "'"));
		assertEquals("Error in 17a, string was " + s, "pt1, 'pt2' , 'pt3', pt4", s);
		s = p.getField(1, lines2[7], new CsvDefinition("|" , "'"));
		assertEquals("Error in 17b, string was " + s, "pt1| 'pt2' | 'pt3'| pt4", s);

	}

	public void testSetField() {
		StandardCsvLineParser p = new StandardCsvLineParser();
		int j;
		String s;
		//StringBuffer comment = new StringBuffer("12345");

		//System.out.println("@@~~ " + comment.substring(comment.length() - 1));

		for (int i = 0; i < lines1.length - 1; i++) {
			for (j = 0; j < 3; j++) {
				s = p.setField(j, Type.NT_TEXT, lines1[i], new CsvDefinition(",", "'"),
						p.getField(j, lines1[i], new CsvDefinition(",", "'")));
				assertEquals("Error in " + i +":" + j + "a got " + s, lines1[i], s);

				s = p.setField(j, Type.NT_TEXT, lines2[i], new CsvDefinition("|", "'"),
						p.getField(j, lines2[i], new CsvDefinition("|", "'")));
				assertEquals("Error in " + i +":" + j + "b got " + s, lines2[i], s);

				s = p.setField(j, Type.NT_TEXT, lines0[i], new CsvDefinition(",", "'"),
						p.getField(j, lines0[i], new CsvDefinition(",", "")));
				assertEquals("Error in " + i +":" + j + "c got " + s, lines0[i], s);
}
		}
	}


	public void testSetField2() {
		StandardCsvLineParser p = new StandardCsvLineParser(true);
		int j;
		String s;
		//StringBuffer comment = new StringBuffer("12345");

		//System.out.println("@@~~ " + comment.substring(comment.length() - 1));

		for (int i = 0; i < lines1.length - 1; i++) {
			for (j = 0; j < 3; j++) {
				s = p.setField(j, Type.NT_NUMBER, lines1[i], new CsvDefinition(",", "'"),
						p.getField(j, lines1[i], new CsvDefinition(",", "'")));
				assertEquals("Error in " + i +":" + j + "a got " + s, lines1[i], s);

				s = p.setField(j, Type.NT_NUMBER, lines2[i], new CsvDefinition("|", "'"),
						p.getField(j, lines2[i], new CsvDefinition("|", "'")));
				assertEquals("Error in " + i +":" + j + "b got " + s, lines2[i], s);

				s = p.setField(j, Type.NT_NUMBER, lines0[i], new CsvDefinition(",", "'"),
						p.getField(j, lines0[i], new CsvDefinition(",", "")));
				assertEquals("Error in " + i +":" + j + "c got " + s, lines0[i], s);
			}
		}
	}


	public void testSetField3() {
			StandardCsvLineParser p = new StandardCsvLineParser(true);
			int j;
			String s, before, after;
			//StringBuffer comment = new StringBuffer("12345");

			//System.out.println("@@~~ " + comment.substring(comment.length() - 1));

			for (int i = 0; i < lines1.length - 1; i++) {
				for (j = 0; j < 3; j++) {
					before = p.getField(j, lines1[i], new CsvDefinition(",", "'"));
					s = p.setField(j, Type.NT_TEXT, lines1[i], new CsvDefinition(",", "'"), before);

					if (before.indexOf(",") == 0) {
						after = p.getField(j, s, new CsvDefinition(",", ""));
						System.out.println(i + ", " + j + " > " + before + " => " + after + " <");

						assertEquals("Error in " + i +":" + j + "aa got " + after, after, "'" + before + "'");
					}
					after = p.getField(j, s, new CsvDefinition(",", "'"));
					assertEquals("Error in " + i +":" + j + "ab got " + after, after, before);

					before = p.getField(j, lines2[i], new CsvDefinition("|", "'"));
					s = p.setField(j, Type.NT_TEXT, lines2[i], new CsvDefinition("|", "'"), before);

					if (before.indexOf("|") == 0) {
						after = p.getField(j, s, new CsvDefinition("|", ""));
						System.out.println(i + ", " + j + " > " + before + " => " + after);

						assertEquals("Error in " + i +":" + j + "ba got " + after, after, "'" + before + "'");
					}
					after = p.getField(j, s, new CsvDefinition("|", "'"));
					System.out.println(i + ", " + j + " > " + before + " => " + after);
					assertEquals("Error in " + i +":" + j + "bb got " + after, after, before);

					before = p.getField(j, lines0[i], new CsvDefinition(",", ""));
					s = p.setField(j, Type.NT_TEXT, lines0[i], new CsvDefinition(",", "'"), before);
					after = p.getField(j, s,  new CsvDefinition(",", ""));
					System.out.println(i + ", " + j + " > " + before + " => " + after);

					assertEquals("Error in " + i +":" + j + "ca got " + after, after, "'" + before + "'");
					after = p.getField(j, s, new CsvDefinition(",", "'"));
					assertEquals("Error in " + i +":" + j + "cb got " + after, after, before);
					System.out.println();

				}
			}
	}

//	public void testSplit() {
//		fail("Not yet implemented");
//	}

}
