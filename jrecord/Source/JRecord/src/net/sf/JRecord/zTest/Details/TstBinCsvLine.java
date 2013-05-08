package net.sf.JRecord.zTest.Details;

import junit.framework.TestCase;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.RecordEditorXmlLoader;
import net.sf.JRecord.Log.TextLog;

/**
 * This class Tests assigning values to a Binary CSV line
 * 
 * @author bm
 *
 */
public class TstBinCsvLine extends TestCase {
	
	//private static final String TMP_DIRECTORY = TstConstants.TEMP_DIRECTORY;
	
	private String layoutFile = "/home/bm/Work/JRecord/CopyBook/Xml/zzBinCsv.Xml";
	private LayoutDetail layout = null;
	
	
	/**
	 * Test Assignments (forward)
	 * 
	 * @throws Exception any error that occurs
	 */
	public void testAssign1() throws Exception {
		long v = 1;
		String s, t;
		Object o;
		String[] ss = new String[5];
		int i, j, k;
		AbstractLine line; 
		
		for (i = 1; i < 10; i++) {
			line = buildLine();
			for (j = 0; j <5; j++) {
				s = (v+j) + "";
				ss[j] = s;
				
				line.setField(0, j, s);
				for (k = 0; k <= j; k++) {
					//System.out.println("--> " + i + " " + j + " " + k + " " + ss[k] + " ~~ " + line.getField(0, k).toString());
					assertEquals("Comparing a1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = j+1; k < 5; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEquals("Comparing a2 " + i + "' " + j +",  " + k ,"", t);
				}
			}
			v *= 10;
		}
	}
	
	/**
	 * Test Assignments (forward)
	 * 
	 * @throws Exception any error that occurs
	 */
	public void testAssign2() throws Exception {
		long v = 1;
		String s, t;
		Object o;
		String[] ss = new String[5];
		int i, j, k;
		AbstractLine line; 
		
		for (i = 1; i < 10; i++) {
			line = buildLine();
			for (j = 4; j >= 0; j--) {
				s = (v+j) + "";
				ss[j] = s;
				
				line.setField(0, j, s);
				for (k = j; k <= 4; k++) {
					System.out.println("--> " + i + " " + j + " " + k + " " + ss[k] + " ~~ " + line.getField(0, k).toString());
					assertEquals("Comparing b1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = 0; k < j; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEquals("Comparing b2 " + i + "' " + j +",  " + k ,"", t);
				}
			}
			v *= 10;
		}
	}
	
	/**
	 * Test Assignments (forward)
	 * 
	 * @throws Exception any error that occurs
	 */
	public void testAssign3() throws Exception {
		long v = 1;
		String s, t;
		Object o;
		String[] ss = new String[5];
		int i, j, k;
		AbstractLine line; 
		
		for (i = 0; i < 5; i++) {
			ss[i] = "";
		}
		
		line = buildLine();
		for (i = 1; i < 10; i++) {
			for (j = 0; j <5; j++) {
				s = (v+j) + "";
				ss[j] = s;
				
				line.setField(0, j, s);
				for (k = 0; k <= j; k++) {
					//System.out.println("--> " + i + " " + j + " " + k + " " + ss[k] + " ~~ " + line.getField(0, k).toString());
					assertEquals("Comparing c1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = j+1; k < 5; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEquals("Comparing c2 " + i + "' " + j +",  " + k , ss[k], t);
				}
			}
			v *= 10;
		}
	}

	/**
	 * Test Assignments (forward)
	 * 
	 * @throws Exception any error that occurs
	 */
	public void testAssign4() throws Exception {
		long v = 1;
		String s, t;
		Object o;
		String[] ss = new String[5];
		int i, j, k;
		AbstractLine line; 
	
		for (i = 0; i < 5; i++) {
			ss[i] = "";
		}

		line = buildLine();
		for (i = 1; i < 10; i++) {
			for (j = 4; j >= 0; j--) {
				s = (v+j) + "";
				ss[j] = s;
				
				line.setField(0, j, s);
				for (k = j; k <= 4; k++) {
					System.out.println("--> " + i + " " + j + " " + k + " " + ss[k] + " ~~ " + line.getField(0, k).toString());
					assertEquals("Comparing d1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = 0; k < j; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEquals("Comparing d2 " + i + "' " + j +",  " + k , ss[k], t);
				}
			}
			v *= 10;
		}
	}

	/**
	 * Test Assignments (forward)
	 * 
	 * @throws Exception any error that occurs
	 */
	public void testAssign5() throws Exception {
		long v = 1000000000 * 10000;
		String s, t;
		Object o;
		String[] ss = new String[5];
		int i, j, k;
		AbstractLine line; 
		
		for (i = 0; i < 5; i++) {
			ss[i] = "";
		}
		
		line = buildLine();
		for (i = 1; i < 10; i++) {
			for (j = 0; j <5; j++) {
				s = (v+j) + "";
				ss[j] = s;
				
				line.setField(0, j, s);
				for (k = 0; k <= j; k++) {
					//System.out.println("--> " + i + " " + j + " " + k + " " + ss[k] + " ~~ " + line.getField(0, k).toString());
					assertEquals("Comparing e1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = j+1; k < 5; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEquals("Comparing e2 " + i + "' " + j +",  " + k , ss[k], t);
				}
			}
			v = v / 10;
		}

		for (j = 0; j <5; j++) {
			s = "";
			ss[j] = s;
			
			line.setField(0, j, s);
			for (k = 0; k <= j; k++) {
				//System.out.println("--> " + i + " " + j + " " + k + " " + ss[k] + " ~~ " + line.getField(0, k).toString());
				assertEquals("Comparing e3 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
			}
			
			for (k = j+1; k < 5; k++) {
				o = line.getField(0, k);
				t = "";
				if (o != null) {
					t = o.toString();
				}
				assertEquals("Comparing e4 " + i + "' " + j +",  " + k , ss[k], t);
			}
		}
	}

	/**
	 * Test Assignments (forward)
	 * 
	 * @throws Exception any error that occurs
	 */
	public void testAssign6() throws Exception {
		long v = 1000000000 * 10000;
		String s, t;
		Object o;
		String[] ss = new String[5];
		int i, j, k;
		AbstractLine line; 
	
		for (i = 0; i < 5; i++) {
			ss[i] = "";
		}

		line = buildLine();
		for (i = 1; i < 10; i++) {
			for (j = 4; j >= 0; j--) {
				s = (v+j) + "";
				ss[j] = s;
				
				line.setField(0, j, s);
				for (k = j; k <= 4; k++) {
					System.out.println("--> " + i + " " + j + " " + k + " " + ss[k] + " ~~ " + line.getField(0, k).toString());
					assertEquals("Comparing f1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = 0; k < j; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEquals("Comparing f2 " + i + "' " + j +",  " + k , ss[k], t);
				}
			}
			v = v/ 10;
		}
		
		for (j = 4; j >= 0; j--) {
			s = "";
			ss[j] = s;
			
			line.setField(0, j, s);
			for (k = j; k <= 4; k++) {
				o = line.getField(0, k);
				t = "";
				if (o != null) {
					t = o.toString();
				}
				//System.out.println("--> " + i + " " + j + " " + k + " >" + ss[k] + "< ~~ >"  + t + "<");
				assertEquals("Comparing f3 " + i + "' " + j +", " + k , ss[k], t);
			}
			
			for (k = 0; k < j; k++) {
				o = line.getField(0, k);
				t = "";
				if (o != null) {
					t = o.toString();
				}
				//System.out.println("~~> " + i + " " + j + " " + k + " >" + ss[k] + "< ~~ >" + t + "<");
				assertEquals("Comparing f4 " + "' " + j +",  " + k , ss[k], t);
			}
		}

	}
	
	/**
	 * Test Assignments (forward)
	 * 
	 * @throws Exception any error that occurs
	 */
	public void testAssign7() throws Exception {
		long v = 1000000000 * 2000000000;
		String s, t;
		Object o;
		String[] ss = new String[5];
		int i, j, k;
		AbstractLine line; 
		
		for (i = 0; i < 5; i++) {
			ss[i] = "";
		}
		
		line = buildLine();
		for (i = 1; i < 10; i++) {
			for (j = 0; j <5; j++) {
				s = (v+j) + "";
				ss[j] = s;
				
				line.setField(0, j, s);
				for (k = 0; k <= j; k++) {
					//System.out.println("--> " + i + " " + j + " " + k + " " + ss[k] + " ~~ " + line.getField(0, k).toString());
					assertEquals("Comparing g1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = j+1; k < 5; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEquals("Comparing g2 " + i + "' " + j +",  " + k , ss[k], t);
				}
			}
			v = v / 100;
		}

		for (j = 0; j <5; j++) {
			s = "";
			ss[j] = s;
			
			line.setField(0, j, s);
			for (k = 0; k <= j; k++) {
				//System.out.println("--> " + i + " " + j + " " + k + " " + ss[k] + " ~~ " + line.getField(0, k).toString());
				assertEquals("Comparing g3 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
			}
			
			for (k = j+1; k < 5; k++) {
				o = line.getField(0, k);
				t = "";
				if (o != null) {
					t = o.toString();
				}
				assertEquals("Comparing e2 " + i + "' " + j +",  " + k , ss[k], t);
			}
		}
	}

	/**
	 * Test Assignments (forward)
	 * 
	 * @throws Exception any error that occurs
	 */
	public void testAssign8() throws Exception {
		long v = 1000000000 * 2100000000;
		String s, t;
		Object o;
		String[] ss = new String[5];
		int i, j, k;
		AbstractLine line; 
	
		for (i = 0; i < 5; i++) {
			ss[i] = "";
		}

		line = buildLine();
		for (i = 1; i < 10; i++) {
			for (j = 4; j >= 0; j--) {
				s = (v+j) + "";
				ss[j] = s;
				
				line.setField(0, j, s);
				for (k = j; k <= 4; k++) {
					System.out.println("--> " + i + " " + j + " " + k + " " + ss[k] + " ~~ " + line.getField(0, k).toString());
					assertEquals("Comparing h1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = 0; k < j; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEquals("Comparing h2 " + i + "' " + j +",  " + k , ss[k], t);
				}
			}
			v = v / 100;
		}
		
		for (j = 4; j >= 0; j--) {
			s = "";
			ss[j] = s;
			
			line.setField(0, j, s);
			for (k = j; k <= 4; k++) {
				o = line.getField(0, k);
				t = "";
				if (o != null) {
					t = o.toString();
				}
				//System.out.println("--> " + i + " " + j + " " + k + " >" + ss[k] + "< ~~ >"  + t + "<");
				assertEquals("Comparing h3 " + i + "' " + j +", " + k , ss[k], t);
			}
			
			for (k = 0; k < j; k++) {
				o = line.getField(0, k);
				t = "";
				if (o != null) {
					t = o.toString();
				}
				//System.out.println("~~> " + i + " " + j + " " + k + " >" + ss[k] + "< ~~ >" + t + "<");
				assertEquals("Comparing h4 " + "' " + j +",  " + k , ss[k], t);
			}
		}

	}
	
	/**
	 * Test Assignments (forward)
	 * 
	 * @throws Exception any error that occurs
	 */
	public void testAssign9() throws Exception {
		long v = 1;
		String s, t;
		Object o;
		String[] ss = new String[5];
		int i, j, k;
		AbstractLine line; 
		
		for (i = 0; i < 5; i++) {
			ss[i] = "";
		}
		
		line = buildLine();
		for (i = 1; i < 10; i++) {
			for (j = 0; j <5; j++) {
				s = (v+j) + "";
				ss[j] = s;
				
				line.setField(0, j, s);
				for (k = 0; k <= j; k++) {
					//System.out.println("--> " + i + " " + j + " " + k + " " + ss[k] + " ~~ " + line.getField(0, k).toString());
					assertEquals("Comparing i1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = j+1; k < 5; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEquals("Comparing i2 " + i + "' " + j +",  " + k , ss[k], t);
				}
			}
			v *= 100;
		}
		
		for (j = 0; j <5; j++) {
			s = "";
			ss[j] = s;
			
			line.setField(0, j, s);
			for (k = 0; k <= j; k++) {
				//System.out.println("--> " + i + " " + j + " " + k + " " + ss[k] + " ~~ " + line.getField(0, k).toString());
				assertEquals("Comparing i3 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
			}
			
			for (k = j+1; k < 5; k++) {
				o = line.getField(0, k);
				t = "";
				if (o != null) {
					t = o.toString();
				}
				assertEquals("Comparing i2 " + i + "' " + j +",  " + k , ss[k], t);
			}
		}
	}

	/**
	 * Test Assignments (forward)
	 * 
	 * @throws Exception any error that occurs
	 */
	public void testAssignA() throws Exception {
		long v = 1;
		String s, t;
		Object o;
		String[] ss = new String[5];
		int i, j, k;
		AbstractLine line; 
	
		for (i = 0; i < 5; i++) {
			ss[i] = "";
		}

		line = buildLine();
		for (i = 1; i < 10; i++) {
			for (j = 4; j >= 0; j--) {
				s = (v+j) + "";
				ss[j] = s;
				
				line.setField(0, j, s);
				for (k = j; k <= 4; k++) {
					System.out.println("--> " + i + " " + j + " " + k + " " + ss[k] + " ~~ " + line.getField(0, k).toString());
					assertEquals("Comparing j1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = 0; k < j; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEquals("Comparing j2 " + i + "' " + j +",  " + k , ss[k], t);
				}
			}
			v *= 100;
		}
		
		
		for (j = 4; j >= 0; j--) {
			s = "";
			ss[j] = s;
			
			line.setField(0, j, s);
			for (k = j; k <= 4; k++) {
				o = line.getField(0, k);
				t = "";
				if (o != null) {
					t = o.toString();
				}
				//System.out.println("--> " + i + " " + j + " " + k + " >" + ss[k] + "< ~~ >"  + t + "<");
				assertEquals("Comparing k3 " + i + "' " + j +", " + k , ss[k], t);
			}
			
			for (k = 0; k < j; k++) {
				o = line.getField(0, k);
				t = "";
				if (o != null) {
					t = o.toString();
				}
				//System.out.println("~~> " + i + " " + j + " " + k + " >" + ss[k] + "< ~~ >" + t + "<");
				assertEquals("Comparing k4 " + "' " + j +",  " + k , ss[k], t);
			}
		}
	}


	/**
	 * Get a Line
	 * @return the requested line
	 * @throws Exception
	 */
	public AbstractLine buildLine() throws Exception {
		if (layout == null) {
			layout = (new RecordEditorXmlLoader())
							.loadCopyBook(layoutFile, 0, 0, "", 0, 0, new TextLog())
							.asLayoutDetail();
			
		}
		
		return new Line(layout);
	}
}
