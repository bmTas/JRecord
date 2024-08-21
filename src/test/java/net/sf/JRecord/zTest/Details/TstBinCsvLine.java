/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */

package net.sf.JRecord.zTest.Details;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.RecordEditorXmlLoader;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.zTest.Common.JUnit3Test;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 * This class Tests assigning values to a Binary CSV line
 * 
 * @author bm
 *
 */
public class TstBinCsvLine extends JUnit3Test  {
	
	//private static final String TMP_DIRECTORY = TstConstants.TEMP_DIRECTORY;
	
	private String layoutFile = TstConstants.RE_XML_DIRECTORY + "zzBinCsv.Xml";
	private LayoutDetail layout = null;
	
	
	/**
	 * Test Assignments (forward)
	 * 
	 * @throws Exception any error that occurs
	 */
	@SuppressWarnings("deprecation")
	@Test public void testAssign1() throws Exception {
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
					assertEqualsV3o("Comparing a1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = j+1; k < 5; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEqualsV3o("Comparing a2 " + i + "' " + j +",  " + k ,"", t);
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
	@SuppressWarnings("deprecation")
	@Test public void testAssign2() throws Exception {
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
					assertEqualsV3o("Comparing b1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = 0; k < j; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEqualsV3o("Comparing b2 " + i + "' " + j +",  " + k ,"", t);
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
	@Test public void testAssign3() throws Exception {
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
					assertEqualsV3o("Comparing c1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = j+1; k < 5; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEqualsV3o("Comparing c2 " + i + "' " + j +",  " + k , ss[k], t);
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
	@Test public void testAssign4() throws Exception {
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
					assertEqualsV3o("Comparing d1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = 0; k < j; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEqualsV3o("Comparing d2 " + i + "' " + j +",  " + k , ss[k], t);
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
	@Test public void testAssign5() throws Exception {
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
					assertEqualsV3o("Comparing e1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = j+1; k < 5; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEqualsV3o("Comparing e2 " + i + "' " + j +",  " + k , ss[k], t);
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
				assertEqualsV3o("Comparing e3 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
			}
			
			for (k = j+1; k < 5; k++) {
				o = line.getField(0, k);
				t = "";
				if (o != null) {
					t = o.toString();
				}
				assertEqualsV3o("Comparing e4 " + i + "' " + j +",  " + k , ss[k], t);
			}
		}
	}

	/**
	 * Test Assignments (forward)
	 * 
	 * @throws Exception any error that occurs
	 */
	@Test public void testAssign6() throws Exception {
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
					assertEqualsV3o("Comparing f1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = 0; k < j; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEqualsV3o("Comparing f2 " + i + "' " + j +",  " + k , ss[k], t);
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
				assertEqualsV3o("Comparing f3 " + i + "' " + j +", " + k , ss[k], t);
			}
			
			for (k = 0; k < j; k++) {
				o = line.getField(0, k);
				t = "";
				if (o != null) {
					t = o.toString();
				}
				//System.out.println("~~> " + i + " " + j + " " + k + " >" + ss[k] + "< ~~ >" + t + "<");
				assertEqualsV3o("Comparing f4 " + "' " + j +",  " + k , ss[k], t);
			}
		}

	}
	
	/**
	 * Test Assignments (forward)
	 * 
	 * @throws Exception any error that occurs
	 */
	@Test public void testAssign7() throws Exception {
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
					assertEqualsV3o("Comparing g1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = j+1; k < 5; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEqualsV3o("Comparing g2 " + i + "' " + j +",  " + k , ss[k], t);
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
				assertEqualsV3o("Comparing g3 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
			}
			
			for (k = j+1; k < 5; k++) {
				o = line.getField(0, k);
				t = "";
				if (o != null) {
					t = o.toString();
				}
				assertEqualsV3o("Comparing e2 " + i + "' " + j +",  " + k , ss[k], t);
			}
		}
	}

	/**
	 * Test Assignments (forward)
	 * 
	 * @throws Exception any error that occurs
	 */
	@Test public void testAssign8() throws Exception {
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
					assertEqualsV3o("Comparing h1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = 0; k < j; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEqualsV3o("Comparing h2 " + i + "' " + j +",  " + k , ss[k], t);
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
				assertEqualsV3o("Comparing h3 " + i + "' " + j +", " + k , ss[k], t);
			}
			
			for (k = 0; k < j; k++) {
				o = line.getField(0, k);
				t = "";
				if (o != null) {
					t = o.toString();
				}
				//System.out.println("~~> " + i + " " + j + " " + k + " >" + ss[k] + "< ~~ >" + t + "<");
				assertEqualsV3o("Comparing h4 " + "' " + j +",  " + k , ss[k], t);
			}
		}

	}
	
	/**
	 * Test Assignments (forward)
	 * 
	 * @throws Exception any error that occurs
	 */
	@Test public void testAssign9() throws Exception {
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
					assertEqualsV3o("Comparing i1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = j+1; k < 5; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEqualsV3o("Comparing i2 " + i + "' " + j +",  " + k , ss[k], t);
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
				assertEqualsV3o("Comparing i3 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
			}
			
			for (k = j+1; k < 5; k++) {
				o = line.getField(0, k);
				t = "";
				if (o != null) {
					t = o.toString();
				}
				assertEqualsV3o("Comparing i2 " + i + "' " + j +",  " + k , ss[k], t);
			}
		}
	}

	/**
	 * Test Assignments (forward)
	 * 
	 * @throws Exception any error that occurs
	 */
	@Test public void testAssignA() throws Exception {
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
					assertEqualsV3o("Comparing j1 " + i + "' " + j +", " + k , ss[k], line.getField(0, k).toString());
				}
				
				for (k = 0; k < j; k++) {
					o = line.getField(0, k);
					t = "";
					if (o != null) {
						t = o.toString();
					}
					assertEqualsV3o("Comparing j2 " + i + "' " + j +",  " + k , ss[k], t);
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
				assertEqualsV3o("Comparing k3 " + i + "' " + j +", " + k , ss[k], t);
			}
			
			for (k = 0; k < j; k++) {
				o = line.getField(0, k);
				t = "";
				if (o != null) {
					t = o.toString();
				}
				//System.out.println("~~> " + i + " " + j + " " + k + " >" + ss[k] + "< ~~ >" + t + "<");
				assertEqualsV3o("Comparing k4 " + "' " + j +",  " + k , ss[k], t);
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
