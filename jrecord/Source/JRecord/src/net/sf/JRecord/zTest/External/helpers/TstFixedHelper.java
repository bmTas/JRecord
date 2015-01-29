package net.sf.JRecord.zTest.External.helpers;

import junit.framework.TestCase;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.Types.Type;

/**
 * Fairly pointless checking of  Fixed Width Helper methods
 * 
 * @author Bruce Martin
 *
 */

public class TstFixedHelper  extends TestCase {

	public void testCreateRecord() {
		String[] fonts = {"", "IBM037"};
		int[] fileStructures = {Constants.IO_DEFAULT, Constants.IO_BIN_TEXT, Constants.IO_TEXT_LINE, Constants.IO_FIXED_LENGTH};
		int i = 1;
		
		for (String f : fonts) {
			for (int fs : fileStructures) {
				ExternalRecord r 
					= 	ExternalRecord
							.newFixedWidthRecord("My_Record_" + i, fs, f)
						.asExternalRecord();
				assertEquals("My_Record_" + i, r.getRecordName());
				assertEquals(fs, r.getFileStructure());
				assertEquals(f, r.getFontName());
				assertEquals(Constants.rtRecordLayout, r.getRecordType());
				assertEquals(0, r.getRecordStyle());
				
				i += 1;
			}
		}
	}
	
	public void testAddFieldByLength() {
		int[] types = {Type.ftChar, Type.ftNumAnyDecimal, Type.ftNumLeftJustified};
		int[] maxDecimal = {0, 0, 2};
		int i = 0;
		
		for (int t: types) {
			for (int decimal = 0; decimal < maxDecimal[i]; decimal++) {
				ExternalRecord r
					= ExternalRecord
						.newFixedWidthRecord("My_Record" , Constants.IO_FIXED_LENGTH, "")
						.asExternalRecord();
				for (int l = 3; l < 9; l++) {
					r.addFieldByLength("fld_" + l, t, l, decimal);
				}
				checkFlds(r, decimal, t);
			}
			i += 1;
		}
	}
	
	
	public void testAddFieldByPos() {
		int[] types = {Type.ftChar, Type.ftNumAnyDecimal, Type.ftNumLeftJustified};
		int p = 1;
		int[] maxDecimal = {0, 0, 2};
		int i = 0;
		
		for (int t: types) {
			for (int decimal = 0; decimal < maxDecimal[i]; decimal++) {
				ExternalRecord r 
					= ExternalRecord
						.newFixedWidthRecord("My_Record" , Constants.IO_FIXED_LENGTH, "")
						.asExternalRecord();
				
				p = 1;
				for (int l = 3; l < 8; l++) {
					r.addFieldByPosition("fld_" + l, t, p, decimal);
					p += l;
				}
				r.addFieldByPosition("fld_8", t, p, 8, decimal);
				
				checkFlds(r, decimal, t);
			}
			i += 1;
		}
	}
	
	
	public void testAddFieldByPosLen() {
		int[] types = {Type.ftChar, Type.ftNumAnyDecimal, Type.ftNumLeftJustified};
		int p = 1;
		int[] maxDecimal = {0, 0, 2};
		int i = 0;
		
		for (int t: types) {
			for (int decimal = 0; decimal < maxDecimal[i]; decimal++) {
				ExternalRecord r 
					= ExternalRecord
						.newFixedWidthRecord("My_Record" , Constants.IO_FIXED_LENGTH, "")
						.asExternalRecord();
				
				p = 1;
				for (int l = 3; l < 9; l++) {
					r.addField("fld_" + l, t, p, l, decimal);
					p += l;
				}
				
				checkFlds(r, decimal, t);
			}
			i += 1;
		}
	}


	private void checkFlds(ExternalRecord r, int decimal, int t) {
		int p = 1;
		for (int l = 3; l < 9; l++) {
			ExternalField fld = r.getRecordField(l-3);
			assertEquals("fld_" + l, fld.getName());
			assertEquals(p, fld.getPos());
			assertEquals(l, fld.getLen());
			assertEquals(t, fld.getType());
			assertEquals(decimal, fld.getDecimal());
			
			p+= l;
		}

	}
}
