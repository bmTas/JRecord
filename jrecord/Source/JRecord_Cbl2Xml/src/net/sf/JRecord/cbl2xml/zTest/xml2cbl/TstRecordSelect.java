package net.sf.JRecord.cbl2xml.zTest.xml2cbl;

import static org.junit.Assert.*;
import net.sf.JRecord.cbl2xml.impl.RecordSelect;

import org.junit.Test;

/**
 * Test the Record Parent (parameter Class
 * 
 * @author Bruce Martin
 *
 */
public class TstRecordSelect {

	@Test
	public void testValid() {
		check(new RecordSelect("aa bb cc"));
		check(new RecordSelect(" aa bb cc "));
		check(new RecordSelect(" aa  bb  cc "));
		check(new RecordSelect("   aa    bb    cc  "));
		
		check(new RecordSelect("aa bb=cc"));
		check(new RecordSelect(" aa bb=cc "));
		check(new RecordSelect("  aa bb =cc  "));
		check(new RecordSelect("  aa bb= cc  "));
		check(new RecordSelect("  aa bb = cc  "));
		check(new RecordSelect("  aa bb  = cc  "));
		check(new RecordSelect("  aa bb=  cc  "));
		check(new RecordSelect("  aa bb  =  cc  "));
	}
	@Test
	public void testInValid() {
		assertFalse(new RecordSelect(" ").ok());
		//assertFalse(new RecordSelect("aa b c").ok());
		assertFalse(new RecordSelect("aa").ok());
		assertFalse(new RecordSelect(" aa ").ok());
		assertFalse(new RecordSelect(" aa").ok());
		assertFalse(new RecordSelect("aa ").ok());
		
		assertFalse(new RecordSelect("aa bb").ok());
		assertFalse(new RecordSelect(" aa bb ").ok());
		assertFalse(new RecordSelect(" aa  bb   ").ok());
		assertFalse(new RecordSelect("   aa    bb      ").ok());
		assertFalse(new RecordSelect("aa bb cc=dd").ok());
		assertFalse(new RecordSelect(" aa bb cc = dd").ok());
		assertFalse(new RecordSelect(" aa  bb   cc=dd").ok());
		assertFalse(new RecordSelect("   aa    bb      cc  = dd").ok());
	}

	private void check(RecordSelect rp) {
		assertTrue(rp.ok());
		assertEquals(rp.recordName, "aa");
		assertEquals(rp.fieldName, "bb");
		assertEquals(rp.value, "cc");
	}
}
