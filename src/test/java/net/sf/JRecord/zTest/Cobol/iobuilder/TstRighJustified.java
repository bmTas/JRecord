package net.sf.JRecord.zTest.Cobol.iobuilder;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;

import org.junit.jupiter.api.Test;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 * Testing values are assigned correctly to Right justified fields
 * 
 * @author Bruce Martin
 *
 */
public class TstRighJustified  {
    private static final String RIGHTJUST_COPYBOOK = TstConstants.COBOL_DIRECTORY + "RightJust.cbl";

    @Test public void testRightJustified() throws IOException {
    	String str = "abcdefghij";
    	ICobolIOBuilder iob = JRecordInterface1.COBOL.newIOBuilder(RIGHTJUST_COPYBOOK);
    	AbstractLine line = iob.newLine();
    	
    	for (int i = 0; i <= str.length(); i++) {
    		String value = str.substring(i);
    		line.getFieldValue(0, 0).set(value);
    		line.getFieldValue(0, 1).set(value);
    		line.getFieldValue(0, 2).set(value);
    		String lineTxt = line.getFullLine();
    		checkRighJust( lineTxt.substring(0, 20), value );
    		checkRighJust( lineTxt.substring(20, 40), value);
    		assertEquals(value, lineTxt.substring(40).trim());
    		
    		checkRighJust( line.getFieldValue(0, 0).asString(), value );
    		checkRighJust( line.getFieldValue(0, 1).asString(), value );
       	 	assertEquals(value, line.getFieldValue(0, 2).asString());
    	}
    }
    
    private void checkRighJust(String fldValue, String value) {
    	if (value.length() > 0) {
    		assertEquals(20, fldValue.length());
    	}
    	assertEquals(value, fldValue.trim());
    	assertTrue(fldValue.endsWith(value));
    }
}
