package net.sf.JRecord.zTest.schema;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.schema.ArrayElementChecks;
import net.sf.JRecord.schema.GroupUpdateDetails;
import net.sf.JRecord.schema.IArrayItemCheck;
import net.sf.JRecord.schema.jaxb.IItem;
import net.sf.JRecord.schema.jaxb.impl.AddPlusToNumeric;
import net.sf.JRecord.schema.jaxb.impl.DoNothingFormat;
import net.sf.JRecord.schema.jaxb.impl.ZeroPad;
import net.sf.JRecord.schema.jaxb.interfaces.IFormatField;
import net.sf.JRecord.schema.jaxb.interfaces.IRedefineSelection;
import net.sf.JRecord.schema.jaxb.interfaces.IWriteCheck;

public class TstGroupUpdateDtls01 {

	private static final String[] FIELD_NAMES = { "Field01", "Field02", "Field03", "Field04", "Field05", };
	private static final ZeroPad ZERO_PAD = new ZeroPad();
	private static final DoNothingFormat DO_NOTHING = new DoNothingFormat();
	private static final AddPlusToNumeric ADD_PLUS = new AddPlusToNumeric();
	private static final IFormatField[] FORMATS = {ZERO_PAD, DO_NOTHING, ADD_PLUS };
	
	private static final IArrayItemCheck[] ARRAY_CHECKS = {
			ArrayElementChecks.INSTANCE.newSkipHighValues(), ArrayElementChecks.INSTANCE.newSkipLowValues(),
			ArrayElementChecks.INSTANCE.newSkipSpaces(), ArrayElementChecks.INSTANCE.newSkipSpacesZeros(),
			ArrayElementChecks.INSTANCE.newStopAtHighValues()
	};
	
	@Test
	public void testBasics() {
		setFormatField(new GroupUpdateDetails());
		setArrayCheck(new GroupUpdateDetails());
		setRedefCheck(new GroupUpdateDetails());
		setWriteCheck(new GroupUpdateDetails());
	}
	
	@Test
	public void testUpdates() {
		GroupUpdateDetails upd = new GroupUpdateDetails();
		setArrayCheck(upd);
		updateAll(upd);
	}
	
	
	private void setFormatField(GroupUpdateDetails upd) {
		
		int j = 0;
		for (String f : FIELD_NAMES) {
			upd.setFormatField(f, FORMATS[(j++) % FORMATS.length]);
		}
		
		j = 0;
		for (String f : FIELD_NAMES) {
			GroupUpdateDetails.UpdateDetails updateDetails = upd.getUpdateDetails(Arrays.asList(f));
			assertTrue(f, FORMATS[(j++) % FORMATS.length] == updateDetails.getFormatField());
			assertTrue(f, null == updateDetails.getArrayCheck());
			assertTrue(f, null == updateDetails.getRedefineSelection());
			assertTrue(f, null == updateDetails.getWriteCheck());			
		}

	}
	
	private void updateAll(GroupUpdateDetails upd) {
		
		for (int i = 0; i < FORMATS.length; i++) {
			upd.setFormatField(FIELD_NAMES[i+1], FORMATS[i]);	
		}
		int j = 0;
		for (String f : FIELD_NAMES) {
			GroupUpdateDetails.UpdateDetails updateDetails = upd.getUpdateDetails(Arrays.asList(f));
			
			IFormatField format = j == 0 || j > FORMATS.length ? null : FORMATS[j-1] ;
			assertTrue(f, ARRAY_CHECKS[(j++)] == updateDetails.getArrayCheck());
			
			assertTrue(f , format == updateDetails.getFormatField());
			assertTrue(f, null == updateDetails.getRedefineSelection());
			assertTrue(f, null == updateDetails.getWriteCheck());			
		}
		
		ArrayList<IRedefineSelection> redefs = new ArrayList<>(FIELD_NAMES.length);
		j = 0;
		for (String f : FIELD_NAMES) {
			redefs.add(new RedefTst());
			upd.setRedefineSelection(f, redefs.get(j++));
		}

		j = 0;
		for (String f : FIELD_NAMES) {
			GroupUpdateDetails.UpdateDetails updateDetails = upd.getUpdateDetails(Arrays.asList(f));
			
			IFormatField format = j == 0 || j > FORMATS.length ? null : FORMATS[j-1] ;
			assertTrue(f, ARRAY_CHECKS[(j)] == updateDetails.getArrayCheck());
			
			assertTrue(f , format == updateDetails.getFormatField());
			assertTrue(f, redefs.get(j) == updateDetails.getRedefineSelection());
			assertTrue(f, null == updateDetails.getWriteCheck());	
			j += 1;
		}
		
		ArrayList<IWriteCheck> writeChecks = new ArrayList<>(FIELD_NAMES.length);
		j = 0;
		for (String f : FIELD_NAMES) {
			writeChecks.add(new WriteChekTst());
			upd.setWriteCheck(f, writeChecks.get(j++));
		}

		j = 0;
		for (String f : FIELD_NAMES) {
			GroupUpdateDetails.UpdateDetails updateDetails = upd.getUpdateDetails(Arrays.asList(f));
			
			IFormatField format = j == 0 || j > FORMATS.length ? null : FORMATS[j-1] ;
			assertTrue(f, ARRAY_CHECKS[(j)] == updateDetails.getArrayCheck());
			
			assertTrue(f , format == updateDetails.getFormatField());
			assertTrue(f, redefs.get(j) == updateDetails.getRedefineSelection());
			assertTrue(f, writeChecks.get(j) == updateDetails.getWriteCheck());	
			j += 1;
		}
		
		int idx = 2;
		for (String f : FIELD_NAMES) {
			upd.setArrayCheck(f, ARRAY_CHECKS[(idx++) ]);
			if (idx >= ARRAY_CHECKS.length) {
				idx = 0;
			}
		}

		j = 0;
		idx = 2;
		for (String f : FIELD_NAMES) {
			GroupUpdateDetails.UpdateDetails updateDetails = upd.getUpdateDetails(Arrays.asList(f));
			
			IFormatField format = j == 0 || j > FORMATS.length ? null : FORMATS[j-1] ;
			assertTrue(f, ARRAY_CHECKS[(idx++)] == updateDetails.getArrayCheck());
			
			assertTrue(f , format == updateDetails.getFormatField());
			assertTrue(f, redefs.get(j) == updateDetails.getRedefineSelection());
			assertTrue(f, writeChecks.get(j) == updateDetails.getWriteCheck());	
			j += 1;
			if (idx >= ARRAY_CHECKS.length) {
				idx = 0;
			}
		}

	}


	
	private void setArrayCheck(GroupUpdateDetails upd) {
		
		int j = 0;
		for (String f : FIELD_NAMES) {
			upd.setArrayCheck(f, ARRAY_CHECKS[(j++) ]);
		}
		
		j = 0;
		for (String f : FIELD_NAMES) {
			GroupUpdateDetails.UpdateDetails updateDetails = upd.getUpdateDetails(Arrays.asList(f));
			assertTrue(f, ARRAY_CHECKS[(j++)] == updateDetails.getArrayCheck());
			assertTrue(f, null == updateDetails.getFormatField());
			assertTrue(f, null == updateDetails.getRedefineSelection());
			assertTrue(f, null == updateDetails.getWriteCheck());			
		}
	}
	
	
	private void setRedefCheck(GroupUpdateDetails upd) {
		
		ArrayList<IRedefineSelection> redefs = new ArrayList<>(FIELD_NAMES.length);
		int j = 0;
		for (String f : FIELD_NAMES) {
			redefs.add(new RedefTst());
			upd.setRedefineSelection(f, redefs.get(j++));
		}
		
		j = 0;
		for (String f : FIELD_NAMES) {
			GroupUpdateDetails.UpdateDetails updateDetails = upd.getUpdateDetails(Arrays.asList(f));
			assertTrue(f, null == updateDetails.getArrayCheck());
			assertTrue(f, null == updateDetails.getFormatField());
			assertTrue(f, redefs.get(j++) == updateDetails.getRedefineSelection());
			assertTrue(f, null == updateDetails.getWriteCheck());			
		}
	}	
	private void setWriteCheck(GroupUpdateDetails upd) {
		
		ArrayList<IWriteCheck> writeChecks = new ArrayList<>(FIELD_NAMES.length);
		int j = 0;
		for (String f : FIELD_NAMES) {
			writeChecks.add(new WriteChekTst());
			upd.setWriteCheck(f, writeChecks.get(j++));
		}
		
		j = 0;
		for (String f : FIELD_NAMES) {
			GroupUpdateDetails.UpdateDetails updateDetails = upd.getUpdateDetails(Arrays.asList(f));
			assertTrue(f, null == updateDetails.getArrayCheck());
			assertTrue(f, null == updateDetails.getFormatField());
			assertTrue(f, null == updateDetails.getRedefineSelection());
			assertTrue(f, writeChecks.get(j++) == updateDetails.getWriteCheck());			
		}
	}


	
	private static class RedefTst implements IRedefineSelection {

		@Override public List<IItem> selectRedefinedItemToWrite(List<IItem> redefinedItemGroup, AbstractLine line) {
			return  redefinedItemGroup;
		}
		
	}
	
	private static  class WriteChekTst implements IWriteCheck {

		@Override public boolean isOkToWrite(IItem item, AbstractLine line) {
			return false;
		}
		
		
		
	}
}
