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

public class TstGroupUpdateDtls02 {

	private static final List<List<String>> COBOL_NAMES = new ArrayList<List<String>>();
	private static final List<List<String>> COBOL_NAMES2 = new ArrayList<List<String>>();
	private static final List<List<String>> COBOL_NAMES3 = new ArrayList<List<String>>();
	static { 
		COBOL_NAMES.add(Arrays.asList("g11", "g31", "g51", "Field01"));
		COBOL_NAMES.add(Arrays.asList("g11", "g31", "g52", "Field01"));
		COBOL_NAMES.add(Arrays.asList("g13", "g31", "g53", "Field03"));
		COBOL_NAMES.add(Arrays.asList("g13", "g31", "g53", "Field01"));
		COBOL_NAMES.add(Arrays.asList("g15", "g35", "g55", "Field05"));
		
		COBOL_NAMES2.add(Arrays.asList("g11", "g21", "g31", "g51", "Field01"));
		COBOL_NAMES2.add(Arrays.asList("g11", "g22", "g22a", "g31", "g41", "g52", "Field01"));
		COBOL_NAMES2.add(Arrays.asList("g13", "g22", "g22a", "g22b", "g31", "g53", "Field03"));
		COBOL_NAMES2.add(Arrays.asList("g13", "g41", "g31", "g44", "g44a", "g44b", "g53", "g64", "Field01"));
		COBOL_NAMES2.add(Arrays.asList("g15", "g35", "g55", "g56", "Field05"));
		
		COBOL_NAMES3.add(Arrays.asList("g11", "g31", "g51", "g51aa", "g51bb", "g51cc", "Field01"));
		COBOL_NAMES3.add(Arrays.asList("g11", "g31", "g52", "g52aa", "g52bb", "Field01"));
		COBOL_NAMES3.add(Arrays.asList("g13", "g31", "g53","g53aa", "g53bb", "g53cc", "g53dd",  "Field03"));
		COBOL_NAMES3.add(Arrays.asList("g13", "g24", "g31", "g53", "g53aa", "g53bb", "g53cc", "g53dd", "Field01"));
		COBOL_NAMES3.add(Arrays.asList("g15", "g35", "g45", "g55", "g55aa", "g55bb","Field05"));		
	};
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
		List<List<String>> cobolNames = COBOL_NAMES;
		for (List<String> f : cobolNames) {
			upd.setFormatField(f, FORMATS[(j++) % FORMATS.length]);
		}
		
		checkFormatField(upd, cobolNames);
		checkFormatField(upd, COBOL_NAMES2);
		checkFormatField(upd, COBOL_NAMES3);

	}

	private void checkFormatField(GroupUpdateDetails upd, List<List<String>> cobolNames) {
		int idx = 0;
		for (List<String> f : cobolNames) {
			GroupUpdateDetails.UpdateDetails updateDetails = upd.getUpdateDetails(f);
			assertTrue(toNamesString(f), FORMATS[(idx++) % FORMATS.length] == updateDetails.getFormatField());
			assertTrue(toNamesString(f), null == updateDetails.getArrayCheck());
			assertTrue(toNamesString(f), null == updateDetails.getRedefineSelection());
			assertTrue(toNamesString(f), null == updateDetails.getWriteCheck());			
		}
	}
	
	private void updateAll(GroupUpdateDetails upd) {
		
		for (int i = 0; i < FORMATS.length; i++) {
			upd.setFormatField(COBOL_NAMES.get(i+1), FORMATS[i]);	
		}
		
	
		checkUpdateFieldFormats(upd, COBOL_NAMES);
		checkUpdateFieldFormats(upd, COBOL_NAMES2);
		checkUpdateFieldFormats(upd, COBOL_NAMES3);
		
		ArrayList<IRedefineSelection> redefs = new ArrayList<>();
		int idx = 0;
		for (List<String> f : COBOL_NAMES) {
			redefs.add(new RedefTst());
			upd.setRedefineSelection(f, redefs.get(idx++));
		}


		checkRedefUpdate(upd, COBOL_NAMES, redefs);
		checkRedefUpdate(upd, COBOL_NAMES2, redefs);
		checkRedefUpdate(upd, COBOL_NAMES3, redefs);
		
		ArrayList<IWriteCheck> writeChecks = new ArrayList<>();
		idx = 0;
		for (List<String> f : COBOL_NAMES) {
			writeChecks.add(new WriteChekTst());
			upd.setWriteCheck(f, writeChecks.get(idx++));
		}

		int j;
		checkWriteCheckUpdates(upd, COBOL_NAMES, redefs, writeChecks);
		checkWriteCheckUpdates(upd, COBOL_NAMES2, redefs, writeChecks);
		checkWriteCheckUpdates(upd, COBOL_NAMES3, redefs, writeChecks);
		
		idx = 2;
		for (List<String> f : COBOL_NAMES) {
			upd.setArrayCheck(f, ARRAY_CHECKS[(idx++) ]);
			if (idx >= ARRAY_CHECKS.length) {
				idx = 0;
			}
		}

		j = 0;
		idx = 2;
		for (List<String> f : COBOL_NAMES) {
			GroupUpdateDetails.UpdateDetails updateDetails = upd.getUpdateDetails(f);
			
			IFormatField format = j == 0 || j > FORMATS.length ? null : FORMATS[j-1] ;
			assertTrue(toNamesString(f), ARRAY_CHECKS[(idx++)] == updateDetails.getArrayCheck());
			
			assertTrue(toNamesString(f) , format == updateDetails.getFormatField());
			assertTrue(toNamesString(f), redefs.get(j) == updateDetails.getRedefineSelection());
			assertTrue(toNamesString(f), writeChecks.get(j) == updateDetails.getWriteCheck());	
			j += 1;
			if (idx >= ARRAY_CHECKS.length) {
				idx = 0;
			}
		}

	}

	private void checkWriteCheckUpdates(GroupUpdateDetails upd, List<List<String>> cobolNames,
			ArrayList<IRedefineSelection> redefs, ArrayList<IWriteCheck> writeChecks) {
		int j = 0;
		for (List<String> f : cobolNames) {
			GroupUpdateDetails.UpdateDetails updateDetails = upd.getUpdateDetails(f);
			
			IFormatField format = j == 0 || j > FORMATS.length ? null : FORMATS[j-1] ;
			assertTrue(toNamesString(f), ARRAY_CHECKS[(j)] == updateDetails.getArrayCheck());
			
			assertTrue(toNamesString(f) , format == updateDetails.getFormatField());
			assertTrue(toNamesString(f), redefs.get(j) == updateDetails.getRedefineSelection());
			assertTrue(toNamesString(f), writeChecks.get(j) == updateDetails.getWriteCheck());	
			j += 1;
		}
	}

	private void checkRedefUpdate(GroupUpdateDetails upd, List<List<String>> cobolNames,
			ArrayList<IRedefineSelection> redefs) {
		int j = 0;
		for (List<String> f : cobolNames) {
			GroupUpdateDetails.UpdateDetails updateDetails = upd.getUpdateDetails(f);
			
			IFormatField format = j == 0 || j > FORMATS.length ? null : FORMATS[j-1] ;
			assertTrue(toNamesString(f), ARRAY_CHECKS[(j)] == updateDetails.getArrayCheck());
			
			assertTrue(toNamesString(f) , format == updateDetails.getFormatField());
			assertTrue(toNamesString(f), redefs.get(j) == updateDetails.getRedefineSelection());
			assertTrue(toNamesString(f), null == updateDetails.getWriteCheck());	
			j += 1;
		}
	}

	private void checkUpdateFieldFormats(GroupUpdateDetails upd, List<List<String>> cobolNames) {
		int j = 0;
		for (List<String> f : cobolNames) {
			GroupUpdateDetails.UpdateDetails updateDetails = upd.getUpdateDetails(f);
			
			IFormatField format = j == 0 || j > FORMATS.length ? null : FORMATS[j-1] ;
			assertTrue(toNamesString(f), ARRAY_CHECKS[(j++)] == updateDetails.getArrayCheck());
			
			assertTrue(toNamesString(f) , format == updateDetails.getFormatField());
			assertTrue(toNamesString(f), null == updateDetails.getRedefineSelection());
			assertTrue(toNamesString(f), null == updateDetails.getWriteCheck());			
		}
	}


	
	private void setArrayCheck(GroupUpdateDetails upd) {
		
		int j = 0;
		for (List<String> f : COBOL_NAMES) {
			upd.setArrayCheck(f, ARRAY_CHECKS[(j++) ]);
		}
		
		j = 0;
		for (List<String> f : COBOL_NAMES) {
			GroupUpdateDetails.UpdateDetails updateDetails = upd.getUpdateDetails(f);
			assertTrue(toNamesString(f), ARRAY_CHECKS[(j++)] == updateDetails.getArrayCheck());
			assertTrue(toNamesString(f), null == updateDetails.getFormatField());
			assertTrue(toNamesString(f), null == updateDetails.getRedefineSelection());
			assertTrue(toNamesString(f), null == updateDetails.getWriteCheck());			
		}
	}
	
	
	private void setRedefCheck(GroupUpdateDetails upd) {
		
		ArrayList<IRedefineSelection> redefs = new ArrayList<>();
		int j = 0;
		for (List<String> f : COBOL_NAMES) {
			redefs.add(new RedefTst());
			upd.setRedefineSelection(f, redefs.get(j++));
		}
		
		j = 0;
		for (List<String> f : COBOL_NAMES) {
			GroupUpdateDetails.UpdateDetails updateDetails = upd.getUpdateDetails(f);
			assertTrue(toNamesString(f), null == updateDetails.getArrayCheck());
			assertTrue(toNamesString(f), null == updateDetails.getFormatField());
			assertTrue(toNamesString(f), redefs.get(j++) == updateDetails.getRedefineSelection());
			assertTrue(toNamesString(f), null == updateDetails.getWriteCheck());			
		}
	}	
	private void setWriteCheck(GroupUpdateDetails upd) {
		
		ArrayList<IWriteCheck> writeChecks = new ArrayList<>();
		int j = 0;
		for (List<String> f : COBOL_NAMES) {
			writeChecks.add(new WriteChekTst());
			upd.setWriteCheck(f, writeChecks.get(j++));
		}
		
		j = 0;
		for (List<String> f : COBOL_NAMES) {
			GroupUpdateDetails.UpdateDetails updateDetails = upd.getUpdateDetails(f);
			assertTrue(toNamesString(f), null == updateDetails.getArrayCheck());
			assertTrue(toNamesString(f), null == updateDetails.getFormatField());
			assertTrue(toNamesString(f), null == updateDetails.getRedefineSelection());
			assertTrue(toNamesString(f), writeChecks.get(j++) == updateDetails.getWriteCheck());			
		}
	}

	static String toNamesString(List<String> names) {
		StringBuilder b = new StringBuilder('.');
		for (String name : names) {
			b.append(name).append('.');
		}
		return b.toString().toLowerCase();
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
