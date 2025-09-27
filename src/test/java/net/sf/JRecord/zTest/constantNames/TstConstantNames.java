package net.sf.JRecord.zTest.constantNames;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.Test;


import java.util.List;

import net.sf.JRecord.Option.ICobolSplitOptions;
import net.sf.JRecord.constantNames.ConstantDetails;
import net.sf.JRecord.constantNames.ConstantNameConversion;
import net.sf.JRecord.constantNames.ConstantNames;

public class TstConstantNames {
	
	private static final Const[] SPLIT_OPTION_VALUES = {
			newSplitConst(ICobolSplitOptions.SPLIT_NONE, "SPLIT_NONE"),
			new Const(ICobolSplitOptions.SPLIT_01_LEVEL, "01",  "SPLIT_01_LEVEL",  "SPLIT_01_LEVEL"),
			newSplitConst(ICobolSplitOptions.SPLIT_HIGHEST_REPEATING, "SPLIT_HIGHEST_REPEATING"),
			newSplitConst(ICobolSplitOptions.SPLIT_REDEFINE, "SPLIT_REDEFINE"),
			newSplitConst(ICobolSplitOptions.SPLIT_TOP_LEVEL, "SPLIT_TOP_LEVEL"),	};
	
	
	private static Const newSplitConst(int id, String jrConstant) {
		String name = jrConstant.substring(6);
		return new Const(id, name, jrConstant, jrConstant);
	}

	@Test
	public void testSplitList() {
		ConstantNameConversion splitOptions = ConstantNames.getConstantConversions().getSplitOptions();
		List<ConstantDetails> splitOptionList = splitOptions.getConstantList();
		
//		for (ConstantDetails splitOpt : splitOptionList) {
//			System.out.println("\tnew Const(ICobolSplitOptions." + splitOpt.getSimpleName() + ", \"" + splitOpt.getSimpleName() + "\"),");
//		}
		
		checkList(splitOptionList, SPLIT_OPTION_VALUES);
	}

	
	@Test
	public void testSplitCodeLookup() {
		tstCodeLookUp(ConstantNames.getConstantConversions().getSplitOptions(), SPLIT_OPTION_VALUES);
	}

	
	@Test
	public void testSplitNameLookup() {	
		tstNameLookup(ConstantNames.getConstantConversions().getSplitOptions(), SPLIT_OPTION_VALUES);
	}
	
	@Test
	public void testConstantNameConversion() {
		Const[] data = {
				new Const(-5, "Minus 5", "minus_5"),
				new Const(-1, "Minus 1", "minus_1"),
				new Const(1, "one", "one"),
				new Const(11, "11", "eleven"),
				new Const(711, "xxx", "yyy"),
				new Const(1611, "aaa", "bbb"),
		};
		
		ConstantDetails[] dtls = new ConstantDetails[data.length];
		for (int i = 0; i < data.length; i++) {
			dtls[i] = new ConstantDetails(data[i].code, data[i].name, data[i].externalName, data[i].externalName);
		}
		ConstantNameConversion conv = new ConstantNameConversion("aaa", "bbb", dtls );
		
		checkList(conv.getConstantList(), data);
		tstCodeLookUp(conv, data);
		tstNameLookup(conv, data);
		
		Const[] lookup = new Const[1700];
		for (Const c : data) {
			lookup[c.code + 5] = c;
		}
		
		for (int i = 0; i < lookup.length; i++) {
			int code = i - 5;
			ConstantDetails constantDetails = conv.getConstantDetails(code);
			if (lookup[i] ==  null) {
				assertTrue(constantDetails==null, "" + code);
			} else {
				assertTrue(constantDetails!=null, "" + code);
				
				checkRetrievedDetails(lookup[i], constantDetails);
			}
		}
	}
	
	private void checkList(List<ConstantDetails> splitOptionList, Const[] splitOptionValues) {
		int min = Math.min(splitOptionValues.length, splitOptionList.size());
		
		for (int i = 0; i < min; i++) {
			assertEquals(splitOptionValues[i].code, splitOptionList.get(i).getCode());
			assertEquals(splitOptionValues[i].name, splitOptionList.get(i).getSimpleName());
			assertEquals(splitOptionValues[i].externalName, splitOptionList.get(i).getExternalName());
		}
		assertEquals(splitOptionValues.length, splitOptionList.size());
	}


	private void tstCodeLookUp(ConstantNameConversion splitOptions, Const[] conversionValues) {
		for (int i = 0; i < conversionValues.length; i++) {
			ConstantDetails constantDetails = splitOptions.getConstantDetails(conversionValues[i].code);
			checkRetrievedDetails(conversionValues[i], constantDetails);
		}
	}


	private void tstNameLookup(ConstantNameConversion splitOptions, Const[] conversionValues) {
		for (int i = 0; i < conversionValues.length; i++) {
			ConstantDetails constantDetails = splitOptions.getConstantDetails(conversionValues[i].name);
			checkRetrievedDetails(conversionValues[i], constantDetails);
		}
		for (int i = 0; i < conversionValues.length; i++) {
			ConstantDetails constantDetails = splitOptions.getConstantDetails(conversionValues[i].externalName);
			checkRetrievedDetails(conversionValues[i], constantDetails);
		}
	}


	private void checkRetrievedDetails(Const conversionValues,  ConstantDetails constantDetails) {
		assertEquals(conversionValues.code, constantDetails.getCode());
		assertEquals(conversionValues.name, constantDetails.getSimpleName());
		assertEquals(conversionValues.externalName, constantDetails.getExternalName());
	}
	
	private static class Const {
		private final int code;
		private final String name, externalName, jrecName;
		
		public Const(int code, String name, String externalName) {
			this(code, externalName, externalName, "");
		}
		public Const(int code, String name, String externalName, String jrecName) {
			super();
			this.code = code;
			this.name = name;
			this.externalName = externalName;
			this.jrecName = jrecName;
		}
		
		
	}

}
