package net.sf.JRecord.zcg.zTest;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.cg.details.ArgumentOption;
import net.sf.JRecord.cg.details.GenerateOptions;
import net.sf.JRecord.cg.details.ParseArgs;

import org.junit.Test;

import test.gen.TstIoBldrGen01;

/**
 * Check the behaviour of the generate options class
 * @author Bruce Martin
 *
 */
public class TstGenerateOptions1 {

	private static final String JAVA_POJO = "javaPojo";
	private static final String STANDARD_TEMPLATE = "standard";
	private static final String MY_PACKAGE_ID = "MyPackageId";
	private static final String OUTPUT_DIR = "G:/Temp/Gen/ioBuilder";
	private static final String US_EBCDIC = "CP037";
	private static final String BASIC_TEMPLATE = "basic";

	@Test
	public void test01() {
		String[] arguments1 = {
				ArgumentOption.OPT_TEMPLATE, BASIC_TEMPLATE,
				ArgumentOption.OPT_SCHEMA, TstIoBldrGen01.class.getResource("DTAR020.cbl").getFile(),
				ArgumentOption.OPT_FILE_ORGANISATION, "FixedWidth",
				ArgumentOption.OPT_FONT_NAME, US_EBCDIC,
				ArgumentOption.OPT_DROP_COPYBOOK_NAME, "true",
				ArgumentOption.OPT_OUTPUT_DIR, OUTPUT_DIR
		};
		GenerateOptions g = new GenerateOptions(new ParseArgs(arguments1));
		
		assertEquals(BASIC_TEMPLATE, g.getTemplateDtls().getTemplate());
		chkValues(g);
		
		arguments1[1] = STANDARD_TEMPLATE;
		
		g = new GenerateOptions(new ParseArgs(arguments1));		
		assertFalse(g.isOk());
		assertEquals(1, g.getTemplateDtls().getGenerateOptions().size());
		
		ArrayList<String> argList = new ArrayList<String>(Arrays.asList(arguments1));
		argList.add(ArgumentOption.OPT_PACKAGE);
		argList.add(MY_PACKAGE_ID);
		
		arguments1 = argList.toArray(new String[argList.size()]);
		
		g = new GenerateOptions(new ParseArgs(arguments1));	
		assertEquals(STANDARD_TEMPLATE, g.getTemplateDtls().getTemplate());
		chkValues1(g);
		assertEquals(1, g.getTemplateDtls().getGenerateOptions().size());
		
		arguments1[1] = JAVA_POJO;
		g = new GenerateOptions(new ParseArgs(arguments1));	
		assertEquals(JAVA_POJO, g.getTemplateDtls().getTemplate());
		chkValues1(g);
		assertEquals(3, g.getTemplateDtls().getGenerateOptions().size());
		assertTrue(g.getTemplateDtls().getGenerateOptions().containsKey("bean"));
		
	}

	private void chkValues1(GenerateOptions g) {
		chkValues(g);
		assertEquals(MY_PACKAGE_ID, g.getPackageId());
	}

	/**
	 * @param g
	 */
	private void chkValues(GenerateOptions g) {
		assertEquals(true, g.isOk());
		assertEquals(Constants.IO_FIXED_LENGTH, g.getFileStructureCode().id);
		assertEquals(US_EBCDIC, g.getFont());
		assertEquals(true, g.isDropCopybookName());
		assertEquals(OUTPUT_DIR, g.getOutputDir());
	}

}
