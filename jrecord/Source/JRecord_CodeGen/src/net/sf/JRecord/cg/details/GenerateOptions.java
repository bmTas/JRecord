/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord CodeGen
 *    
 *    Sub-Project purpose: Generate Java - JRecord source code 
 *                        to read/write cobol data files.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: GPL 3 or later
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU General Public License
 *    as published by the Free Software Foundation; either
 *    version 3.0 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.cg.details;



import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cg.schema.LayoutDef;
import net.sf.JRecord.def.IO.builders.IIOBuilder;
import net.sf.JRecord.utilityClasses.ParseArguments;

public class GenerateOptions implements ArgNames, IGenerateOptions {
	private static final String JAVA_POJO_TEMPLATE = "javaPojo";
	private static final String OPT_REQUIRE_PACKAGE_ID = "requirePackageId";
	private static final String OPT_SPLIT_ON_REDEF = "splitOnRedef";

	private static final Opts DEFAULT_FILE_ORG = new Opts("Constants.IO_*", "Constants.IO_*", "", Constants.IO_DEFAULT);

	private static final Opts[] TEMPLATE_OPTIONS = {
		new Opts("basic",  "", "Generate example code using JRecord IO Builders"),
		new Opts("standard",  "", "Generate example code using JRecord IO Builders + Field Name Class"),
		new Opts("schemaClass", "", "Generate example code using JRecord IO Builders + Schema details"),
		new Opts(JAVA_POJO_TEMPLATE,   "", "Generate java classes for each Cobol Record"),
	};
	private static final Opts[] LOAD_SCHEMA_OPTS = {
		new Opts("inLine",   "", "Create a SchemaClass in code"),
		new Opts("fromFile", "", "Reload schema's from a file"),
		new Opts("both",   "", "Do both, create schema class and allow user to read from a file")};
	private static final Opts[] FILE_ORGANISATION_OPTS = {
		new Opts("Text",         "Constants.IO_BIN_TEXT",     "Standard Windows/Unix text file (single byte characterset)", Constants.IO_BIN_TEXT),
		new Opts("FixedWidth",   "Constants.IO_FIXED_LENGTH", "File where lines (records) are the same length no \\n", Constants.IO_FIXED_LENGTH),
		new Opts("Mainframe_VB", "Constants.IO_VB",           "Mainframe VB, file consists of <record-length><record-data>", Constants.IO_VB)};
	private static final Opts[] GENERATE_OPTS = {
		new Opts("bean",     "","Generate Java \"bean\" class"),
		new Opts("imutable", "","Generate Imutable class"),
	};
	
	private static final Opts[] SPLIT_OPTS = {
		new Opts("None",      "CopybookLoader.SPLIT_NONE", "No Copybook split", CopybookLoader.SPLIT_NONE ),
		new Opts("01",        "CopybookLoader.SPLIT_01_LEVEL", "Split on 01", CopybookLoader.SPLIT_01_LEVEL),
		new Opts("redefine",  "CopybookLoader.SPLIT_REDEFINE", "Split on redefine", CopybookLoader.SPLIT_REDEFINE),
		new Opts("highest",   "CopybookLoader.SPLIT_HIGHEST_REPEATING", "Hightest Level", CopybookLoader.SPLIT_HIGHEST_REPEATING),
	};

	
	private boolean ok = true;
	private final String template, schemaName, schemaShortName, packageId, packageDir, font, outputDir, currentDate, currentDateTime;
	private final boolean inLineSchema, LoadSchemaFromFile;
	
	private final Map<String, String> generateOptions = new HashMap<String, String>(10);
	private final Opts io, splitOption;
	private final LayoutDef schemaDefinition;
	
	private final boolean dropCopybookName, xmlSchema;
	
	private final Properties templateProperties;
	
	public GenerateOptions(ParseArguments pa) {
		List<String> generateOpts = pa.getArgList(OPT_GENERATE);
		String splitVal = pa.getArg(OPT_SPLIT);
		String dropVal = pa.getArg(OPT_DROP_COPYBOOK_NAME, "");
		

		template = decodeTemplate(pa.getArg(OPT_TEMPLATE));
		templateProperties = getProperties(template);
		
		schemaName   = required(pa, OPT_SCHEMA);
		if (schemaName == null) {
			schemaShortName = "";
		} else {
			schemaShortName = (new File(schemaName)).getName();
		}
//		if (BASIC_TEMPLATE.equals(template)) {
		if (hasOption(OPT_REQUIRE_PACKAGE_ID)) {
			packageId = required(pa, OPT_PACKAGE);
		} else {
		
			String s = pa.getArg(OPT_PACKAGE);
			if (s == null) {
				s = "";
			}
			packageId = s;
		}
		if (! hasOption(OPT_SPLIT_ON_REDEF)) {
			if (splitVal != null && splitVal.length() > 0) {
				System.out.println("Split does is not supported for " + template + " !!!");
				ok = false;
			}
		}
		
		if (packageId == null) {
			packageDir = "";
		} else {
			packageDir = Conversion.replace(packageId, ".", "/") + "/";
		}
		
		inLineSchema = checkFor(pa, OPT_LOAD_SCHEMA, false, false, "inLine", "both");
		LoadSchemaFromFile = checkFor(pa, OPT_LOAD_SCHEMA, false, false, "fromFile", "both");
		checkFor(pa, OPT_LOAD_SCHEMA, true, false,  LOAD_SCHEMA_OPTS);
		io = decodeAsOpt(pa, OPT_FILE_ORGANISATION, true, DEFAULT_FILE_ORG, FILE_ORGANISATION_OPTS);
		splitOption = decodeAsOpt(splitVal, OPT_SPLIT, false, SPLIT_OPTS[0], SPLIT_OPTS);
		
		dropCopybookName = dropVal != null && dropVal.toLowerCase().startsWith("t");
		font = pa.getArg(OPT_FONT_NAME, "");
		outputDir = pa.getArg(OPT_OUTPUT_DIR, ".");
		
		
		if (generateOpts != null) {
			for (String s : generateOpts) {
				String key = s;
				String v = s;
				int pos = s.indexOf('.');
				if (pos > 0) {
					key = s.substring(0, pos);
					v = s.substring(pos+1);
				}
				generateOptions.put(key.toLowerCase(), v);
			}
		} else {
			loadDefaultOptions();
		}
		
		LayoutDef t = null;
		
		if (ok) {
			IIOBuilder ioBldr;
			xmlSchema = schemaName.toLowerCase().endsWith(".xml");

			if (xmlSchema) {
				ioBldr = JRecordInterface1.SCHEMA_XML.newIOBuilder(schemaName);
			} else {
				ioBldr = JRecordInterface1.COBOL.newIOBuilder(schemaName)
						.setDialect(ICopybookDialects.FMT_MAINFRAME)
						.setSplitCopybook(splitOption.id)
						.setFileOrganization(io.id)
						.setDropCopybookNameFromFields(dropCopybookName)
						.setFont(font);
			}
			try {
				t = new LayoutDef(ioBldr.getLayout());
			} catch (RecordException e) {
				processError(e);
			} catch (IOException e) {
				processError(e);
			}
		} else {
			xmlSchema = false;
		}
				
		
		schemaDefinition = t;
		
		Date date = new Date();
		currentDateTime = new SimpleDateFormat("d MMM yyyy H:m:s").format(date);
		currentDate = new SimpleDateFormat("d MMM yyyy").format(date);
	}
	
	private String decodeTemplate(String template) {
		if (template == null || template.length() == 0) {
			template = JAVA_POJO_TEMPLATE;
		}
		return template;
	}
	
	private Properties getProperties(String template)  {
		Properties p = new Properties();
		try {
			p.load(this.getClass().getResourceAsStream("/net/sf/JRecord/cg/velocity/" + template + "/Generate.properties"));
		} catch (IOException e) {
			ok = false;
			System.out.println();
			System.out.println("Could not Load Template: " + e);
			System.out.println();
		}
		return p;
	}
	
	private void processError(Exception e) {
		System.out.println();
		System.out.println("Could not process the copybook (schema): " + e);
		System.out.println();
		ok = false;
	}
	
	private String  required(ParseArguments pa, String key) {
		String r = pa.getArg(key);
		if (r == null || r.length() == 0) {
			System.out.println("Argument: " + key + " is required !!!");
			ok = false;
		}
		return r;
	}
	

	private void checkFor(ParseArguments pa, String key, boolean msg, boolean defaultVal, Opts[] opts) {
		String[] chkFor = new String[opts.length];
		int i = 0;
		for (Opts o : opts) {
			chkFor[i++] = o.option;
		}
		checkFor(pa, key, msg, defaultVal, chkFor);
	}
	
	private boolean checkFor(ParseArguments pa, String key, boolean msg, boolean defaultVal, String... opts) {
		String v = pa.getArg(key);
		
		if (v != null) {
			for (String o : opts) {
				if (o.equalsIgnoreCase(v)) {
					return true;
				}
			}
			if (msg) {
				System.out.println("Invalid value " + v + " for argument " + key);
				ok = false;
			}
		}
		
		return defaultVal;
	}
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#hasOption(java.lang.String)
	 */
	@Override
	public boolean hasOption(String opt) {
		return ! "N".equals(templateProperties.get(opt));
	}
	
	private void loadDefaultOptions() {
		String arrayKey = "defaultOpts.";
		String countStr = templateProperties.getProperty(arrayKey + '0');
		
		if (countStr != null && countStr.length() > 0) {
			try {
				int count = Integer.parseInt(countStr);
				for (int i = 1; i <= count; i++) {
					String key = arrayKey + i;
					String k = templateProperties.getProperty(key);
					String v = templateProperties.getProperty(key + ".val");
					if (v == null) {
						v = k;
					}
					if (k != null) {
						generateOptions.put(k, v);
					}
				}
			} catch (NumberFormatException e) {
			}
			
		}
				
	}
	
//	private String decode(ParseArguments pa, String key, boolean printMsg, String defaultVal, Opts[] opts) {
//		return decode(pa.getArg(key), key, printMsg, defaultVal, opts);
//	}
//	
//	private String decode(String v, String key, boolean printMsg, String defaultVal, Opts[] opts) {
//		
//		if (v != null) {
//			Opts o = decodeAsOpt(v, key, printMsg, null, opts);
//			if (o != null) {
//				if (o.code == null || o.code.length() == 0) {
//					return o.option;
//				}
//				return o.code;
//			}
//			if (printMsg) {
//				System.out.println("Invalid value " + v + " for argument " + key);
//				ok = false;
//			}
//			
//			return v;
//		}
//		
//		return defaultVal;
//	}

	private Opts decodeAsOpt(ParseArguments pa, String key, boolean printMsg, Opts defaultOption, Opts[] opts) {
		return decodeAsOpt(pa.getArg(key), key, printMsg, defaultOption, opts);
	}

	private Opts decodeAsOpt(String v, String key, boolean printMsg, Opts defaultOption, Opts[] opts) {
		
		if (v != null) {
			for (Opts o : opts) {
				if (o.option.equalsIgnoreCase(v)) {	
					return o;
				}
			}
			if (printMsg) {
				System.out.println("Invalid value " + v + " for argument " + key);
				ok = false;
			}
		}
		
		return defaultOption;
	}

	public static void printOptions() {
		System.out.println();
		System.out.println(" -------------------------------------------------------");
		System.out.println("Program: CodeGen");
		System.out.println("Purpose: Generate Skelton JRecord Code from a Cobol Copybook");
		System.out.println();
		System.out.println(" -------------------------------------------------------");
		System.out.println("Program Options:");
		System.out.println();
		System.out.println("    -Template:\tWhich template to generate");
		printList(TEMPLATE_OPTIONS);
		System.out.println("    -Schema:\tCobol copybook/Xml schema to generate code for");
		System.out.println("    " + OPT_PACKAGE + ":\tJava Package Id");
		System.out.println("    " + OPT_LOAD_SCHEMA + ":\tWether to generate a Schema (LayoutDetail) class or not");
		printList(LOAD_SCHEMA_OPTS);
		System.out.println("    " + OPT_FILE_ORGANISATION + ":\tWhat sort of file will be read ???");
		printList(FILE_ORGANISATION_OPTS);
		System.out.println("    " + OPT_SPLIT + ":\tHow to split the copybook up");
		printList(SPLIT_OPTS);
		System.out.println("    " + OPT_FONT_NAME + ":\tFont (characterset name");
		System.out.println("    " + OPT_DROP_COPYBOOK_NAME + ":\tWhether to Drop the copybook name from the start of field names");
		System.out.println("    " + OPT_LOAD_SCHEMA + ":\tWether to generate a Schema (LayoutDetail) class or not");
		printList(LOAD_SCHEMA_OPTS);
		System.out.println("    " + OPT_GENERATE + ":\tWhich skeltons to generate, for template=javaPojo:");
		printList(GENERATE_OPTS);
		System.out.println("    " + OPT_OUTPUT_DIR + ":\tOutput directory");
		System.out.println();
		System.out.println("    -h -?:\tList options");
		System.out.println();
	}
	
	private static void printList(Opts[] list) {
		for (Opts o : list) {
			System.out.println("\t\t" + o.option + "\t- " + o.description );
		}
	}


	public final boolean isOk() {
		return ok;
	}
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getSchemaName()
	 */
	@Override
	public final String getSchemaName() {
		return schemaName;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getTemplate()
	 */
	@Override
	public final String getTemplate() {
		return template;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getPackageId()
	 */
	@Override
	public final String getPackageId() {
		return packageId;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getPackageDir()
	 */
	@Override
	public final String getPackageDir() {
		return packageDir;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getFont()
	 */
	@Override
	public final String getFont() {
		return font;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getOutputDir()
	 */
	@Override
	public final String getOutputDir() {
		return outputDir;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#isInLineSchema()
	 */
	@Override
	public final boolean isInLineSchema() {
		return inLineSchema;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#isLoadSchemaFromFile()
	 */
	@Override
	public final boolean isLoadSchemaFromFile() {
		return LoadSchemaFromFile;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getGenerateOptions()
	 */
	@Override
	public final Map<String, String> getGenerateOptions() {
		return generateOptions;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getIo()
	 */
	@Override
	public final Opts getIo() {
		return io;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getSplitOption()
	 */
	@Override
	public final Opts getSplitOption() {
		return splitOption;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getSchemaDefinition()
	 */
	@Override
	public final LayoutDef getSchemaDefinition() {
		return schemaDefinition;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#isDropCopybookName()
	 */
	@Override
	public final boolean isDropCopybookName() {
		return dropCopybookName;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#isXmlSchema()
	 */
	@Override
	public final boolean isXmlSchema() {
		return xmlSchema;
	}

	public static final class Opts {
		public final String option, code, description;
		public final int id;
		
		private Opts(String option, String code, String description) {
			this(option, code, description, 0);
		}
		private Opts(String option, String code, String description, int id) {
			super();
			this.option = option;
			this.code = code;
			this.description = description;
			this.id = id;
		}
		
		/**
		 * @return the option
		 */
		public final String getOption() {
			return option;
		}
		
		/**
		 * @return the code
		 */
		public final String getCode() {
			return code;
		}
		
		/**
		 * @return the description
		 */
		public final String getDescription() {
			return description;
		}
		
		/**
		 * @return the id
		 */
		public final int getId() {
			return id;
		}
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getSchemaShortName()
	 */
	@Override
	public final String getSchemaShortName() {
		return schemaShortName;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getCurrentDate()
	 */
	@Override
	public final String getCurrentDate() {
		return currentDate;
	}
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getCurrentDateTime()
	 */
	@Override
	public final String getCurrentDateTime() {
		return currentDateTime;
	}

}
