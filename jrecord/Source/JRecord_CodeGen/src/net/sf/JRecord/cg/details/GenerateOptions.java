/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord CodeGen
 *    
 *    Sub-Project purpose: Generate Java - JRecord source code 
 *                        to read/write cobol data files.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: GPL
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
 *    GNU General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.cg.details;



import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cg.schema.LayoutDef;
import net.sf.JRecord.def.IO.builders.IIOBuilder;
import net.sf.JRecord.utilityClasses.ParseArguments;

public class GenerateOptions implements ArgNames {
	private static final Opts DEFAULT_FILE_ORG = new Opts("Constants.IO_*", "Constants.IO_*", "", Constants.IO_DEFAULT);

	private static final Opts[] TEMPLATE_OPTIONS = {
		new Opts("ioBuilder",  "", "Generate example code using JRecord IO Builders"),
		new Opts("ioBuilderFieldNameClass",  "", "Generate example code using JRecord IO Builders + Field Name Class"),
		new Opts("ioBuilderWithSchemaClass", "", "Generate example code using JRecord IO Builders + Schema details"),
		new Opts("javaPojo",   "", "Generate java classes for each Cobol Record"),
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
	public final String template, schemaName, schemaShortName, packageId, packageDir, font, outputDir, currentDate;
	public final boolean inLineSchema, LoadSchemaFromFile;
	
	public final Set<String> generateOptions = new HashSet<String>(10);
	public final Opts io, splitOption;
	public final LayoutDef schemaDefinition;
	
	public final boolean dropCopybookName, xmlSchema;
	
	public GenerateOptions(ParseArguments pa) {
		List<String> generateOpts = pa.getArgList(OPT_GENERATE);
		String splitVal = pa.getArg(OPT_SPLIT);
		String dropVal = pa.getArg(OPT_DROP_COPYBOOK_NAME, "");
		

		template = decode(pa, OPT_TEMPLATE, false, "javaPojo", TEMPLATE_OPTIONS);
		schemaName   = required(pa, OPT_SCHEMA);
		if (schemaName == null) {
			schemaShortName = "";
		} else {
			schemaShortName = (new File(schemaName)).getName();
		}
		if ("ioBuilder".equals(template)) {
			String s = pa.getArg(OPT_PACKAGE);
			if (s == null) {
				s = "";
			}
			packageId = s;
		} else {
			packageId = required(pa, OPT_PACKAGE);
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
		
		if ("javaPojo".equals(template)) {
			if (generateOpts == null || generateOpts.size() == 0) {
				for (Opts o : GENERATE_OPTS) {
					generateOptions.add(o.option);
				}
			} else {
				for (String s : generateOpts) {
					generateOptions.add(decode(s, OPT_GENERATE, false, "", GENERATE_OPTS));
				}
			}
			
			if (splitVal != null && splitVal.length() > 0) {
				System.out.println("Split does not work for javaPojo generation !!! ");
				ok = false;
			}
		} else if (generateOpts != null){
			generateOptions.addAll(generateOpts);
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
		
		SimpleDateFormat df = new SimpleDateFormat("d MMM yyyy H:m:s");
		currentDate = df.format(new Date());
	}
	
	private void processError(Exception e) {
		System.out.println();
		System.out.println(" Could not process the copybook (schema): " + e);
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
	
	private String decode(ParseArguments pa, String key, boolean printMsg, String defaultVal, Opts[] opts) {
		return decode(pa.getArg(key), key, printMsg, defaultVal, opts);
	}
	
	private String decode(String v, String key, boolean printMsg, String defaultVal, Opts[] opts) {
		
		if (v != null) {
			Opts o = decodeAsOpt(v, key, printMsg, null, opts);
			if (o != null) {
				if (o.code == null || o.code.length() == 0) {
					return o.option;
				}
				return o.code;
			}
			if (printMsg) {
				System.out.println("Invalid value " + v + " for argument " + key);
				ok = false;
			}
			
			return v;
		}
		
		return defaultVal;
	}

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
		System.out.println("    " + OPT_LOAD_SCHEMA + ":\tWether to generate a Schema (LayoutDetail) class or not");
		printList(LOAD_SCHEMA_OPTS);
		System.out.println("    " + OPT_GENERATE + ":\tWhich skeltons to generate, for template=javaPojo:");
		printList(GENERATE_OPTS);
		System.out.println();
		System.out.println("    -h -?:\tList options");
		System.out.println();
	}
	
	private static void printList(Opts[] list) {
		for (Opts o : list) {
			System.out.println("\t\t" + o.option + "\t- " + o.description );
		}
	}


	/**
	 * @return the ok
	 */
	public final boolean isOk() {
		return ok;
	}
	
	/**
	 * @return the schemaName
	 */
	public final String getSchemaName() {
		return schemaName;
	}

	/**
	 * @return the template
	 */
	public final String getTemplate() {
		return template;
	}

	/**
	 * @return the packageId
	 */
	public final String getPackageId() {
		return packageId;
	}

	/**
	 * @return the packageDir
	 */
	public final String getPackageDir() {
		return packageDir;
	}

	/**
	 * @return the font
	 */
	public final String getFont() {
		return font;
	}

	/**
	 * @return the outputDir
	 */
	public final String getOutputDir() {
		return outputDir;
	}

	/**
	 * @return the inLineSchema
	 */
	public final boolean isInLineSchema() {
		return inLineSchema;
	}

	/**
	 * @return the loadSchemaFromFile
	 */
	public final boolean isLoadSchemaFromFile() {
		return LoadSchemaFromFile;
	}

	/**
	 * @return the generateOptions
	 */
	public final Set<String> getGenerateOptions() {
		return generateOptions;
	}

	/**
	 * @return the io
	 */
	public final Opts getIo() {
		return io;
	}

	/**
	 * @return the splitOption
	 */
	public final Opts getSplitOption() {
		return splitOption;
	}

	/**
	 * @return the schemaDefinition
	 */
	public final LayoutDef getSchemaDefinition() {
		return schemaDefinition;
	}

	/**
	 * @return the dropCopybookName
	 */
	public final boolean isDropCopybookName() {
		return dropCopybookName;
	}

	/**
	 * @return the xmlSchema
	 */
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

	/**
	 * @return the schemaShotName
	 */
	public final String getSchemaShortName() {
		return schemaShortName;
	}

	/**
	 * @return the currentDate
	 */
	public final String getCurrentDate() {
		return currentDate;
	}
}
