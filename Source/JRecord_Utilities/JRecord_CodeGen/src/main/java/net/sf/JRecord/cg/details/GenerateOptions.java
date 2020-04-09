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

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.cg.schema.CodeGenFileName;
import net.sf.JRecord.cg.schema.LayoutDef;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.def.IO.builders.IIOBuilder;
import net.sf.JRecord.utilityClasses.ParseArguments;


public class GenerateOptions implements IGenerateOptions {
//	private static final String JAVA_POJO_TEMPLATE = "javaPojo";
	private static final CodeGenFileName EMPTY_FILE_NAME = new CodeGenFileName("");
	
	private static final ArgumentOption DEFAULT_FILE_ORG = new ArgumentOption("Constants.IO_*", "Constants.IO_*", "", Constants.IO_DEFAULT);

	private static final ArgumentOption[] TEMPLATE_OPTIONS = ArgumentOption.TEMPLATE_OPTIONS;
	private static final ArgumentOption[] LOAD_SCHEMA_OPTS = {
		new ArgumentOption("inLine",   "", "Create a SchemaClass in code"),
		new ArgumentOption("fromFile", "", "Reload schema's from a file"),
		new ArgumentOption("both",   "", "Do both, create schema class and allow user to read from a file")};
	private static final ArgumentOption[] FILE_ORGANISATION_OPTS = ArgumentOption.FILE_ORGANISATION_OPTS;
//	private static final Opts[] GENERATE_OPTS = {
//		new Opts("bean",     "","Generate Java \"bean\" class"),
//		new Opts("imutable", "","Generate Imutable class"),
//	};
	
	private static final ArgumentOption[] SPLIT_OPTS = ArgumentOption.SPLIT_OPTS;

	
	private boolean ok = true;
	private final String packageId, packageDir, font, outputDir;
	
//	private final Map<String, String> generateOptions = new HashMap<String, String>(10);
	private final ArgumentOption io, splitOption, dialect;
	private final LayoutDef schemaDefinition;
	
	private final boolean dropCopybookName;
	
	private TemplateDtls templateDtls;
	
	private StringBuilder message = new StringBuilder();
	
//	private final Properties templateProperties;
	
	public GenerateOptions(ParseArguments pa) {
		List<String> generateOpts = pa.getArgList(ArgumentOption.OPT_GENERATE);
		String splitVal = pa.getArg(ArgumentOption.OPT_SPLIT);
		String dialectVal = pa.getArg(ArgumentOption.OPT_DIALECT);
		String dropVal = pa.getArg(ArgumentOption.OPT_DROP_COPYBOOK_NAME, "");
		String versionStr =  pa.getArg(ArgumentOption.OPT_JREC_VERSION, "");
		int version = 0;
		if (versionStr.length() > 0) {
			try {
				double d = Double.parseDouble(versionStr) * 1000 + 0.5;
						
				version = (int) d;
			} catch (NumberFormatException e) {
				e.printStackTrace();
			}
		}
				
		String schemaName   = required(pa, ArgumentOption.OPT_SCHEMA);
		String templateDir  = pa.getArg(ArgumentOption.OPT_TEMPLATE_DIRECTORY, "");

		templateDtls = new TemplateDtls(
									templateDir,
									decodeTemplate(templateDir, pa.getArg(ArgumentOption.OPT_TEMPLATE)), 
									TemplateDtls.DEFAULT_TEMPLATE_BASE,
									false,
									version > 0 ? version : TemplateDtls.DEFAULT_JREC_VERSION,
									pa.getArg(ArgumentOption.OPT_GEN_DATE));

//		if (BASIC_TEMPLATE.equals(template)) {
		if (templateDtls.hasOption(TemplateDtls.T_REQUIRE_PACKAGE_ID)) {
			packageId = required(pa, ArgumentOption.OPT_PACKAGE);
		} else {
			String s = pa.getArg(ArgumentOption.OPT_PACKAGE);
			if (s == null) {
				s = "";
			}
			packageId = s;
		}
		if (! templateDtls.hasOption(TemplateDtls.T_SPLIT_ALLOWED)) {
			if (splitVal != null && splitVal.length() > 0) {
				appendMessage( "Split is not supported for " + templateDtls.template + " !!!");
				ok = false;
			}
		}
		
		if (packageId == null) {
			packageDir = "";
		} else {
			packageDir = Conversion.replace(packageId, ".", "/") + "/";
		}
		
		boolean inLineSchema = checkFor(pa, ArgumentOption.OPT_LOAD_SCHEMA, false, false, "inLine", "both");
		boolean loadSchemaFromFile = checkFor(pa, ArgumentOption.OPT_LOAD_SCHEMA, false, false, "fromFile", "both");
		if (inLineSchema) {
			templateDtls.generateOptions.put("inlineschema", true);
			if (loadSchemaFromFile) {
				templateDtls.generateOptions.put("loadschemafromfile", true);
			}
		} else {
			templateDtls.generateOptions.put("loadschemafromfile", true);
		}
		
		
		checkFor(pa, ArgumentOption.OPT_LOAD_SCHEMA, true, false,  LOAD_SCHEMA_OPTS);
		io = decodeAsOpt(pa, ArgumentOption.OPT_FILE_ORGANISATION, true, DEFAULT_FILE_ORG, FILE_ORGANISATION_OPTS);
		splitOption = decodeAsOpt(splitVal, ArgumentOption.OPT_SPLIT, false, SPLIT_OPTS[0], SPLIT_OPTS);
		dialect = decodeAsOpt(dialectVal, ArgumentOption.OPT_DIALECT, false, ArgumentOption.MAINFRAME_DIALECT, ArgumentOption.DIALECT_OPTS);
		
		dropCopybookName = dropVal != null && dropVal.toLowerCase().startsWith("t");
		font = pa.getArg(ArgumentOption.OPT_FONT_NAME, "");
		outputDir = pa.getArg(ArgumentOption.OPT_OUTPUT_DIR, ".");
		
		List<RecordSelect> recordSelect = new ArrayList<RecordSelect>(10);

		List<String> recSelList = pa.getArgList(ArgumentOption.OPT_RECSEL);
		if (recSelList != null && recSelList.size() > 0) {
			RecordSelect recSel;

			for (String s : recSelList) {
				recordSelect.add((recSel = new RecordSelect(s)));
				if (! recSel.ok()) {
					appendMessage("Invalid Record Selection=" + s);
				}
			}
		}

		if (generateOpts != null) {
			for (String s : generateOpts) {
				String key = s;
				String v = s;
				int pos = s.indexOf('.');
				if (pos > 0) {
					key = s.substring(0, pos);
					v = s.substring(pos+1);
				}
				templateDtls.generateOptions.put(key.toLowerCase(), v);
			}
		} else {
			templateDtls.loadDefaultOptions();
		}
		
		LayoutDef t = null;
		
		if (ok) {
			IIOBuilder ioBldr;
			boolean xmlSchema = schemaName.toLowerCase().endsWith(".xml");

			if (xmlSchema) {
				ioBldr = JRecordInterface1.SCHEMA_XML.newIOBuilder(schemaName);
			} else {
				ICobolIOBuilder cblIoBldr= JRecordInterface1.COBOL.newIOBuilder(schemaName)
						.setDialect(dialect.id)
						.setSplitCopybook(splitOption.id)
						.setFileOrganization(io.id)
						.setDropCopybookNameFromFields(dropCopybookName)
						.setFont(font);
				for (RecordSelect rs : recordSelect) {
					cblIoBldr.setRecordSelection(rs.recordName, newFieldSelection(rs.fieldName, "=", rs.value));
				}

				ioBldr = cblIoBldr;
			}
			try {
				LayoutDetail schema = ioBldr.getLayout();
				
				templateDtls.setMultiRecord(schema.getRecordCount() > 1);
				if (! templateDtls.hasOption(TemplateDtls.T_DUPLICATE_FIELD_NAMES)) {
					check4DuplicateFieldNames(schema);
				} else {
					check4DuplicateArrayFieldNames(schema);
				}

				t = new LayoutDef(schema, schemaName, null);
			} catch (RecordException e) {
				processError(e);
			} catch (IOException e) {
				processError(e);
			}
		} 				
		
		schemaDefinition = t;
	}

	/**
	 * @param schema
	 * @throws RuntimeException
	 */
	private void check4DuplicateFieldNames(LayoutDetail schema)
			throws RuntimeException {
		Set<String> duplicateFieldNames = schema.getDuplicateFieldNames();
		if (duplicateFieldNames != null && duplicateFieldNames.size() > 0) {
			HashSet<String> dups = new HashSet<String>(duplicateFieldNames.size() * 3 / 2);
			Iterator<String> dupIterator = duplicateFieldNames.iterator();
			while (dupIterator.hasNext()) {
				String s = dupIterator.next();
				int indexOf = s.indexOf('(');
				if (indexOf > 0) {
					s = s.substring(0, indexOf - 1);
				}
				dups.add(s);
			}
			
			StringBuilder b = new StringBuilder("Duplicate Field Names:");
			
			dupIterator = dups.iterator();
			for (int i = 0; dupIterator.hasNext(); i++) {
				if (i % 3 == 0) {
					b.append('\n');
				}
				b.append('\t').append(dupIterator.next());
			}
			System.err.println( b.toString() );
			System.err.println();
			throw new RuntimeException("Duplicate Cobol FieldNames ar not allowed for this Template");				
		}
	}
	
	private void check4DuplicateArrayFieldNames(LayoutDetail schema)
			throws RuntimeException {
		Set<String> duplicateFieldNames = schema.getDuplicateFieldNames();
		if (duplicateFieldNames != null && duplicateFieldNames.size() > 0) {
			HashSet<String> dups = new HashSet<String>(duplicateFieldNames.size() * 3 / 2);
			Iterator<String> dupIterator = duplicateFieldNames.iterator();
			while (dupIterator.hasNext()) {
				String s = dupIterator.next();
				int indexOf = s.indexOf('(');
				if (indexOf > 0) {
					dups.add( s.substring(0, indexOf - 1));
				}
			}
			
			if (dups.size() > 0) {
				StringBuilder b = new StringBuilder("Duplicate Array Field Names:");
				
				dupIterator = dups.iterator();
				for (int i = 0; dupIterator.hasNext(); i++) {
					if (i % 3 == 0) {
						b.append('\n');
					}
					b.append('\t').append(dupIterator.next());
				}
				System.err.println( b.toString() );
				System.err.println();
				throw new RuntimeException("Duplicate Cobol Array FieldNames ar not allowed");
			}
		}
	}

	
	private void appendMessage(String msg) {
		System.out.println(msg);
		if (message.length() > 0) {
			message.append("\n\n");
		}
		message.append(msg);
	}
	
	private String decodeTemplate(String templateDir, String template) {
		if (templateDir != null && templateDir.length() > 0) {
		} else if (template == null || template.length() == 0) {
			template = ArgumentOption.LINE_WRAPPER_TEMPLATE;
		}
		return template;
	}
//	
//	private Properties getProperties(String template)  {
//		Properties p = new Properties();
//		try {
//			p.load(this.getClass().getResourceAsStream("/net/sf/JRecord/cg/velocity/" + template + "/Generate.properties"));
//		} catch (IOException e) {
//			ok = false;
//			System.out.println();
//			System.out.println("Could not Load Template: " + e);
//			System.out.println();
//		}
//		return p;
//	}
	
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
	

	private void checkFor(ParseArguments pa, String key, boolean msg, boolean defaultVal, ArgumentOption[] opts) {
		String[] chkFor = new String[opts.length];
		int i = 0;
		for (ArgumentOption o : opts) {
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
	
//	/* (non-Javadoc)
//	 * @see net.sf.JRecord.cg.details.IGenerateOptions#hasOption(java.lang.String)
//	 */
//	public boolean hasOption(String opt) {
//		return ! "N".equals(templateProperties.get(opt));
//	}
//	
//	private void loadDefaultOptions() {
//		loadOptions("defaultOpts.");		
//	}
//
//	/**
//	 * @param arrayKey
//	 */
//	public void loadOptions(String arrayKey) {
//		String countStr = templateProperties.getProperty(arrayKey + '0');
//		
//		if (countStr != null && countStr.length() > 0) {
//			try {
//				int count = Integer.parseInt(countStr);
//				for (int i = 1; i <= count; i++) {
//					String key = arrayKey + i;
//					String k = templateProperties.getProperty(key);
//					String v = templateProperties.getProperty(key + ".val");
//					if (v == null) {
//						v = "true";
//					}
//					if (k != null) {
//						generateOptions.put(k, v);
//					}
//				}
//			} catch (NumberFormatException e) {
//			}
//		}
//	}
	
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

	private ArgumentOption decodeAsOpt(ParseArguments pa, String key, boolean printMsg, ArgumentOption defaultOption, ArgumentOption[] opts) {
		return decodeAsOpt(pa.getArg(key), key, printMsg, defaultOption, opts);
	}

	private ArgumentOption decodeAsOpt(String v, String key, boolean printMsg, ArgumentOption defaultOption, ArgumentOption[] opts) {
		
		if (v != null) {
			for (ArgumentOption o : opts) {
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
		System.out.println("    " + ArgumentOption.OPT_TEMPLATE_DIRECTORY + ":\tDirectory for user written Templates");
		System.out.println("    -Template:\tWhich template to generate");
		printList(TEMPLATE_OPTIONS);
		System.out.println("    -Schema:\tCobol copybook/Xml schema to generate code for");
		System.out.println("    " + ArgumentOption.OPT_PACKAGE + ":\tJava Package Id");
		System.out.println("    " + ArgumentOption.OPT_LOAD_SCHEMA + ":\tWether to generate a Schema (LayoutDetail) class or not");
		printList(LOAD_SCHEMA_OPTS);
		System.out.println("    " + ArgumentOption.OPT_DIALECT + ":\tCobol Dialect ???");
		printList(ArgumentOption.DIALECT_OPTS);
		System.out.println("    " + ArgumentOption.OPT_FILE_ORGANISATION + ":\tWhat sort of file will be read ???");
		printList(FILE_ORGANISATION_OPTS);
		System.out.println("    " + ArgumentOption.OPT_SPLIT + ":\tHow to split the copybook up");
		printList(SPLIT_OPTS);
		System.out.println("    " + ArgumentOption.OPT_FONT_NAME + ":\tFont (characterset name");
		System.out.println("    " + ArgumentOption.OPT_DROP_COPYBOOK_NAME + ":\tWhether to Drop the copybook name from the start of field names");
		System.out.println("    " + ArgumentOption.OPT_LOAD_SCHEMA + ":\tWether to generate a Schema (LayoutDetail) class or not");
//		printList(LOAD_SCHEMA_OPTS);
//		System.out.println("    " + OPT_GENERATE + ":\tWhich skeltons to generate, for template=javaPojo:");
//		printList(GENERATE_OPTS);
		System.out.println("    " + ArgumentOption.OPT_OUTPUT_DIR + ":\tOutput directory");
		System.out.println();
		System.out.println("    -h -?:\tList options");
		System.out.println();
	}
	
	private static void printList(ArgumentOption[] list) {
		for (ArgumentOption o : list) {
			System.out.println("\t\t" + o.option + "\t- " + o.description );
		}
	}


	public final boolean isOk() {
		return ok && templateDtls.isOk();
	}
	
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getDataFileName()
	 */
	@Override
	public CodeGenFileName getDataFileName() {
		return EMPTY_FILE_NAME;
	}

	@Override
	public final TemplateDtls getTemplateDtls() {
		return templateDtls;
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
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getIo()
	 */
	@Override
	public final ArgumentOption getFileStructureCode() {
		return io;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getSplitOption()
	 */
	@Override
	public final ArgumentOption getSplitOption() {
		return splitOption;
	}

	/**
	 * @return the dialect
	 */
	@Override
	public final ArgumentOption getDialect() {
		return dialect;
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
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getConstantValues()
	 */
	@Override
	public ConstantVals getConstantValues() {
		return ConstantVals.CONSTANT_VALUES;
	}
	
	
	public String getMessage() {
		return message.toString();
	}
    
    public static ExternalFieldSelection newFieldSelection(String fieldName, String op, String value) {
    	ExternalFieldSelection r = new ExternalFieldSelection(fieldName, value, op);
    	r.setCaseSensitive(false);
    	return r;
    }

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#getJRecordVersion()
	 */
	@Override
	public int getJRecordVersion() {
		return templateDtls.getJrecordVersion();
	}
   


	@Override
	public String getDefinitionPackageId() {
		return DEFAULT_DEFINITION_PACKAGE_ID;
	}   
//    public static ExternalGroupSelection<ExternalSelection> newAnd(ExternalSelection... selections) {
//    	return ExternalGroupSelection.newAnd(selections);
//    }
//    
//    public static ExternalGroupSelection<ExternalSelection> newOr(ExternalSelection... selections) {
//    	return ExternalGroupSelection.newOr(selections);
//    }

}
