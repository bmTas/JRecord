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
      
package net.sf.JRecord.cg.velocity;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;
import org.apache.velocity.app.event.EventCartridge;
import org.apache.velocity.app.event.implement.IncludeRelativePath;
import org.apache.velocity.exception.ParseErrorException;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.cg.details.IGenerateOptions;
import net.sf.JRecord.cg.details.TemplateDtls;
import net.sf.JRecord.cg.details.xml.SkelGenDefinition;
import net.sf.JRecord.cg.details.xml.Skelton;
import net.sf.JRecord.cg.schema.RecordDef;


public class GenerateVelocity {
	
//	private static final String OUTPUT_FILE = ".output";
//	private static final String TEMPLATE = ".template";
//	private static final String IF = ".if.";
//	private static final String SKEL_PREF = "skel.";
	
	public final List<GeneratedSkelDetails> generatedFiles = new ArrayList<GeneratedSkelDetails>();
	
	public GenerateVelocity(IGenerateOptions opts, Object sourceApp) throws IOException, XMLStreamException, FactoryConfigurationError {
		
	
//		try {
			generate(opts, sourceApp);
//		} catch (Exception e) {
//			e.printStackTrace();
//		}
	}
	
	private void generate(IGenerateOptions opts, Object sourceApp) throws IOException, XMLStreamException, FactoryConfigurationError  {
		
		SkelGenDefinition skeltonDetails = opts.getTemplateDtls().getSkeltons();

		String outputFile;
		String outputDir = opts.getOutputDir();
		if ((! outputDir.endsWith("/")) && (! outputDir.endsWith("\\")) ) {
			outputDir = outputDir + "/";
		}

		List<Skelton> skeltons = skeltonDetails.getLayoutSkeltons();
		List<RecordDef> records = opts.getSchemaDefinition().getRecords();
		if (skeltons != null) {
			for (Skelton skel : skeltons) {
				if (skel.generate) {
						outputFile = expand(opts, null, outputDir + skel.getOutput());
						genSkel(skel.getTemplateFileName(), outputFile, opts, null, skel.getPackageExtension(), sourceApp);
					    generatedFiles.add(
					    		new GeneratedSkelDetails(
					    				outputFile, 
					    				expand( opts, 
					    						records != null && records.size() > 0 ? records.get(0) : null, 
					    						skel.getOutput()), 
					    						skel.getDescription()));
				}
			}
		}
		
		skeltons = skeltonDetails.getRecordSkeltons();
		if (skeltons != null) {
			for (Skelton skel : skeltons) {
				if (skel.generate) {
					for (RecordDef r : records) {
						outputFile = expand(opts, r, outputDir + skel.getOutput());
						genSkel(skel.getTemplateFileName(), outputFile, opts, r, skel.getPackageExtension(), sourceApp);
					    generatedFiles.add(
					    		new GeneratedSkelDetails(
					    				outputFile, 
					    				expand(opts, r, skel.getOutput()), 
					    				skel.getDescription()));
					}				
				}
			}
		}
		
//		int optCount;
//
//		Properties templateProperties = opts.getTemplateDtls().templateProperties;
//		int skelCount = Integer.parseInt(templateProperties.getProperty(SKEL_PREF + "0"));
//		String mod, outputFile, s;
//		for (int i = 1; i <= skelCount; i++) {
//			boolean gen = false;
//			mod =  Conversion.replace(
//								getString(templateProperties, SKEL_PREF + i + TEMPLATE),
//								"&template.", 
//								opts.getTemplateDtls().getTemplate()
//				   ).toString();
//
//			s = getString(templateProperties, SKEL_PREF + i + IF + "0");
//			if (s == null || s.length() == 0) {
//				gen = true;
//			} else {
//				optCount = Integer.parseInt(s);
//				for (int j = 1; j <= optCount; j++) { 
//					s = getString(templateProperties, SKEL_PREF + i + IF + j);
//					if (opts.getTemplateDtls().getGenerateOptions().containsKey(s.toLowerCase())) {
//						gen = true;
//						break;
//					}
//				}
//			}
//			
//			if (gen) {
//				String genAt  =  getString(templateProperties, SKEL_PREF + i + ".genAt");
//				String fileExtension = getString(templateProperties,  SKEL_PREF + i + OUTPUT_FILE);
//				String fileDescription = getString(templateProperties,  SKEL_PREF + i + ".description");
//				int st = fileExtension.indexOf('/');
//				int en = fileExtension.lastIndexOf('/');
//				String extra = "";
//				if (st >= 0 && en > st) {
//					StringBuilder b = new StringBuilder()
//											.append('.')
//											.append(fileExtension.substring(st+1, en));
//				    extra = Conversion.replace(b, "/", ".").toString();
//				}
//				String outputDir = opts.getOutputDir();
//				if ((! outputDir.endsWith("/")) && (! outputDir.endsWith("\\")) ) {
//					outputDir = outputDir + "/";
//				}
//				if (genAt != null && "record".equals(genAt.toLowerCase())) {
//					for (RecordDef r : opts.getSchemaDefinition().getRecords()) {
//						outputFile = expand(opts, r, outputDir + fileExtension);
//						genSkel(mod, outputFile, opts, r, extra, sourceApp);
//					    generatedFiles.add(new GeneratedSkel(outputFile, expand(opts, r, fileExtension), fileDescription));
//					}
//				} else {
//					outputFile = expand(opts, null, outputDir + fileExtension);
//					genSkel(mod, outputFile, opts, null, extra, sourceApp);
//				    generatedFiles.add(new GeneratedSkel(outputFile, expand(opts, null, fileExtension), fileDescription));
//				}
//			}
//		}
	}
	
//	private String getString(Properties rb, String key) {
//		if (rb.containsKey(key)) {
//			return rb.getProperty(key);
//		}
//		return null;
//	}
	
	private String expand(IGenerateOptions opts, RecordDef r, String s) {
		StringBuilder b = new StringBuilder(s);
		Conversion.replace(b, "&suffix.", opts.getSchemaDefinition().getExtensionName() );
		Conversion.replace(b, "&directory.", opts.getPackageDir());
		Conversion.replace(b, "&template.", opts.getTemplateDtls().getTemplate());
		if (r != null) {
			Conversion.replace(b, "&recordSuffix.", r.getExtensionName() );
		}
		Conversion.replace(b, "//", "/");
		return b.toString();
	}
	
    /**
     * Generate a velocity skelton
     *
     * @param templateFile file to be generated
     * @param writer output writer
     * @param context variable definitions
     * @throws IOException 
     *
     * @throws Exception any error that occurs
     */
    public final void genSkel(String templateFile, String outputFile, IGenerateOptions opts,
    		RecordDef r, String packageExtension, Object sourceApp ) 
    throws IOException {

        /*
         *  get the Template object.  This is the parsed version of your
         *  template input file.  Note that getTemplate() can throw
         *   ResourceNotFoundException : if it doesn't find the template
         *   ParseErrorException : if there is something wrong with the VTL
         *   Exception : if something else goes wrong (this is generally
         *        indicative of as serious problem...)
         */
    	
    	VelocityContext context = new VelocityContext();
        //Template template =  null;

		EventCartridge ec = new EventCartridge();

		ec.addEventHandler(new IncludeRelativePath());

		context.put("generateOptions", opts);
		context.put("packageId", opts.getPackageId() + packageExtension);
		context.put("SourceApplication", sourceApp);
		if (r != null) {
			context.put("currentRecord", r);
		}
		context.attachEventCartridge(ec);
		
		Files.createDirectories(Paths.get(outputFile).getParent());
		Writer writer = new FileWriter(outputFile);

		System.out.println("Template: " + templateFile);
		
		ArrayList<SkelLineNum> skelLines = new ArrayList<SkelLineNum>();
		String template = null;
		try {
			 template = loadTemplate(skelLines, opts, templateFile);
			Velocity.evaluate( context, writer, "log tag name", template);
		} catch(ParseErrorException pe) {
			String errorText = pe.toString();
			
			System.err.println(template);
			int idx1 = errorText.indexOf("line");
			if (idx1 > 0) {
				int idx2 = errorText.indexOf(",", idx1 + 5);
				if (idx2 > 0) {
					try {
						int lineNo = Integer.parseInt(errorText.substring(idx1 + 5, idx2).trim());
						int i = 0;
						while (i < skelLines.size() && lineNo > skelLines.get(i).lineCount) {
							lineNo -= skelLines.get(i).lineCount;
							i += 1;
						}
						errorText = errorText.substring(0, idx1)
								+ "line "
								+ (skelLines.get(i).lineCountStart + lineNo) + " of " + skelLines.get(i).name
								+ errorText.substring(idx2);
					} catch (Exception e) {
						e.printStackTrace();
					}
				}
			}
			throw new RuntimeException(errorText);
		} finally {
	        writer.flush();
	        writer.close();
		}
    }

    private String loadTemplate(List<SkelLineNum> skelLines, IGenerateOptions opts, final String templatePath) {
    	StringBuilder b = new StringBuilder();
    	loadTemplate(b, skelLines, opts, templatePath);
    	return b.toString();
    }

	/**
	 * @param templatePath
	 * @param b
	 * @throws RuntimeException
	 */
	private void loadTemplate(StringBuilder b, List<SkelLineNum> skelLines, IGenerateOptions opts, String templatePath) {
		BufferedReader r = null;
		try { 	
			InputStream in = null;
			String templateDir = opts.getTemplateDtls().templateDir;
			if (templatePath.startsWith("$std.")) {
				templatePath = templatePath.substring(5);
				in = this.getClass().getResourceAsStream(TemplateDtls.DEFAULT_TEMPLATE_BASE + templatePath);
			}
			if (in == null && templateDir != null && templateDir.length() > 0) {
				char lastChar = templateDir.charAt(templateDir.length() - 1);
				if (lastChar != '/' && lastChar != '\\') {
					templateDir = templateDir + '/';
				}
				File f = new File(templateDir + templatePath);
				if (f.exists()) {
					in = new FileInputStream(f);
				}
			} 
			if (in == null) {
				String name = opts.getTemplateDtls().templateBase + templatePath;
				in = this.getClass().getResourceAsStream(name);
			} 
			if (in == null && (! TemplateDtls.DEFAULT_TEMPLATE_BASE.equals(opts.getTemplateDtls().templateBase))) {
				in = this.getClass().getResourceAsStream(TemplateDtls.DEFAULT_TEMPLATE_BASE + templatePath);
			} 
			if (in == null) {
				throw new RuntimeException("Template not found: " + templatePath + ", searched: "
						+ templateDir + ", " + opts.getTemplateDtls().templateBase);
			}
    		String s;
    		
    		SkelLineNum lineNumHolder = new SkelLineNum(templatePath, 0);
			skelLines.add(lineNumHolder);
    		r = new BufferedReader(new InputStreamReader(in));
    		while ((s = r.readLine()) != null) {
    			if (s.trim().startsWith("#incl(")) {
    				s = s.trim();
    				s = s.substring(6, s.length() -1).trim();
    				//System.out.print("~~>>" + s + "<<");
    				s = s.substring(1, s.length() - 1);
    				//System.out.println("\t~~>>" + s + "<<");
    				loadTemplate(b, skelLines, opts, s);
    				lineNumHolder = new SkelLineNum(templatePath, lineNumHolder.lineCountStart + lineNumHolder.lineCount + 1);
    				
    				skelLines.add(lineNumHolder);
    			} else {
    				b.append(s).append('\n');
    				lineNumHolder.lineCount += 1;
    			}
    		}
    	} catch (IOException ex) { 
    		throw new RuntimeException(ex);
    	} finally {
    		if (r != null) {
	    		try {
					r.close();
				} catch (IOException e) {
					throw new RuntimeException(e);
				}
    		}
    	}
	}
	
	private static class SkelLineNum {
		private final String name;
		private int lineCount = 0;
		private final int lineCountStart;
		
		protected SkelLineNum(String name, int start) {
			this.name = name;
			this.lineCountStart = start;
		}
		
	}
}
