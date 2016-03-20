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
      
package net.sf.JRecord.cg.velocity;

import java.io.BufferedReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Locale;
import java.util.ResourceBundle;

import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;
import org.apache.velocity.app.event.EventCartridge;
import org.apache.velocity.app.event.implement.IncludeRelativePath;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.cg.details.GenerateOptions;
import net.sf.JRecord.cg.schema.RecordDef;


public class GenerateVelocity {
	
	private static final String OUTPUT_FILE = ".output";
	private static final String TEMPLATE = ".template";
	private static final String OPT = ".opt.";
	private static final String SKEL_PREF = "skel.";
	
	public GenerateVelocity(GenerateOptions opts) {
		ResourceBundle rb = ResourceBundle.getBundle(
				"net.sf.JRecord.cg.velocity." + opts.template + ".Generate",
				Locale.getDefault(),
				GenerateVelocity.class.getClassLoader());
		if (rb == null) {
			System.out.println("Template: " +  opts.template + " does not exist !!!");
			return;
		}
		
		try {
			generate(opts, rb);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	private void generate(GenerateOptions opts, ResourceBundle rb) throws Exception {
		int optCount;
		int skelCount = Integer.parseInt(rb.getString(SKEL_PREF + "0"));
		String mod, outputFile, s;
		for (int i = 1; i <= skelCount; i++) {
			boolean gen = false;
			mod =  getString(rb, SKEL_PREF + i + TEMPLATE);
			s = getString(rb, SKEL_PREF + i + OPT + "0");
			if (s == null || s.length() == 0) {
				gen = true;
			} else {
				optCount = Integer.parseInt(s);
				for (int j = 1; j <= optCount; j++) {
					s = getString(rb, SKEL_PREF + i + OPT + j);
					if (opts.generateOptions.contains(s.toLowerCase())) {
						gen = true;
						break;
					}
				}
			}
			
			if (gen) {
				String genAt  =  getString(rb, SKEL_PREF + i + ".genAt");
				if (genAt != null && "record".equals(genAt.toLowerCase())) {
					for (RecordDef r : opts.schemaDefinition.getRecords()) {
						outputFile = expand(opts, r, opts.outputDir + "/" + getString(rb,  SKEL_PREF + i + OUTPUT_FILE));
						genSkel(mod, outputFile, opts, r);
					}
				} else {
					outputFile = expand(opts, null, opts.outputDir + "/" + getString(rb,  SKEL_PREF + i + OUTPUT_FILE));
					genSkel(mod, outputFile, opts, null);
				}
			}
		}
	}
	
	private String getString(ResourceBundle rb, String key) {
		if (rb.containsKey(key)) {
			return rb.getString(key);
		}
		return null;
	}
	
	private String expand(GenerateOptions opts, RecordDef r, String s) {
		StringBuilder b = new StringBuilder(s);
		Conversion.replace(b, "&suffix.", opts.schemaDefinition.getExtensionName() );
		Conversion.replace(b, "&directory.", opts.packageDir);
		if (r != null) {
			Conversion.replace(b, "&recordSuffix.", r.getExtensionName() );
		}
		return b.toString();
	}
	
    /**
     * Generate a velocity skelton
     *
     * @param templateFile file to be generated
     * @param writer output writer
     * @param context variable definitions
     *
     * @throws Exception any error that occurs
     */
    public final void genSkel(String templateFile, String outputFile, GenerateOptions opts,  RecordDef r )
    throws Exception {

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
		if (r != null) {
			context.put("currentRecord", r);
		}
		context.attachEventCartridge(ec);

//        VelocityEngine e = new VelocityEngine();
//      
//        e.init();
//        
//            //e.setProperty(Velocity.RESOURCE_LOADER, s1);
//            //templateFile = templateFile.substring(idx + 1);
//        //e.setProperty(Velocity.RESOURCE_LOADER, "classpath"); 
//        //e.setProperty("classpath.resource.loader.class", ClasspathResourceLoader.class.getName());
//
//        e.setProperty(Velocity.FILE_RESOURCE_LOADER_PATH, "classpath"); 
//
//        template = e.getTemplate(templateFile, "UTF8");
		
		Files.createDirectories(Paths.get(outputFile).getParent());
		Writer writer = new FileWriter(outputFile);

		System.out.println("Template: " + templateFile);
        Velocity.evaluate( context, writer, "log tag name", getTemplateFromResource(templateFile));
 
        /*
         *  Now have the template engine process your template using the
         *  data placed into the context.  Think of it as a  'merge'
         *  of the template and the data to produce the output stream.
         */

//
//        if (template != null) {
//            template.merge(context, writer);
//        }

        /*
         *  flush and cleanup
         */

        writer.flush();
        writer.close();
    }

    private String getTemplateFromResource(final String templatePath) {
    	StringBuilder b = new StringBuilder();
    	getTemplateFromResource(b, templatePath);
    	return b.toString();
    }

	/**
	 * @param templatePath
	 * @param b
	 * @throws RuntimeException
	 */
	private void getTemplateFromResource(StringBuilder b, final String templatePath) throws RuntimeException {
		try { 	
    		InputStream in = this.getClass().getResourceAsStream(templatePath);   	
    		BufferedReader r = new BufferedReader(new InputStreamReader(in));
    		String s;
    		while ((s = r.readLine()) != null) {
    			if (s.trim().startsWith("#incl(")) {
    				s = s.trim();
    				s = s.substring(6, s.length() -1).trim();
    				//System.out.print("~~>>" + s + "<<");
    				s = s.substring(1, s.length() - 1);
    				//System.out.println("\t~~>>" + s + "<<");
    				getTemplateFromResource(b, s);
    				
    			} else {
    				b.append(s).append('\n');
    			}
    		}
    	} catch (IOException ex) {   	
    		throw new RuntimeException(ex);

    	}
	}
}
