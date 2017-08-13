package net.sf.JRecord.cg.details;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import net.sf.JRecord.cg.details.jaxb.Option;
import net.sf.JRecord.cg.details.jaxb.Options;
import net.sf.JRecord.cg.details.jaxb.SkelGenDefinition;
import net.sf.JRecord.cg.details.jaxb.Skelton;
import net.sf.JRecord.cg.details.jaxb.Skeltons;



/**
 * Get the details related to the CodeGen Template
 * being used
 * 
 * @author Bruce Martin
 *
 */
public class TemplateDtls {
	
	private static final String SKELTON_XML_MULTI_RECORD = "SkeltonXmlMR";
	private static final String SKELTON_XML_PROPERTY = "SkeltonXml";
	private static final String OUTPUT_FILE = ".output";
	private static final String TEMPLATE = ".template";
	private static final String IF = ".if.";
	private static final String SKEL_PREF = "skel.";

	public static final String DEFAULT_TEMPLATE_BASE = "/net/sf/JRecord/cg/velocity/";
	public static final String GENERATE_PROPERTIES = "Generate.properties";
	public static final String T_REQUIRE_PACKAGE_ID = "requirePackageId";
	public static final String T_SPLIT_ALLOWED = "splitAllowed";
	public static final String T_DUPLICATE_FIELD_NAMES = "duplicateFieldNames";

	
	public final String template, templateDir, templateBase;
	private String language;
	public final Map<String, Object> generateOptions = new HashMap<String, Object>(10);
	public final Properties templateProperties;
	private boolean ok = true;
	private boolean inTemplateBase = false, multiRecord = false;
	
	private String currentDate, currentDateTime;
	
	private SkelGenDefinition skeltons;

	public TemplateDtls(String templateDir, String template, boolean multiRecord) {
		this(templateDir, template, DEFAULT_TEMPLATE_BASE, multiRecord);
	}

	public TemplateDtls(String templateDir, String template, String templateBase, boolean multiRecord) {
		
		this.multiRecord = multiRecord;
		
		String t = templateDir;
		boolean useTemplateDir = false;
		if ((! isEmpty(templateDir))) {
			char endChar = templateDir.charAt(templateDir.length() - 1);
			if (endChar != '/' && endChar != '\\') {
				t = t + '/';
			}
			templateDir = t;
			if (isEmpty(template) && (new File(t + GENERATE_PROPERTIES)).exists()) {
				File f = new File(t);
				template = f.getName();
				templateDir = f.getParent();
				useTemplateDir = true;
			} 
		}
		if (templateBase == null) {
			templateBase = DEFAULT_TEMPLATE_BASE;
		}
		this.templateBase = templateBase;
		this.templateDir = templateDir;
		this.template = template;
		templateProperties = getProperties(templateDir, useTemplateDir, template);
		loadOptions("Opts.");
	}
	
	private boolean isEmpty(String s) {
		return s == null || s.length() == 0;
	}
	
	
	private Properties getProperties(String dir, boolean useTemplateDir, String template)  {
		Properties p = new Properties();
		try {
			InputStream stream;
			String filePropertiesName = dir + template + '/' + GENERATE_PROPERTIES;
			if ((new File(filePropertiesName)).exists()) {
				stream = new FileInputStream(filePropertiesName );
			} else {
				stream = this.getClass().getResourceAsStream(templateBase + template + '/' + GENERATE_PROPERTIES);
				inTemplateBase = true;
			}
			if (stream == null) {
				System.out.println();
				System.out.println("Could not Load Template: " + template);
				System.out.println();
				throw new RuntimeException("Could not load properties file for Template:" + template);
			}
			p.load(stream);				
		} catch (IOException e) {
			ok = false;
			System.out.println();
			System.out.println("Could not Load Template: " + template);
			System.out.println(e);
			System.out.println();
		}
		return p;
	}
	
	/**
	 * Get the Template description Html 
	 * (displayed in the RecordEditor when generating a template).
	 * 
	 * @return
	 */
	public final String getDescriptionHtml() {
		InputStream stream;
		StringBuffer html = new StringBuffer();
		String descHtml = "Description.html";

		try {
			if (inTemplateBase) {
				stream = this.getClass().getResourceAsStream(templateBase + template + '/' + descHtml);
			} else {
				stream = new FileInputStream(templateDir + template + '/' + descHtml);
			}

			if (stream != null) {
				BufferedReader r = new BufferedReader(new InputStreamReader(stream));
				String line;
				while ((line = r.readLine()) != null) {
					html.append(line).append('\n');
				}
				r.close();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		return html.toString();
	}

	/**
	 * Check if Template has an option set.
	 */
	public boolean hasOption(String opt) {
		return ! "N".equals(templateProperties.get(opt));
	}
	

	/**
	 * Check if Template has an option set.
	 */
	public boolean hasOption(String opt, boolean defaultVal) {
		if (defaultVal) {
			return ! "N".equals(templateProperties.get(opt));
		} else {
			return "Y".equals(templateProperties.get(opt));
		}
	}

	/**
	 * @param arrayKey
	 */
	public void loadOptions(String arrayKey) {
		String countStr = templateProperties.getProperty(arrayKey + '0');
		
		if (countStr != null && countStr.length() > 0) {
			try {
				int count = Integer.parseInt(countStr);
				for (int i = 1; i <= count; i++) {
					String key = arrayKey + i;
					String k = templateProperties.getProperty(key);
					String v = templateProperties.getProperty(key + ".val");
					if (v == null) {
						v = "true";
					}
					if (k != null) {
						generateOptions.put(k, v);
					}
				}
			} catch (NumberFormatException e) {
			}
		}
	}
	
	public void loadDefaultOptions() {
		loadOptions("defaultOpts.");		
	}


	/**
	 * @return the template
	 */
	public final String getTemplate() {
		return template;
	}


	/**
	 * @return the templateDir
	 */
	protected final String getTemplateDir() {
		return templateDir;
	}

	public String getLanguage() {
		if (language == null) {
			SkelGenDefinition tempSkels = skeltons;
			String lang = "unknown";
			
				if (tempSkels == null) {
					String xml;
					if ((xml = getString(templateProperties, SKELTON_XML_PROPERTY )) != null) {
						try {
							tempSkels = loadSkelsFromXml(xml);
						} catch (JAXBException e) {
							return lang;
						}
					} else {
						tempSkels = loadSkeltonsFromProperties();
					}
				}
				String s = "";
				List<Skelton> skelList = tempSkels.getLayoutSkeltons();
				int pos;
				
				if (skelList == null || skelList.size() == 0) {
					skelList = tempSkels.getRecordSkeltons();
				}
				if (skelList != null && skelList.size() > 0) {
					s = skelList.get(0).getOutput();
				}
				if (! isEmpty(s)) {
					s = s.toLowerCase();
					if (s.endsWith(".java")) {
						lang = "Java";
					} else if (s.endsWith(".py")) {
						lang = "Python";
					} else if (s.endsWith(".rb")) {
						lang = "Ruby";
					} else if (s.endsWith(".jjs") || s.endsWith(".javascript")) {
						lang = "JavaScript";
					} else if ((pos = s.lastIndexOf('.')) >= 0) {
						lang = s.substring(pos + 1);
					}
				}
			language = lang;
		}
		return language;
	}

	/**
	 * Get the Template options
	 * @return Template options
	 */
	public final Map<String, Object> getGenerateOptions() {
		return generateOptions;
	}

	/**
	 * Get a specific Template option
	 * @param key
	 * @return
	 */
	public final Object getGenerateOption(String key) {
		return generateOptions.get(key);
	}



	/**
	 * @return the ok
	 */
	protected final boolean isOk() {
		return ok;
	}

	

	public final String getCurrentDate() {
		setupDate();
		return currentDate;
	}
	

	public final String getCurrentDateTime() {
		setupDate();
		return currentDateTime;
	}

	public boolean isMultiRecord() {
		return multiRecord;
	}

	public void setMultiRecord(boolean multiRecord) {
		this.multiRecord = multiRecord;
	}

	public SkelGenDefinition getSkeltons() throws JAXBException {
		if (skeltons == null) {
			String xml;
			if (multiRecord && (xml = getString(templateProperties, SKELTON_XML_MULTI_RECORD )) != null) {
				skeltons = loadSkelsFromXml(xml);
			} else if ((xml = getString(templateProperties, SKELTON_XML_PROPERTY )) != null) {
				skeltons = loadSkelsFromXml(xml);
			} else {
				skeltons = loadSkeltonsFromProperties();
			}
			loadOptionsWithSkeltons(skeltons.getLayoutSkeltons());
			loadOptionsWithSkeltons(skeltons.getRecordSkeltons());
		}
		return skeltons;
	}
	
	private void loadOptionsWithSkeltons(List<Skelton> skeltons) {
		if (skeltons != null) {
			for (Skelton skel : skeltons) {
				generateOptions.put("skel=" + skel.getTemplateFileName(), true);
			}
		}
	}
	
	private SkelGenDefinition loadSkelsFromXml(String xmlFile) throws JAXBException {
        JAXBContext jc = JAXBContext
        		.newInstance(SkelGenDefinition.class, Skeltons.class, Skelton.class, Options.class, Option.class);
        
        Unmarshaller unmarshaller = jc.createUnmarshaller();
        String resourceName = templateBase + template + "/" + xmlFile;
        System.out.println("Xml resource name: " + resourceName);
		//InputStream xmlStream = this.getClass().getResourceAsStream(resourceName);
        URL resource = this.getClass().getResource(resourceName);
        System.out.println("Xml resource: " + resource + " ");
        SkelGenDefinition skelDefs = (SkelGenDefinition) unmarshaller.unmarshal(resource); 
        
        updateSkelDetails(skelDefs.getLayoutSkeltons());
        updateSkelDetails(skelDefs.getRecordSkeltons());
        
        List<Option> options = skelDefs.getOptions();
        for (Option option : options) {
        	Object o = Boolean.TRUE;
        	String value = option.getValue();
			if (value != null && value.length() > 0) {
        		o = value;
        	}
			generateOptions.put(option.getId(), o);
        }
        
        return skelDefs;
	}
	
	private void updateSkelDetails(List<Skelton> skels) {
		if (skels != null) {
			for (Skelton skel : skels) {
				skel.update(templateProperties, template, true);
			}
		}
	}

	private SkelGenDefinition loadSkeltonsFromProperties() {
		int optCount;
		String skelPref = SKEL_PREF;		 
		
		int skelCount = Integer.parseInt(templateProperties.getProperty(skelPref + "0"));
		List<Skelton> skelton = new ArrayList<Skelton>();
		List<Skelton> recordSkelton = new ArrayList<Skelton>();
		
		String  s;
		for (int i = 1; i <= skelCount; i++) {
			boolean gen = false;

			s = getString(templateProperties, skelPref + i + IF + "0");
			if (s == null || s.length() == 0) {
				gen = true;
			} else {
				optCount = Integer.parseInt(s);
				for (int j = 1; j <= optCount; j++) { 
					s = getString(templateProperties, skelPref + i + IF + j);
					if (getGenerateOptions().containsKey(s.toLowerCase())) {
						gen = true;
						break;
					}
				}
			}
			
			String velocityTemplate = getString(templateProperties, skelPref + i + TEMPLATE);
			String genAt  =  getString(templateProperties, skelPref + i + ".genAt");
			String outputFileName = getString(templateProperties,  skelPref + i + OUTPUT_FILE);
			String description = getString(templateProperties,  skelPref + i + ".description");

			Skelton skel = new Skelton(templateProperties, template, velocityTemplate, description, outputFileName, gen);
			if (genAt != null && "record".equals(genAt.toLowerCase())) {
				recordSkelton.add(skel);
			} else {
				skelton.add(skel);
			}

		}
		
		return new SkelGenDefinition(skelton, recordSkelton);
	}

	private String getString(Properties rb, String key) {
		if (rb.containsKey(key)) {
			return rb.getProperty(key);
		}
		return null;
	}
	
	private void setupDate() {
		if (currentDate == null) {
			Date date = new Date();
			currentDateTime = new SimpleDateFormat("d MMM yyyy H:m:s").format(date);
			currentDate = new SimpleDateFormat("d MMM yyyy").format(date);
		}
	}
}
