package net.sf.JRecord.cg.details;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

//import javax.xml.bind.JAXBContext;
//import javax.xml.bind.JAXBException;
//import javax.xml.bind.Unmarshaller;

import net.sf.JRecord.cg.details.xml.Option;
import net.sf.JRecord.cg.details.xml.ReadSkelGenDefinition;
import net.sf.JRecord.cg.details.xml.SkelGenDefinition;
import net.sf.JRecord.cg.details.xml.Skelton;




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

	public static final int DEFAULT_JREC_VERSION = 815;
	public static final String DEFAULT_TEMPLATE_BASE = "/net/sf/JRecord/cg/velocity/";
	public static final String GENERATE_PROPERTIES = "Generate.properties";
	public static final String T_REQUIRE_PACKAGE_ID = "requirePackageId";
	public static final String T_SPLIT_ALLOWED = "splitAllowed";
	public static final String T_DUPLICATE_FIELD_NAMES = "duplicateFieldNames";
	public static final String T_UPDATE_TYPES = "UpdateTypes";
	
	public static final String JRECORD_VERION_FLAG = "checkJRecordVersion";

	public final boolean useTemplateDir;
	public final String template, templateDir, templateBase;
	private String language;
	public final Map<String, Object> generateOptions = new HashMap<String, Object>(10);
	public final Properties templateProperties;
	private boolean ok = true;
	private boolean inTemplateBase = false, multiRecord = false;
	
	public final int jrecordVersion;
	
	private String currentDate, currentDateTime;
	
	private SkelGenDefinition skeltons;

	public TemplateDtls(String templateDir, String template, boolean multiRecord, int jrecordVersion) {
		this(templateDir, template, DEFAULT_TEMPLATE_BASE, multiRecord, jrecordVersion, null);
	}

	public TemplateDtls(String templateDir, String template, String templateBase, 
			boolean multiRecord, int jrecordVersion, String genDate) {
		
		this.multiRecord = multiRecord;
		this.jrecordVersion = jrecordVersion;
		
		boolean useTemplateDir = false;
		if ((! isEmpty(templateDir))) {
			String t = addDirChar(templateDir);
			templateDir = t;
			if (isEmpty(template)) {
				if ((new File(t + GENERATE_PROPERTIES)).exists()) {
					File f = new File(t);
					template = f.getName();
					templateDir = f.getParent();
					useTemplateDir = true;
				}
			} else  {
				useTemplateDir = (new File(t + template + '/' + GENERATE_PROPERTIES)).exists();
			}
		}
		this.useTemplateDir = useTemplateDir;
		if (templateBase == null) {
			templateBase = DEFAULT_TEMPLATE_BASE;
		}
		this.templateBase = templateBase;
		this.templateDir = templateDir;
		this.template = template;
		templateProperties = getProperties(templateDir, useTemplateDir, template);
		loadOptions("Opts.");
		
		if (! isEmpty(genDate)) {
			currentDate = genDate;
			currentDateTime = genDate + " 0:0:0";
		}
	}

	private String addDirChar(String t) {
		if (t != null && t.length() > 1) {
			char endChar = t.charAt(t.length() - 1);
			if (endChar != '/' && endChar != '\\') {
				t = t + '/';
			}
		}
		return t;
	}
	
	private boolean isEmpty(String s) {
		return s == null || s.length() == 0;
	}
	
	
	private Properties getProperties(String dir, boolean useTemplateDir, String template)  {
		Properties p = new Properties();
		try {
			InputStream stream;
			String filePropertiesName, name;
			if (useTemplateDir && (new File(filePropertiesName = addDirChar(dir) + template + '/' + GENERATE_PROPERTIES)).exists()) {
				stream = new FileInputStream(filePropertiesName );
				name = "File: " + filePropertiesName;
			} else {
				String resoueceName = templateBase + template + '/' + GENERATE_PROPERTIES;
				name = "Resource: " + resoueceName;
				stream = TemplateDtls.class.getResourceAsStream(resoueceName);
				//stream = this.getClass().getResourceAsStream(templateBase + template + '/' + GENERATE_PROPERTIES);
				inTemplateBase = true;
			}
			if (stream == null) {
				System.out.println();
				System.out.println("         useTemplateDir: " + useTemplateDir + " " + dir);
				System.out.println("Could not Load Template: " + template + " " + name);
				System.out.println();
				throw new RuntimeException("Could not load properties file for Template:" + template);
			}
			p.load(stream);	
			stream.close();
		} catch (IOException e) {
			ok = false;
			System.out.println();
			System.out.println("Could not Load Template: " + template);
			System.out.println(e);
			System.out.println();
		}
		return p;
	}
	
	
	
	public static Properties getTemplateProperties(String dir, boolean useTemplateDir,String template)
	throws IOException  {
		InputStream stream; 
		
		String filePropertiesName;
		if (useTemplateDir && (new File(filePropertiesName = dir + template + '/' + GENERATE_PROPERTIES)).exists()) {
			stream = new FileInputStream(filePropertiesName );
		} else {
			stream = TemplateDtls.class.getResourceAsStream(DEFAULT_TEMPLATE_BASE + template + '/' + GENERATE_PROPERTIES);
		}
		Properties p = new Properties();

		if (stream != null) {
			p.load(stream);	
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
						} catch (XMLStreamException e) {
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

	public SkelGenDefinition getSkeltons() throws XMLStreamException, FactoryConfigurationError  {
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
				String s = skel.getTemplateFileName();
				if (s.startsWith("$std.")) {
					s = s.substring(5);
				}
				generateOptions.put("skel=" + s, Boolean.TRUE);
			}
		}
	}
	
	private SkelGenDefinition loadSkelsFromXml(String xmlFile) throws XMLStreamException, FactoryConfigurationError {
//        JAXBContext jc = JAXBContext
//        		.newInstance(SkelGenDefinition.class, Skeltons.class, Skelton.class, Options.class, Option.class);
//        
//        Unmarshaller unmarshaller = jc.createUnmarshaller();
        SkelGenDefinition skelDefs;
        ReadSkelGenDefinition readSkel;
    	InputStream is = null;
    	try {
	        if (useTemplateDir) {
				is = new FileInputStream(templateDir + '/' + template + "/" + xmlFile);
	        } else {
		        String resourceName = templateBase + template + "/" + xmlFile;
		        //System.out.println("Xml resource name: " + resourceName);
				//InputStream xmlStream = this.getClass().getResourceAsStream(resourceName);
		       is = this.getClass().getResource(resourceName).openStream();
	        }
        
			XMLStreamReader xmlStream = XMLInputFactory.newInstance().createXMLStreamReader(is);
			readSkel =new ReadSkelGenDefinition(xmlStream);
			skelDefs = readSkel.readXml();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
			throw new RuntimeException(e);
		} catch (IOException e) {
			e.printStackTrace();
			throw new RuntimeException(e);
		} finally {
			if (is != null) {
				try {
					is.close();
				} catch (IOException e) {
				}
			}
		}

        updateSkelDetails(skelDefs.getLayoutSkeltons());
        updateSkelDetails(skelDefs.getRecordSkeltons());
        
        List<Option> options = skelDefs.getOptions().getOption();
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

	/**
	 * @return the jrecordVersion
	 */
	public int getJrecordVersion() {
		return jrecordVersion;
	}
}
