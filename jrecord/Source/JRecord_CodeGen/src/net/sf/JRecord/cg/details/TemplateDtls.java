package net.sf.JRecord.cg.details;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

public class TemplateDtls {
	
	public static final String DEFAULT_TEMPLATE_BASE = "/net/sf/JRecord/cg/velocity/";
	public static final String GENERATE_PROPERTIES = "Generate.properties";
	public static final String T_REQUIRE_PACKAGE_ID = "requirePackageId";
	public static final String T_SPLIT_ALLOWED = "splitAllowed";

	
	public final String template, templateDir, templateBase, language;
	public final Map<String, Object> generateOptions = new HashMap<String, Object>(10);
	public final Properties templateProperties;
	private boolean ok = true;
	private boolean inTemplateBase = false;
	
	private String currentDate, currentDateTime;

	public TemplateDtls(String templateDir, String template) {
		this(templateDir, template, DEFAULT_TEMPLATE_BASE);
	}

	public TemplateDtls(String templateDir, String template, String templateBase) {
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
		
		String s = templateProperties.getProperty("skel.1.output");
		String l = "unknown";
		int pos;
		if (! isEmpty(s)) {
			s = s.toLowerCase();
			if (s.endsWith(".java")) {
				l = "Java";
			} else if (s.endsWith(".py")) {
				l = "Python";
			} else if (s.endsWith(".rb")) {
				l = "Ruby";
			} else if (s.endsWith(".jjs") || s.endsWith(".javascript")) {
				l = "JavaScript";
			} else if ((pos = s.lastIndexOf('.')) >= 0) {
				l = s.substring(pos + 1);
			}
		}
		language = l;
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

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cg.details.IGenerateOptions#hasOption(java.lang.String)
	 */
	public boolean hasOption(String opt) {
		return ! "N".equals(templateProperties.get(opt));
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

	public final Map<String, Object> getGenerateOptions() {
		return generateOptions;
	}

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

	private void setupDate() {
		if (currentDate == null) {
			Date date = new Date();
			currentDateTime = new SimpleDateFormat("d MMM yyyy H:m:s").format(date);
			currentDate = new SimpleDateFormat("d MMM yyyy").format(date);
		}
	}
}
