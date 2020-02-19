package  net.sf.JRecord.cg.details.xml;

import java.util.Properties;

import net.sf.JRecord.Common.Conversion;

public class  Skelton implements  ISkelton {

    String description;
    String output;
    String template;

    private String packageExtension, codeGenTemplate;
    public boolean generate = true; 
    private Properties templateProperties;

    public Skelton() {
    	
    }
    
	public Skelton(Properties templateProperties,
			String codeGenTemplate, String template, String description, String outputFileName, boolean generate) {
		super();
//		this.fileName = fileName;
		this.templateProperties = templateProperties;
		this.codeGenTemplate = codeGenTemplate;
		this.template = template;
		this.description = description;
		this.output = outputFileName;
		this.generate = generate;
	}
	
	public void update(Properties templateProperties, String codeGenTemplate, boolean generate) {
		this.templateProperties = templateProperties;
		this.codeGenTemplate = codeGenTemplate;
		this.generate = generate;		
	}

	
	public String getPackageExtension() {
		if (this.packageExtension == null) {
			int st = output.indexOf('/');
			int en = output.lastIndexOf('/');
			String extension = "";
			if (st >= 0 && en > st) {
				StringBuilder b = new StringBuilder()
										.append('.')
										.append(output.substring(st+1, en));
			    extension = Conversion.replace(b, "/", ".").toString();
			}

			this.packageExtension = extension;
		}
		
		return this.packageExtension;
	}
	
	public String getTemplateFileName() {
		return Conversion.replace(
				template,
				"&template.", 
				codeGenTemplate
		).toString();
	}


    public String getDescription() {
    	return  description;
    }
    public String getOutput() {
    	return  output;
    }
    public String getTemplate() {
    	return  template;
    }


}
