package test.gen;

import java.io.File;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import net.sf.JRecord.cg.details.TemplateDtls;
import net.sf.JRecord.cg.details.jaxb.Option;
import net.sf.JRecord.cg.details.jaxb.Options;
import net.sf.JRecord.cg.details.jaxb.SkelGenDefinition;
import net.sf.JRecord.cg.details.jaxb.Skelton;
import net.sf.JRecord.cg.details.jaxb.Skeltons;


/**
 * This program converts CodeGen Skelton list to Xml
 * 
 * @author Bruce Martin
 *
 */
public class ACnvSkelListToXml {

	
	public static void toXml(Marshaller marshaller , String template) throws JAXBException {
		TemplateDtls t = new TemplateDtls(null, template, false, TemplateDtls.DEFAULT_JREC_VERSION);
			
		marshaller.marshal(t.getSkeltons(), new File("/home/bruce/" + template + ".xml"));
	}
	public static void reToXml(Marshaller marshaller , String template) throws JAXBException {
		TemplateDtls t = new TemplateDtls(
				"/home/bruce/eclipse-workspace2/RecordEditor/src/net/sf/RecordEditor/cg/velocity/" + template + "/",
				null, 
				true, TemplateDtls.DEFAULT_JREC_VERSION);
			
		marshaller.marshal(t.getSkeltons(), new File("/home/bruce/work/temp/" + template + ".xml"));
	}

	public static void main(String[] args) throws JAXBException {
		
		
		JAXBContext jc = JAXBContext.newInstance(
				SkelGenDefinition.class, Skeltons.class, Skelton.class, Options.class, Option.class);
		Marshaller marshaller = jc.createMarshaller();
		marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);

//		toXml(marshaller, "pojo");
//		toXml(marshaller, "standard");
//		toXml(marshaller, "lineWrapper");
//		toXml(marshaller, "stdPojo");
		
		reToXml(marshaller, "javaJRecPojo");
		reToXml(marshaller, "javaJRecWrapper");
		reToXml(marshaller, "javaJRec");
	}

}
