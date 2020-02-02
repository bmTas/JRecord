package test.gen;

import java.io.IOException;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.cg.Generate;
import net.sf.JRecord.cg.details.ArgumentOption;

public class TstPojoGen02 {

	public static void main(String[] args) throws IOException, XMLStreamException, FactoryConfigurationError {

//		String[] arguments1 = {
//				ArgNames.OPT_TEMPLATE, "javaPojo1",
//				ArgNames.OPT_PACKAGE, "dtar020.pojo",
//				ArgNames.OPT_SCHEMA, TstPojoGen02.class.getResource("DTAR020.cbl").getFile(),
//				ArgNames.OPT_FILE_ORGANISATION, "FixedWidth",
//				ArgNames.OPT_FONT_NAME, "CP037",
//				ArgNames.OPT_DROP_COPYBOOK_NAME, "true",
//				ArgNames.OPT_OUTPUT_DIR, "G:/Temp/Gen/Dtar020Pojo"
//		};
//		
//		Generate.main(arguments1);

		String[] arguments2 = {
				ArgumentOption.OPT_TEMPLATE, "javaPojo1",
				ArgumentOption.OPT_PACKAGE, "poDownload.pojo",
				ArgumentOption.OPT_SCHEMA, TstPojoGen02.class.getResource("ams_PO_Download.Xml").getFile(),
				ArgumentOption.OPT_FILE_ORGANISATION, "Text",
//				ArgNames.OPT_SPLIT, "01"
				ArgumentOption.OPT_OUTPUT_DIR, "G:/Temp/Gen/PoDownloadPojo"
		};
		
		Generate.main(arguments2);
	}

}
