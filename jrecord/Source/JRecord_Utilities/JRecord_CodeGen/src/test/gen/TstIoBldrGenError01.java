package test.gen;

import java.io.IOException;

import javax.xml.bind.JAXBException;

import net.sf.JRecord.cg.Generate;
import net.sf.JRecord.cg.details.ArgumentOption;

public class TstIoBldrGenError01 {

	public static void main(String[] args) throws IOException, JAXBException {

		String[] arguments1 = {
				ArgumentOption.OPT_TEMPLATE, "error",
				ArgumentOption.OPT_SCHEMA, TstIoBldrGenError01.class.getResource("DTAR020.cbl").getFile(),
				ArgumentOption.OPT_FILE_ORGANISATION, "FixedWidth",
				ArgumentOption.OPT_FONT_NAME, "CP037",
				ArgumentOption.OPT_DROP_COPYBOOK_NAME, "true",
				ArgumentOption.OPT_OUTPUT_DIR, "G:/Temp/Gen/error"
		};
		
		Generate.main(arguments1);
	}

}
