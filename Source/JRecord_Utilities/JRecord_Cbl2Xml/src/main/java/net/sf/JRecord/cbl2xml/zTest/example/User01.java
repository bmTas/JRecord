package net.sf.JRecord.cbl2xml.zTest.example;

import java.io.IOException;

import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cbl2xml.Cobol2Xml;
import net.sf.cb2xml.def.Cb2xmlConstants;

public class User01 {

	public static void main(String[] args) throws IOException, XMLStreamException {
		Cobol2Xml.newCobol2Xml(User01.class.getResource("user01.cbl").getFile())
	        .setDialect(ICopybookDialects.FMT_MAINFRAME)
	        .setFont("")
	        .setFileOrganization(Constants.IO_BIN_TEXT)
			.setCopybookFileFormat(Cb2xmlConstants.FREE_FORMAT)
	        .cobol2xml(User01.class.getResource("user01.txt").getFile(), 
	                "/Volumes/BruceMacHD/Temp/UserOut/user01.xml");
	}

}
