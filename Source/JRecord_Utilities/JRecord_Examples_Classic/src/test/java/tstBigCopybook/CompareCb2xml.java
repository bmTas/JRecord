package tstBigCopybook;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.List;

import javax.xml.stream.XMLStreamException;

import org.w3c.dom.Document;

import net.sf.cb2xml.Cb2Xml2;
import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.IItem;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;

public class CompareCb2xml {

	private static final int EXECUTE_COUNT = 1000;
	String cpybook;
	public CompareCb2xml() throws IOException, ParserException, LexerException, XMLStreamException {
		readCopybook();
		
		readViaBld();
		System.out.println("Read via bld");
		readViaOldInterface();
		System.out.println("Read via old");
		readViaBld();
		System.out.println("Read via bld");
		readViaOldInterface();
		System.out.println("Read via old");
		System.out.println();
		long cpu;
		
		cpu = System.currentTimeMillis();
		readViaOldInterface();
		long oldTime = System.currentTimeMillis() - cpu;
		cpu = System.currentTimeMillis();
		readViaBld();
		long bldrTime = System.currentTimeMillis() - cpu;
		System.out.println("Read via bld: " + bldrTime);
		System.out.println("Read via bld: " + bldrTime);
		System.out.println("Read via old: " + oldTime);

	}
	
	void readCopybook() throws IOException {
		BufferedReader r = new BufferedReader(new FileReader("/media/sf_Shared/LargeCopybook.cbl"));
		StringBuilder b = new StringBuilder(256000);
		String l;
		
		while((l = r.readLine()) != null) {
			b.append(l).append('\n');
		}
		r.close();
		
		cpybook = b.toString();
		cpybook = cpybook + cpybook + cpybook + cpybook + cpybook; 
	}
	
	void readViaBld() {
		
		for (int i = 0; i< EXECUTE_COUNT; i++) {
			List<? extends IItem> itemTree 
					= Cb2Xml3.newBuilder(new StringReader(cpybook), "LargeCopybook")
							 .setCobolLineFormat(Cb2xmlConstants.USE_STANDARD_COLUMNS)
							 .setLoadComments(false)
							 .setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat.CLASSIC)
							 .asCobolItemTree()
							 	.getChildItems();
		}
	}
	
	void readViaOldInterface() throws ParserException, LexerException, IOException, XMLStreamException {
		for (int i = 0; i< EXECUTE_COUNT; i++) {
			 Document convertToXMLDOM = Cb2Xml2.convert(new StringReader(cpybook), "LargeCopybook", false, Cb2xmlConstants.USE_STANDARD_COLUMNS);
		}
	}
	
	public static void main(String[] args)  throws IOException, ParserException, LexerException, XMLStreamException {
		new CompareCb2xml();
	}

}
