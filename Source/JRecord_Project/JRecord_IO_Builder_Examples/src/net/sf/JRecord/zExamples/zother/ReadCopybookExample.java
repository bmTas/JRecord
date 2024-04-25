package net.sf.JRecord.zExamples.zother;

import java.io.IOException;

import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.cb2xml.copybookReader.ReadCobolCopybook;
import net.sf.cb2xml.sablecc.lexer.LexerException;
import net.sf.cb2xml.sablecc.parser.ParserException;

public class ReadCopybookExample {

	public static void main(String[] args) throws XMLStreamException, LexerException, IOException, ParserException {
		ReadCobolCopybook copybook = JRecordInterface1.COBOL.newCobolCopybookReader()
				.addCobolCopybook("Copbook_File_Name")
				.addFreeFormatCobolText("" +
						" 05 RECORD-Name        PIC X.\n" + 
						"    88 REC_Val         VALUE \"1\".\n");
		
		ICobolIOBuilder iob = JRecordInterface1.COBOL.newIOBuilder(copybook);
	}

}
