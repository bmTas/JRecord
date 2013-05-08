/*************************************************************
 * This file is part of RecordEditor.
 * See the file "LICENSE" for copyright information and the
 * terms and conditions for copying, distribution and
 * modification of RecordEditor.
 *************************************************************
 * The original file from CB2XML was modified to make use of
 * some logging facility in Record-Editor
 * This new file is now part of the Record-Editor package
 * It makes use of the CB2XML packages (changes done by Bruce Martin)
 * The class was moved in the Record-Editor project by Jean-Francois Gagnon
 */

package net.sf.JRecord.External.Def;

import java.io.File;
import java.io.PushbackReader;
import java.io.StringReader;

import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Numeric.ConversionManager;
import net.sf.JRecord.Numeric.Convert;
import net.sf.cb2xml.CobolPreprocessor;
import net.sf.cb2xml.CopyBookAnalyzer;
import net.sf.cb2xml.DebugLexer;
import net.sf.cb2xml.def.NumericDefinition;
import net.sf.cb2xml.sablecc.lexer.Lexer;
import net.sf.cb2xml.sablecc.node.Start;
import net.sf.cb2xml.sablecc.parser.Parser;
import net.sf.cb2xml.sablecc.parser.ParserException;

import org.w3c.dom.Document;


/**
 * This is interface to the Cb2Xml package.
 *
 * main utility for parsing a Cobol copybook into XML.
 *
 * It calls calls pre-processor, then parser to perform parse
 * note: use the debug mode to view detailed SableCC debug output
 *
 * @author Peter Thomas
 */

public class Cb2Xml {
//    /**
//     * convert a Cobol copybook to XML
//     * @param args Cobol Copybbok name
//     */
//	public static void main(String[] args) {
//		File file = new File(args[0]);
//		boolean debug = false;
//		if (args.length > 1) {
//			debug = true;
//		}
//		Document document = convert(file, debug, new TextLog());
//		String result = XmlUtils.domToString(document).toString();
//
//		writeFile(result, file.getName() + ".xml", false);
//	}

	/**
	 * Convert cobol file to XML~Dom
	 *
	 * @param file Cobol file
	 * @param log RecordEditor Error Log
	 *
	 * @return XML-Document
	 */
	public static Document convertToXMLDOM(File file, int binaryFormat, AbsSSLogger log) {
		return convert(file, binaryFormat, false, log);
	}

	// overloaded methods for debug mode
	/*public static Document convertToXMLDOM(File file, boolean debug, AbsSSLogger log) {
		return convert(file, debug, log);
	}*/

/*
	public static String convertToXMLString(File file) {
		Document document = convert(file, false);
		return XmlUtils.domToString(document).toString();
	}


	public static String convertToXMLString(File file, boolean debug) {
		Document document = convert(file, debug);
		return XmlUtils.domToString(document).toString();
	}
*/
	/**
	 * Convert a cobol file to a XML document
	 */
	private static Document convert(File file, int binaryFormat, boolean debug, AbsSSLogger log) {
		Document document = null;
		Lexer lexer = null;
		String preProcessed = null;
		Convert conv = ConversionManager.getInstance().getConverter4code(binaryFormat) ;
		try {
			CopyBookAnalyzer.setNumericDetails((NumericDefinition) conv.getNumericDefinition());
			preProcessed = CobolPreprocessor.preProcess(file);
			StringReader sr = new StringReader(preProcessed);
			PushbackReader pbr = new PushbackReader(sr, 1000);
			if (debug) {
			    log.logMsg(AbsSSLogger.TESTING, "*** debug mode ***");
				lexer = new DebugLexer(pbr);
			} else {
				lexer = new Lexer(pbr);
			}
			Parser parser = new Parser(lexer);
			Start ast = parser.parse();
			CopyBookAnalyzer copyBookAnalyzer = new CopyBookAnalyzer(file.getName(), parser);
			ast.apply(copyBookAnalyzer);
			document = copyBookAnalyzer.getDocument();
		} catch (ParserException pe) {
			pe.printStackTrace();
		    log.logMsg(AbsSSLogger.ERROR, "*** fatal parse error ***");
		    log.logMsg(AbsSSLogger.ERROR, pe.getMessage());
			if (debug) {
			    log.logMsg(AbsSSLogger.ERROR, "=== buffer dump start ===");
			    log.logMsg(AbsSSLogger.ERROR, ((DebugLexer) lexer).getBuffer().toString());
			    log.logMsg(AbsSSLogger.ERROR, "=== buffer dump end ===");
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return document;
	}


//	/**
//	 * Write String to file
//	 * @param content contents to be written to a file
//	 * @param fileName file to be written
//	 * @param append wether to append to the file
//	 */
//	private static void writeFile(String content, String fileName, boolean append) {
//	    FileWriter writer = null;
//	    try {
//	        writer = new FileWriter(fileName, append);
//	        writer.write(content);
//	    } catch (Exception e) {
//	        e.printStackTrace();
//	    } finally {
//	        if (writer != null) {
//	            try {
//	                writer.close();
//	            } catch (Exception e) { }
//	        }
//	    }
//	}
}