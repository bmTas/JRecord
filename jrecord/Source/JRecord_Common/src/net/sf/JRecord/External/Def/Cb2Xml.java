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

/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.External.Def;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Numeric.ConversionManager;
import net.sf.JRecord.Numeric.Convert;
import net.sf.cb2xml.CopyBookAnalyzer;
import net.sf.cb2xml.def.NumericDefinition;
import net.sf.cb2xml.sablecc.lexer.LexerException;
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
 */

public class Cb2Xml {


	/**
	 * Convert cobol file to XML~Dom
	 *
	 * @param file Cobol file
	 * @param log RecordEditor Error Log
	 *
	 * @return XML-Document
	 * @throws IOException 
	 * @throws LexerException 
	 * @throws ParserException 
	 */
	public static Document convertToXMLDOM(File file, int binaryFormat, boolean debug, int format, AbsSSLogger log) 
			throws ParserException, LexerException, IOException {

		Convert conv = ConversionManager.getInstance().getConverter4code(binaryFormat) ;

		
		CopyBookAnalyzer.setNumericDetails((NumericDefinition) conv.getNumericDefinition());
		return net.sf.cb2xml.Cb2Xml2.convertToXMLDOM(file, debug, format);
//		//log = TextLog.getLog(log);
//		
////		try {
//			Reader sr;
//			CopyBookAnalyzer.setNumericDetails((NumericDefinition) conv.getNumericDefinition());
//			
//			switch (format) {
//			case Cb2xmlConstants.USE_STANDARD_COLUMNS:
//			case Cb2xmlConstants.USE_COLS_6_TO_80:
//			case Cb2xmlConstants.USE_LONG_LINE:
//				preProcessed = CobolPreprocessor.preProcess(new FileInputStream(file), FIRST_COBOL_COLUMN, END_COLS[format]);
//				sr = new StringReader(preProcessed);
//				break;
//			case Cb2xmlConstants.FREE_FORMAT:
//				sr = new FileReader(file);
//				firstColumn = 0;
//				break;
////			case Cb2xmlConstants.USE_PROPERTIES_FILE:
//			default:
//				preProcessed = CobolPreprocessor.preProcess(new FileInputStream(file));
//				sr = new StringReader(preProcessed);
//			}
//			PushbackReader pbr = new PushbackReader(sr, 1000);
//			if (debug) {
//			    log.logMsg(AbsSSLogger.TESTING, "*** debug mode ***");
//				lexer = new DebugLexer(pbr);
//			} else {
//				lexer = new Lexer(pbr);
//			}
//			Parser parser = new Parser(lexer);
//			Start ast = parser.parse();
//			CopyBookAnalyzer copyBookAnalyzer = new CopyBookAnalyzer(file.getName(), parser);
//			ast.apply(copyBookAnalyzer);
//			document = copyBookAnalyzer.getDocument();
////		} catch (ParserException pe) {
////			pe.printStackTrace();
////		    log.logMsg(AbsSSLogger.ERROR, "*** fatal parse error ***");
////		    log.logMsg(AbsSSLogger.ERROR, pe.getMessage());
////			if (debug) {
////			    log.logMsg(AbsSSLogger.ERROR, "=== buffer dump start ===");
////			    log.logMsg(AbsSSLogger.ERROR, ((DebugLexer) lexer).getBuffer().toString());
////			    log.logMsg(AbsSSLogger.ERROR, "=== buffer dump end ===");
////			}
////		} catch (Exception e) {
////			e.printStackTrace();
////		}
//		return document;
	}

	public static Document convertToXMLDOM(InputStream is, String name,  int binaryFormat, boolean debug, int format) 
			throws ParserException, LexerException, IOException {
		
		Convert conv = ConversionManager.getInstance().getConverter4code(binaryFormat) ;
	
		CopyBookAnalyzer.setNumericDetails((NumericDefinition) conv.getNumericDefinition());
		return net.sf.cb2xml.Cb2Xml2.convertToXMLDOM(is, name, debug, format);
	}
	

	public static Document convertToXMLDOM(Reader reader, String name,  int binaryFormat, boolean debug, int format) 
			throws ParserException, LexerException, IOException {
		
		Convert conv = ConversionManager.getInstance().getConverter4code(binaryFormat) ;
	
		CopyBookAnalyzer.setNumericDetails((NumericDefinition) conv.getNumericDefinition());
		return net.sf.cb2xml.Cb2Xml2.convert(reader, name, debug, format);
	}

}