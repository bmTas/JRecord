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
import java.io.InputStreamReader;
import java.io.Reader;

import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Numeric.ConversionManager;
import net.sf.JRecord.Numeric.Convert;
import net.sf.cb2xml.ICb2XmlBuilder;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.analysis.Copybook;
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
	
	public static final int USE_DEFAULT_THREADSIZE = Cb2xmlConstants.USE_DEFAULT_THREADSIZE;
	public static final int CALCULATE_THREAD_SIZE  = Cb2xmlConstants.CALCULATE_THREAD_SIZE;


//	private static final String SYNC = "xx123xx";  

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
	 * @throws XMLStreamException 
	 */
	public static Document convertToXMLDOM(File file, int cobolDialect, boolean debug, int format, AbsSSLogger log) 
			throws ParserException, LexerException, IOException, XMLStreamException {
		return convertToXMLDOM(
				net.sf.cb2xml.Cb2Xml3
					.newBuilder(file),
				cobolDialect, debug, format);
	
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

	/**
	 * 
	 * @param is Cobol Copybook stream
	 * @param name Copybook Name
	 * @param cobolDialect COBOL dialect<ul>
	 *   <li><b>ICopybookDialects.FMT_MAINFRAME</b> - Mainframe Cobol
	 *   <li><b>ICopybookDialects.FMT_FUJITSU</b> - Written for the old Fujitsu Cobol 3 compiler
	 *   <li><b>ICopybookDialects.FMT_GNU_COBOL</b> - GNU Cobol (formerly Open Cobol) on a Little Endian machine (e.g Intel).
	 *   <li><b>ICopybookDialects.FMT_OC_MICRO_FOCUS_BE</b> -  GNU Cobol running in Microfocus compatibility mode on a Big Endian machine
	 * </ul>
	 * @param debug
	 * @param formatFormat of the Cobol Copybook:<ul>
	 *    <li>Cb2xmlConstants.FREE_FORMAT Free format copybook
	 *    <li>Cb2xmlConstants.USE_STANDARD_COLUMNS - Standard Cobol Columns
	 *    <li>Cb2xmlConstants.USE_COLS_6_TO_80 - use columns 6 -> 80
	 *    <li>Cb2xmlConstants.USE_LONG_LINE - Long line starting at column 6
	 *  </ul>
	 * @return
	 * @throws ParserException
	 * @throws LexerException
	 * @throws IOException
	 * @throws XMLStreamException
	 */
	public static Document convertToXMLDOM(
			InputStream is, String name,  int cobolDialect, 
			boolean debug, int copybookFormat) 
			throws ParserException, LexerException, IOException, XMLStreamException {
		
		return convertToXMLDOM(
				net.sf.cb2xml.Cb2Xml3
					.newBuilder(new InputStreamReader(is), name),
				cobolDialect, debug, copybookFormat);
	}
	
	/**
	 * 
	 * @param reader  Cobol Copybook
	 * @param name
	 * @param cobolDialect COBOL dialect<ul>
	 *   <li><b>ICopybookDialects.FMT_MAINFRAME</b> - Mainframe Cobol
	 *   <li><b>ICopybookDialects.FMT_FUJITSU</b> - Written for the old Fujitsu Cobol 3 compiler
	 *   <li><b>ICopybookDialects.FMT_GNU_COBOL</b> - GNU Cobol (formerly Open Cobol) on a Little Endian machine (e.g Intel).
	 *   <li><b>ICopybookDialects.FMT_OC_MICRO_FOCUS_BE</b> -  GNU Cobol running in Microfocus compatibility mode on a Big Endian machine
	 * </ul>
	 * @param debug
	 * @param formatFormat of the Cobol Copybook:<ul>
	 *    <li>Cb2xmlConstants.FREE_FORMAT Free format copybook
	 *    <li>Cb2xmlConstants.USE_STANDARD_COLUMNS - Standard Cobol Columns
	 *    <li>Cb2xmlConstants.USE_COLS_6_TO_80 - use columns 6 -> 80
	 *    <li>Cb2xmlConstants.USE_LONG_LINE - Long line starting at column 6
	 *  </ul>
	 * @return
	 * @throws ParserException
	 * @throws LexerException
	 * @throws IOException
	 * @throws XMLStreamException
	 */
	public static Document convertToXMLDOM(Reader reader, String name,  int cobolDialect, boolean debug, int format) 
			throws ParserException, LexerException, IOException, XMLStreamException {
		
		return convertToXMLDOM(
				net.sf.cb2xml.Cb2Xml3
					.newBuilder(reader, name),
				cobolDialect, debug, format);
	}


	@SuppressWarnings("deprecation")
	private static Document convertToXMLDOM(ICb2XmlBuilder bldr, int cobolDialect, boolean debug, int format) 
			throws ParserException, LexerException, IOException, XMLStreamException {
		
		Convert conv = ConversionManager.getInstance().getConverter4code(cobolDialect) ;
		bldr
			.setDebug(debug)
			.setCobolLineFormat(format)
			.setLoadComments(false)
			.setXmlFormat(Cb2xmlConstants.Cb2xmlXmlFormat.CLASSIC)
			.setDialect(conv);
	
		return net.sf.cb2xml.Cb2Xml2.bldrToDocument(bldr);	
	}



	/**
	 * 
	 * @param reader Cobol Copybook Reader
	 * @param name Cobol Copybook Name
	 * @param cobolDialect COBOL dialect<ul>
	 *   <li><b>ICopybookDialects.FMT_MAINFRAME</b> - Mainframe Cobol
	 *   <li><b>ICopybookDialects.FMT_FUJITSU</b> - Written for the old Fujitsu Cobol 3 compiler
	 *   <li><b>ICopybookDialects.FMT_GNU_COBOL</b> - GNU Cobol (formerly Open Cobol) on a Little Endian machine (e.g Intel).
	 *   <li><b>ICopybookDialects.FMT_OC_MICRO_FOCUS_BE</b> -  GNU Cobol running in Microfocus compatibility mode on a Big Endian machine
	 * </ul>
	 * @param debug
	 * @param formatFormat of the Cobol Copybook:<ul>
	 *    <li>Cb2xmlConstants.FREE_FORMAT Free format copybook
	 *    <li>Cb2xmlConstants.USE_STANDARD_COLUMNS - Standard Cobol Columns
	 *    <li>Cb2xmlConstants.USE_COLS_6_TO_80 - use columns 6 -> 80
	 *    <li>Cb2xmlConstants.USE_LONG_LINE - Long line starting at column 6
	 *  </ul>
	 * @param stackSize
	 * @return
	 */
	public static Copybook getCopybook(Reader reader, String name,  int cobolDialect, boolean debug,
			int format, int stackSize) {
		Convert conv = ConversionManager.getInstance().getConverter4code(cobolDialect) ;
		//synchronized (SYNC) {
			return net.sf.cb2xml.Cb2Xml3
					.newBuilderJRec(reader, name)
							.setDebug(debug)
							.setCobolLineFormat(format)
							.setLoadComments(false)
							.setStackSize(stackSize)
							.setDialect(conv)
						.asCobolItemTree();
			
		//}
	}

}