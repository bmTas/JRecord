/*
 * @Author Bruce Martin
 * Created on 14/04/2005
 *
 * Purpose:
 */
/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
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

package net.sf.JRecord.External;

import java.io.File;
import java.io.InputStream;
import java.io.Reader;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Log.TextLog;

import org.w3c.dom.Document;

/**
 * This class holds routines to load a Cobol Copybook
 * into ExternalRecord.
 *
 * @author Bruce Martin
 *
 */
public class CobolCopybookLoader implements ICopybookLoaderCobol {

    private static final String PROBLEM_LOADING_COPYBOOK = "Error Converting Cobol Copybook: {0}    Cause:\n{1}";
	private static boolean available = true;
    private static boolean toCheck = true;
  
    private XmlCopybookLoader xmlLoader;

    /**
     * Create Cobol Copybook loader
     */
    public CobolCopybookLoader() {
        this(new XmlCopybookLoader());
    }

    /**
     * Create Cobol Copybook loader
     * @param loader4xml class to load a Cb2Xml XML file
     */
    public CobolCopybookLoader(XmlCopybookLoader loader4xml) {
        super();

        xmlLoader = loader4xml;
       // System.out.println("Cobol Copybook loader");
    }
    
    
    
	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.ICopybookLoaderStream#setSaveCb2xmlDocument(boolean)
	 */
	@Override
	public void setSaveCb2xmlDocument(boolean saveCb2xml) {
		xmlLoader.setSaveCb2xmlDocument(saveCb2xml);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.CopybookLoader#loadCopyBook(java.lang.String, int, int, java.lang.String, int, int, net.sf.JRecord.Log.AbsSSLogger)
	 */
	@Override
	public final ExternalRecord loadCopyBook(String copyBookFile,
		                                	int splitCopybookOption, 
                                            int dbIdx, 
                                            String font, 
                                            int binFormat,
		                                	int systemId, 
                                            AbsSSLogger log) {

		return loadCopyBook(copyBookFile, splitCopybookOption, dbIdx, font, CommonBits.getDefaultCobolTextFormat(), binFormat, systemId, log);
		
	}


    /**
     * Insert a XML Dom Copybook into the Copybook DB
     *
     * @param copyBookFile Copy Book file Name
     * @param splitCopybook wether to split a copy book on a redefine / 01
     * @param dbIdx Database Index
     * @param font font name to use
     * @param binFormat binary format to use
     * @param systemId System Identifier
     * @param log log where any messages should be written
     *
     * @return return the record that has been read in
     */
    public final ExternalRecord loadCopyBook(String copyBookFile, //Document copyBookXml,
            						  		int splitCopybook,
            						  		int dbIdx,
                  						  final String font,
                  						  final int copybookFormat,
                						  final int binFormat,
                						  final int systemId,
                						        AbsSSLogger log)
    				{
        ExternalRecord ret = null;
        //System.out.println("load Copybook (Cobol)");
        try {
        	synchronized (PROBLEM_LOADING_COPYBOOK) {	
	            Document xml = net.sf.JRecord.External.Def.Cb2Xml.convertToXMLDOM(new File(copyBookFile), binFormat, false, copybookFormat, log);
	            String copyBook = Conversion.getCopyBookId(copyBookFile);
	
	            if (xml != null) {
		            ret = xmlLoader.loadDOMCopyBook(xml, copyBook,
		                    splitCopybook, dbIdx,
						    font, binFormat, systemId);
		        } else if (log != null) {
	            	log.logMsg(AbsSSLogger.ERROR, "Error parsing Cobol File ???");
	            }
        	}
        } catch (Exception e) {
        	log = TextLog.getLog(log);
            log.logMsg(AbsSSLogger.ERROR, e.getMessage());
            log.logException(AbsSSLogger.ERROR, e);
            
            if (! (log instanceof TextLog)) {
            	e.printStackTrace();
            }
            throw new RecordException(
            				PROBLEM_LOADING_COPYBOOK,
            				new Object[] {copyBookFile, e.getMessage()},
            				e);
        }

        return ret;
    }

    /**
     * Insert a XML Dom Copybook into the Copybook DB
     *
     * @param copyBookName Copy Book file Name
     * @param splitCopybook wether to split a copy book on a redefine / 01
     * @param dbIdx Database Index
     * @param font font name to use
     * @param binaryFormat binary format to use
     * @param systemId System Identifier
     * @param log log where any messages should be written
     *
     * @return return the record that has been read in
     */
    public final ExternalRecord loadCopyBook(InputStream inputStream, //Document copyBookXml,
    		                             String copyBookName,
            						  		int splitCopybook,
            						  		int dbIdx,
                  						  final String font,
                						  final int binaryFormat,
                						  final int systemId,
                						  final AbsSSLogger log)
    				{
    	return loadCopyBook(inputStream, copyBookName, splitCopybook, dbIdx, font, CommonBits.getDefaultCobolTextFormat(), binaryFormat, systemId, log);
    }
    
    /* (non-Javadoc)
	 * @see net.sf.JRecord.External.ICopybookLoaderStream#loadCopyBook(java.io.InputStream, java.lang.String, int, int, java.lang.String, int, int, int, net.sf.JRecord.Log.AbsSSLogger)
	 */
    @Override
    public final ExternalRecord loadCopyBook(InputStream inputStream, //Document copyBookXml,
            String copyBookName,
		  		int splitCopybook,
		  		int dbIdx,
			  final String font,
			  final int copybookFormat,	  
			  final int binaryFormat,
			  final int systemId,
			        AbsSSLogger log)
					  {

        ExternalRecord ret = null;
        //System.out.println("load Copybook (Cobol)");
        try {
        	synchronized (PROBLEM_LOADING_COPYBOOK) {		
	            Document xml = net.sf.JRecord.External.Def.Cb2Xml.convertToXMLDOM(inputStream, copyBookName,  binaryFormat, false, copybookFormat);

	            //Document xml = net.sf.cb2xml.Cb2Xml2.convertToXMLDOM(inputStream, copyBookName, false, copybookFormat);
	
	            if (xml != null) {
		            ret = xmlLoader.loadDOMCopyBook(xml, copyBookName,
		                    splitCopybook, dbIdx,
						    font, binaryFormat, systemId);
		        } else if (log != null) {
	            	log.logMsg(AbsSSLogger.ERROR, "Error parsing Cobol File ???");
	            }
        	}
        } catch (Exception e) {
        	log = TextLog.getLog(log);
            log.logMsg(AbsSSLogger.ERROR, e.getMessage());
            log.logException(AbsSSLogger.ERROR, e);
            
            if (! (log instanceof TextLog)) {
            	e.printStackTrace();
            }
            throw new RecordException(
            					PROBLEM_LOADING_COPYBOOK ,
                    			new Object[] {copyBookName, e.getMessage()},
                    			e);
        }

        return ret;
    }

    @Override
    public final ExternalRecord loadCopyBook(Reader reader, //Document copyBookXml,
            String copyBookName,
		  		int splitCopybook,
		  		int dbIdx,
			  final String font,
			  final int copybookFormat,	  
			  final int binaryFormat,
			  final int systemId,
			        AbsSSLogger log)
					  {

        ExternalRecord ret = null;
        //System.out.println("load Copybook (Cobol)");
        try {
        	synchronized (PROBLEM_LOADING_COPYBOOK) {		
	        	//CopyBookAnalyzer.setNumericDetails((NumericDefinition) conv.getNumericDefinition());
	            Document xml = net.sf.JRecord.External.Def.Cb2Xml.convertToXMLDOM(reader, copyBookName,  binaryFormat, false, copybookFormat);

	            //Document xml = net.sf.cb2xml.Cb2Xml2.convertToXMLDOM(inputStream, copyBookName, false, copybookFormat);
	
	            if (xml != null) {
		            ret = xmlLoader.loadDOMCopyBook(xml, copyBookName,
		                    splitCopybook, dbIdx,
						    font, binaryFormat, systemId);
		        } else if (log != null) {
	            	log.logMsg(AbsSSLogger.ERROR, "Error parsing Cobol File ???");
	            }
        	}
        } catch (Exception e) {
        	log = TextLog.getLog(log);
            log.logMsg(AbsSSLogger.ERROR, e.getMessage());
            log.logException(AbsSSLogger.ERROR, e);
            
            if (! (log instanceof TextLog)) {
            	e.printStackTrace();
            }
            throw new RecordException(
            					PROBLEM_LOADING_COPYBOOK ,
                    			new Object[] {copyBookName, e.getMessage()},
                    			e);
        }

        return ret;
    }

    /**
     * wether cb2xml is available (needed for converting a Cobol Copybook
     * to a XML Dom representation
     *
     * @return wether cb2xml is available on the class path
     */
    public static final boolean isAvailable() {

        if (toCheck) {
            try {
                /*
                 * try to load CobolPreprocessor to see if the cb2xml jar is present
                 * I use the CobolPreprocessor because it only uses IO classes.
                 * This aviods loading unnessary classes before need be
                 */
                available = ((new CobolCopybookLoader()).getClass().getClassLoader().getResource("net/sf/cb2xml/Cb2Xml.class") != null);
            } catch (Exception e) {
                available = false;
            }
            toCheck = false;
        }
        return available;
    }

	/**
	 * @param dropCopybookFromFieldNames
	 * @see net.sf.JRecord.External.XmlCopybookLoader#setDropCopybookFromFieldNames(boolean)
	 */
    @Override
	public final void setDropCopybookFromFieldNames(
			boolean dropCopybookFromFieldNames) {
		xmlLoader.setDropCopybookFromFieldNames(dropCopybookFromFieldNames);
	}

	/**
	 * @param keepFiller
	 * @see net.sf.JRecord.External.XmlCopybookLoader#setKeepFillers(boolean)
	 */
    @Override
	public void setKeepFillers(boolean keepFiller) {
		xmlLoader.setKeepFillers(keepFiller);
	}
	
	
}
