/*
 * @Author Bruce Martin
 * Created on 19/03/2007
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

package net.sf.JRecord.IO;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.IBasicFileSchema;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LineProvider;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.builders.CblIOBuilderMultiSchema;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;


/**
 * 
 * This Class Creates a line-reader or line-writer for a Cobol file (Cobol Copybook).
 *
 * <pre>
 * <b>Usage:</b>
 *   
 *        CobolIoProvider ioProvider = CobolIoProvider.getInstance();
 *
 *        try {
 *             AbstractLineReader reader  = ioProvider.getLineReader(
 *                 Constants.IO_TEXT_LINE, ICopybookDialects.FMT_INTEL,
 *                 CopybookLoader.SPLIT_NONE, copybookName, vendorFile
 *             );
 * </pre>
 * 
 * @author Bruce Martin
 *
 */
public class CobolIoProvider {

    private static CobolIoProvider instance = new CobolIoProvider();
    private CopybookLoader copybookInt = new CobolCopybookLoader();

    /**
     * Create a new Mainframe-Cobol IOBulder for a file.
     * @param copybookFileame name of the Copybook (or schema file).
     * @return requested IOBuilder
     */
	public ICobolIOBuilder newIOBuilder(String copybookFileame) {
    	return newIOBuilder(copybookFileame, ICopybookDialects.FMT_MAINFRAME);
    }
    
    /**
     * Create a new Cobol IOBulder for a file.
     * 
     * @param copybookFilename name of the Copybook (or schema file).
     * @param cobolDialect Cobol Dialect. Values include:<ul>
     *   <li><b>ICopybookDialects.FMT_MAINFRAME</b> - Mainframe cobol
     *   <li><b>ICopybookDialects.FMT_GNU_COBOL</b> - GNU cobol 
     *   <li><b>ICopybookDialects.FMT_FUJITSU</b> - Old Free Fujitsu Cobol 3. 
     * </ul>
     * 
     * These are the default values (which can be overriden with the appropriate set* method
     * @return requested IOBuilder
     */
	public ICobolIOBuilder newIOBuilder(String copybookFilename, int cobolDialect) {
		return new CblIOBuilderMultiSchema(copybookFilename, new CobolCopybookLoader(), cobolDialect);
//    	return new CblIOBuilderSchemaFilename(copybookFileame, new CobolCopybookLoader(), cobolDialect);
    }

    /**
     * Create a new Mainframe-Cobol IOBulder for a file.
     * @param cobolCopybookStream stream to read the Cobol Copybook from
     * @param copybookName name of the Cobol-Copybook.
     * @return requested IOBuilder
     */
	public ICobolIOBuilder newIOBuilder(InputStream cobolCopybookStream, String copybookName) throws IOException {
    	return newIOBuilder(cobolCopybookStream, copybookName, ICopybookDialects.FMT_MAINFRAME);
    }
    
    /**
     * Create a new Cobol IOBulder for a file.
     * @param cobolCopybookStream stream to read the Cobol Copybook from
     * @param copybookName name of the Cobol-Copybook.
     * @param cobolDialect Cobol Dialect. Values include:<ul>
     *   <li><b>ICopybookDialects.FMT_MAINFRAME</b> - Mainframe cobol
     *   <li><b>ICopybookDialects.FMT_GNU_COBOL</b> - GNU Cobol 
     *   <li><b>ICopybookDialects.FMT_FUJITSU</b> - Old Free Fujitsu Cobol 3. 
     * </ul>
     * 
     * These are the default values (which can be over-ridden with the appropriate set* method
     * @return requested IOBuilder
     */
	public ICobolIOBuilder newIOBuilder(InputStream cobolCopybookStream, String copybookName, int cobolDialect) {
    	return new CblIOBuilderMultiSchema(cobolCopybookStream, copybookName, new CobolCopybookLoader(), cobolDialect);
//    	return new CblIOBuilderSchemaStream(cobolCopybookStream, copybookName, new CobolCopybookLoader(), cobolDialect);
    }
    

    /**
     * <b>Note:</b> This is part of the "old JRecord Interface". 
     * Most users  will be better off using {@link net.sf.JRecord.JRecordInterface1#COBOL} to 
     * create {@link net.sf.JRecord.def.IO.builders.IIOBuilder} classes or the 
     * {@link CobolIoProvider#newIOBuilder(String)}}
     * 
     * Creates a line reader for a Cobol file
     * 
     * @param fileStructure Structure of the input file
     * @param numericType Numeric Format data (options include mainframe, Fujitu, PC compiler etc)
     * @param splitOption Option to split the copybook up <ul>
     *  <li>No Split
     *  <li>Split on  redefine
     *  <li>Split on 01 level
     * </ul>
     * @param copybookName Copybook (or Layout) name
     * @param filename input file name
     * @return requested Line Reader
     * @throws Exception
     */
    public AbstractLineReader getLineReader(int fileStructure,
			   int numericType, int splitOption, int copybookFormat,
			   String copybookName, String filename)
    throws Exception {
        return getLineReader(fileStructure,
 			   numericType, splitOption, copybookFormat,
			   copybookName, filename,
			   null);
    }

    public AbstractLineReader getLineReader(int fileStructure,
			   int numericType, int splitOption,
			   String copybookName, String filename)
					   throws Exception {
     return getLineReader(fileStructure,
			   numericType, splitOption,
			   copybookName, filename,
			   null);
 }

    /**
      * <b>Note:</b> This is part of the "old JRecord Interface". 
     * Most users  will be better off using {@link net.sf.JRecord.JRecordInterface1#COBOL} to 
     * create {@link net.sf.JRecord.def.IO.builders.IIOBuilder} classes or the 
     * {@link CobolIoProvider#newIOBuilder(String)}}
     * 
     * Creates a line reader for a Cobol file
     * 
     * @param fileStructure Structure of the input file
     * @param numericType Numeric Format data (is mainframe, Fujitu PC compiler etc)
     * @param splitOption Option to split the copybook up <ul>
     *  <li>No Split
     *  <li>Split on  redefine
     *  <li>Split on 01 level
     * </ul>
     * @param copybookName Copybook (or Layout) name
     * @param filename input file name
     * @param provider line provider (to build your own lines)
     * @return requested Line Reader
     * @throws Exception
     */
    public AbstractLineReader getLineReader(int fileStructure,
 			   int numericType, int splitOption,
 			   String copybookName, String filename,
 			   LineProvider provider)
     throws Exception {
    	return getLineReader(fileStructure, numericType, splitOption, CommonBits.getDefaultCobolTextFormat(), copybookName, filename, provider);
    }
    
    /**
     * <b>Note:</b> This is part of the "old JRecord Interface". 
     * Most users  will be better off using {@link net.sf.JRecord.JRecordInterface1#COBOL} to 
     * create {@link net.sf.JRecord.def.IO.builders.IIOBuilder} classes or the 
     * {@link CobolIoProvider#newIOBuilder(String)}}
     * Creates a line reader for a Cobol file
     * 
     * @param fileStructure Structure of the input file
     * @param numericType Numeric Format data (is mainframe, Fujitu PC compiler etc)
     * @param splitOption Option to split the copybook up <ul>
     *  <li>No Split
     *  <li>Split on  redefine
     *  <li>Split on 01 level
     * </ul>
     * @param copybookFormat format of the copybook e.g. Cb2xmlConstants.USE_*
     * @param copybookName Copybook (or Layout) name
     * @param filename input file name
     * @param provider line provider (to build your own lines)
     * @return requested Line Reader
     * @throws Exception
     */
    public AbstractLineReader getLineReader(int fileStructure,
			   int numericType, int splitOption, int copybookFormat,
			   String copybookName, String filename,
			   LineProvider provider)
    throws Exception {
        AbstractLineReader ret;
        String font = "";
        if (numericType == ICopybookDialects.FMT_MAINFRAME) {
            font = "cp037";
        }
       	LayoutDetail copyBook = 
       	     copybookInt.loadCopyBook(
                        copybookName,
                        splitOption, 0, font,
                        copybookFormat,
                        numericType, 0, null
                ).setFileStructure(fileStructure)
       	     	 .asLayoutDetail()
        ;

//       	if (provider == null) {
//       		provider = LineIOProvider.getInstance().getLineProvider(fileStructure, font);
//       	}
       	ret = LineIOProvider.getInstance()
       				.getLineReader(copyBook, provider);
       	ret.open(filename, copyBook);

       	return ret;

    }


    /**
     * <b>Note:</b> This is part of the "old JRecord Interface". 
     * Most users  will be better off using {@link net.sf.JRecord.JRecordInterface1#COBOL} to 
     * create {@link net.sf.JRecord.def.IO.builders.IIOBuilder} classes or the 
     * {@link CobolIoProvider#newIOBuilder(String)}}
     * 
     * Create a line writer for a Cobol File
     * 
     * @param fileStructure structure of the output file
     * @return Line writer for the file
     */ @Deprecated
    public AbstractLineWriter getLineWriter(int fileStructure, String outputFileName)
    throws IOException {
        AbstractLineWriter ret = LineIOProvider.getInstance()
        			.getLineWriter(fileStructure);
        ret.open(outputFileName);
        return ret;
    }

 

     /**
      * <b>Note:</b> This is part of the "old JRecord Interface". 
      * Most users  will be better off using {@link net.sf.JRecord.JRecordInterface1#COBOL} to 
      * create {@link net.sf.JRecord.def.IO.builders.IIOBuilder} classes or the 
      * {@link CobolIoProvider#newIOBuilder(String)}}
      * 
      * Create a line writer for a Cobol File
      * 
      * @param schema File-Schema (or file definition)
      * @param outputFileName name of the file to be written
      * 
      * @return Line writer for the file
      */
     public AbstractLineWriter getLineWriter(IBasicFileSchema schema, String outputFileName)
     throws IOException {
         AbstractLineWriter ret = LineIOProvider.getInstance()
         			.getLineWriter(schema);
         ret.open(outputFileName);
         return ret;
     }


    /**
     * Get a CobolIoProvider
     * 
     * @return Returns the instance.
     */
    public static CobolIoProvider getInstance() {
        return instance;
    }
}
