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

package net.sf.JRecord;

import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDecider;
import net.sf.JRecord.External.CopybookLoaderFactory;
import net.sf.JRecord.IO.builders.CsvIOBuilder;
import net.sf.JRecord.IO.builders.FileSchemaBuilder;
import net.sf.JRecord.IO.builders.FixedWidthIOBuilder;
import net.sf.JRecord.IO.builders.SchemaIOBuilder;
import net.sf.JRecord.IO.builders.recordDeciders.RecordDeciderBuilder;
import net.sf.JRecord.def.IO.builders.ICobolCopybookIOProvider;
import net.sf.JRecord.def.IO.builders.ICsvIOBuilder;
import net.sf.JRecord.def.IO.builders.IFixedWidthIOBuilder;
import net.sf.JRecord.def.IO.builders.IIOCopybookProvider;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;
import net.sf.JRecord.def.IO.builders.Icb2xmlIOProvider;
import net.sf.JRecord.def.IO.builders.recordDeciders.IRecordDeciderBuilder;


/**
 * This class implements an easy to use interface to main JRecord functions. It will create <b>IOBuilders</b>
 * for a variety of schema-types, both those in files / streams
 * and those defined in Java Code. The variables available are<ul>
 * <li><b>COBOL</b> used to Create Cobol data file readers / writers
 * <li><b>CB2XML</b> used to create file reader / writers using a  Cb2xml Xml schema's
 * <li><b>SCHEMA_XML</b> used to create file reader / writers using a  RecordEditor-Xml schema's
 * <li><b>CSV</b> Create Csv readers / writers in java code.
 * <li><b>FIXED_WIDTH</b> Create Fixed-Width file readers / writers in java code (instead of from a Cobol-Copybook or Xml-Schema).
 * <li><b>SCHEMA</b> Create file readers / writers from an existing schema.
 * </ul>
 * 
 * <b>Example:</b><pre>{@code
 *      AbstractLineReader r = JRecordInterface1.COBOL
 *              .newIOBuilder("file-name")
 *                  .setFileOrganization(Constants.IO_FIXED_LENGTH)
 *                  .setDialect(ICopybookDialects.FMT_FUJITSU)
 *              .newReader("Data-Filename");
 * }</pre> 
 * 
 * @author Bruce Martin
 *
 */
public class JRecordInterface1 {

    /**
     * Create Reader's / Writers based on Cobol-Copybooks
     * <pre>
     * <b>Example:</b>
     * <pre>{@code
     *      AbstractLineReader r = JRecordInterface1.COBOL
     *              .newIOBuilder("file-name.cbl")
     *                  .setFileOrganization(Constants.IO_FIXED_LENGTH)
     *                  .setDialect(ICopybookDialects.FMT_FUJITSU)
     *              .newReader("Data-Filename");
     * }</pre> 
     * 
     * The CodeGen utility can generate basic JRecord code. It is available<ul>
     * <li>as part of the <b>RecordEditor</b> see 
     * <a href="http://record-editor.sourceforge.net/RecordEditorGenerate.htm#HDRJRECGEN">http://record-editor.sourceforge.net/RecordEditorGenerate.htm#HDRJRECGEN</a>
     * <li>As a standalone download: <a href="https://sourceforge.net/projects/jrecord/files/jrecord_CodeGen/">https://sourceforge.net/projects/jrecord/files/jrecord_CodeGen/</a>
     * </ul>
     */
    public static final ICobolCopybookIOProvider  COBOL = new FileSchemaBuilder(CopybookLoaderFactory.COBOL_LOADER); 
    
    /**
     * Create IO Builders for Cb2Xml-xml schemas (created from Cobol copybooks by the cb2xml program)
     * 
     * <pre>
     * <b>Example:</b>
     * {@code
     *      AbstractLineReader r = JRecordInterface1.COBOL
     *              .newIOBuilder("file-name.xml")
     *                  .setFileOrganization(Constants.IO_FIXED_LENGTH)
     *              .newReader("Data-Filename");
     * }</pre> 
     */
    public static final Icb2xmlIOProvider CB2XML = new FileSchemaBuilder(CopybookLoaderFactory.CB2XML_LOADER); 

    /**
     * Create IOBuilders for Record-Editor Xml file descriptions
     * 
     * <pre>
     * <b>Example:</b>
     * {@code
     *      AbstractLineReader r = JRecordInterface1.COBOL
     *              .newIOBuilder("file-name.xml")
     *              .newReader("Data-Filename");
     * }</pre> 
     */
    public static final IIOCopybookProvider  SCHEMA_XML = new FileSchemaBuilder(CopybookLoaderFactory.RECORD_EDITOR_XML_LOADER); 
    
    /** 
     * Create an IOBuilder based on an existing Schema (LayoutDetail)
     * 
     * <pre>
     * <b>Example:</b>
     * {@code
     *         AbstractLineReader r = JRecordInterface1.COBOL
     *              .newIOBuilder(IOBuilder.getLayout())
     *              .newReader("Data-Filename");
     * }</pre> 
     */
    public static final SchemaIOBuilderProvider  SCHEMA = new SchemaIOBuilderProvider(); 

    /**
     * Create Csv-IO-Builders. Typical usage for reading is:
     * 
     * <pre>
     * <b>Example:</b>
     * {@code
     *         AbstractLineReader reader = JRecordInterface1.CSV
     *                 .newIOBuilder("\t", "\"")
     *                         .newReader(salesFile);
     *         while ((csvLine = reader.read()) { ... }
     *       
     * }</pre> 
     *  or
     * <pre>{@code
     *  
     *         ICsvIOBuilder outIOBlbdr = JRecordInterface1.CSV
     *                 .newIOBuilder(";", "\"")
     *                         .defineFields()
     *                              .addCsvField(FLD_SKU,   Type.ftChar, 0)
     *                              .addCsvField(FLD_STORE, Type.ftNumAnyDecimal, 0)
     *                              .addCsvField(FLD_DATE,  Type.ftNumAnyDecimal, 0)
     *                              .addCsvField(FLD_DEPT,  Type.ftNumAnyDecimal, 0)
     *                              .addCsvField(FLD_QTY,   Type.ftNumAnyDecimal, 0)
     *                              .addCsvField(FLD_PRICE, Type.ftNumAnyDecimal, 0)
     *                              .addCsvField(FLD_GST,   Type.ftNumAnyDecimal, 0)
     *                          .endOfRecord();
     *         AbstractLineWriter writer = outIOBlbdr.newWriter(salesFileOut);                        
     *         AbstractLine outCsvRecord = outIOBlbdr.newLine();
     *       
     * }</pre> 
     * 
     */
    public static final CsvIOBuilderProvider        CSV = new CsvIOBuilderProvider();
    
    /**
     * Create Fixed-Width-IO Builders
     * 
     * <pre>
     * <b>Example:</b>
     * {@code
     *         AbstractLineReader reader = JRecordInterface1.FIXED_WIDTH.newIOBuilder()
     *                          .defineFieldsByLength()
     *                              .addFieldByLength("Sku"  , Type.ftChar,   8, 0)
     *                              .addFieldByLength("Store", Type.ftNumRightJustified, 3, 0)
     *                              .addFieldByLength("Date" , Type.ftNumRightJustified, 6, 0)
     *                              .addFieldByLength("Dept" , Type.ftNumRightJustified, 3, 0)
     *                              .addFieldByLength("Qty"  , Type.ftNumRightJustified, 2, 0)
     *                              .addFieldByLength("Price", Type.ftNumRightJustified, 6, 2)
     *                          .endOfRecord()
     *                                .newReader(this.getClass().getResource("DTAR020_tst1.bin.txt").getFile());
     *         AbstractLine saleRecord;
     *        
     *         while ((saleRecord = reader.read()) != null) { ..}
     *
     * }</pre> 
     */
    public static final FixedWidthIOBuilderProvider FIXED_WIDTH = new FixedWidthIOBuilderProvider();
    
    
    /**
     * Cobol Copybooks do not provide a means to determine which
	 * Record applies to a particular Data-Line. Most of the
	 * time this does not matter but there are exceptions 
	 * (e.g. <b>Constants.IO_CONTINOUS_NO_LINE_MARKER</b>).
	 * 
	 * <p>One way to tell
	 * JRecord / RecordEditor which Record to use for a Data-line
	 * is to define a {@link RecordDecider}.
	 * 
	 * <pre>
	 *   iobuilder.setRecordDecider(myRecordDecider);
	 * </pre>
	 * 
	 * <p>While you can write your own {@link RecordDecider}.
	 * The RECORD_DECIDER_BUILDER makes it easy to create efficient {@link RecordDecider}
	 * using a builder style interface.
	 * 
	 * <p>For a copybook like:
	 * 
	 * <pre>
	 *       01  Header.
	 *           03 Record-Type            Pic x.
	 *              88 Header-Rec  value 'H'.
	 *              88 Trailer-Rec value 'T'.
	 *           ....
	 *       01  Detail.
	 *           03 Field-1                Pic x.
	 *           ....
	 *       01  Trailer.
	 *           03 Record-TypeT           Pic x.
	 *           ....
	 * </pre>
	 *
	 * The Java code
	 * 
	 *  <pre>
	 *     RecordDecider decider = JRecordInterface1.RECORD_DECIDER_BUILDER
	 *                                    .singleFieldDeciderBuilder("Record-Type", "Detail")
	 *                                        .addRecord("H", "Header")
	 *                                        .addRecord("T", "Trailer")
	 *                                    .build();
	 *    AbstractLineReader r = JRecordInterface1.COBOL
     *              .newIOBuilder("file-name.cbl")
     *                  .setRecordDecider(decider)
     *                  ...                     
	 *  </pre>
     */
    public static final IRecordDeciderBuilder RECORD_DECIDER_BUILDER = new RecordDeciderBuilder();
    
    /**
     * This class creates Csv-IO-Builders {@link CsvIOBuilder}
     * 
     * Create Csv-IO-Builders. Typical usage for reading is:
     * 
     * <pre>
     * <b>Example:</b>
     * {@code
     *         AbstractLineReader reader = JRecordInterface1.CSV
     *                   .newIOBuilder("\t", "\"")
     *                           .newReader(salesFile);
     *         while ((csvLine = reader.read()) { ... }
     *       
     * }</pre> 
     *  or
     * <pre>{@code
     *  
     *         ICsvIOBuilder outIOBlbdr = JRecordInterface1.CSV
     *                   .newIOBuilder(";", "\"")
     *                           .defineFields()
     *                                .addCsvField(FLD_SKU,   Type.ftChar, 0)
     *                                .addCsvField(FLD_STORE, Type.ftNumAnyDecimal, 0)
     *                                .addCsvField(FLD_DATE,  Type.ftNumAnyDecimal, 0)
     *                                .addCsvField(FLD_DEPT,  Type.ftNumAnyDecimal, 0)
     *                                .addCsvField(FLD_QTY,   Type.ftNumAnyDecimal, 0)
     *                                .addCsvField(FLD_PRICE, Type.ftNumAnyDecimal, 0)
     *                                .addCsvField(FLD_GST,   Type.ftNumAnyDecimal, 0)
     *                            .endOfRecord();
     *         AbstractLineWriter writer = outIOBlbdr.newWriter(salesFileOut);                        
     *         AbstractLine outCsvRecord = outIOBlbdr.newLine();
     * }</pre>
     * 
     * @author Bruce Martin
     *
     */
    public static class CsvIOBuilderProvider {
        /**
         * Create a Csv-IO-Builder {@link CsvIOBuilder} with Quote="\"" and field-separator=","
         * @return Csv-IO-Builder {@link CsvIOBuilder}
         */
        public ICsvIOBuilder newIOBuilder() {
            return CsvIOBuilder.newCsvIOBuilder();
        }
        
        /**
         * Create a Csv-IO-Builder {@link CsvIOBuilder} with supplied Quote= and field-separator
         * 
         * @param delimiter field delimiter to use in the file
         * @param quote Quote used to surround fields
         * 
         * @return Csv-IO-Builder {@link CsvIOBuilder}
         */
        public ICsvIOBuilder newIOBuilder(String delimiter, String quote) {
            return CsvIOBuilder.newCsvIOBuilder(delimiter, quote);
        }
    }
    
    /**
     * This class creates Fixed-Width IO Builders {@link IFixedWidthIOBuilder}
     * 
     * <pre>
     * <b>Example:</b>
     * {@code
     *          AbstractLineReader reader = JRecordInterface1.FIXED_WIDTH.newIOBuilder()
     *                                .defineFieldsByLength()
     *                                    .addFieldByLength("Sku"  , Type.ftChar,   8, 0)
     *                                    .addFieldByLength("Store", Type.ftNumRightJustified, 3, 0)
     *                                    .addFieldByLength("Date" , Type.ftNumRightJustified, 6, 0)
     *                                    .addFieldByLength("Dept" , Type.ftNumRightJustified, 3, 0)
     *                                    .addFieldByLength("Qty"  , Type.ftNumRightJustified, 2, 0)
     *                                    .addFieldByLength("Price", Type.ftNumRightJustified, 6, 2)
     *                                .endOfRecord()
     *                                .newReader(this.getClass().getResource("DTAR020_tst1.bin.txt").getFile());
     *         AbstractLine saleRecord;
     *        
     *         while ((saleRecord = reader.read()) != null) { ..}
     *
     * }</pre> 
     * 
     * @author Bruce Martin
     *
     */
    public static class FixedWidthIOBuilderProvider {
        public IFixedWidthIOBuilder newIOBuilder() {
            return FixedWidthIOBuilder.newFixedWidthIOBuilder();
        }
    }
    
    /**
     * This class creates SchemaIOProviders {@link ISchemaIOBuilder}
     * 
     * 
     * <pre>
     * <b>Example:</b>
     * {@code
     *      AbstractLineReader r = JRecordInterface1.COBOL
     *              .newIOBuilder(IOBuilder.getLayout())
     *              .newReader("Data-Filename");
     * }</pre> 
     * 
     * @author Bruce Martin
     *
     */
    public static class SchemaIOBuilderProvider {
        public ISchemaIOBuilder newIOBuilder(LayoutDetail schema) {
            return SchemaIOBuilder.newSchemaIOBuilder(schema);
        }
    }
}
