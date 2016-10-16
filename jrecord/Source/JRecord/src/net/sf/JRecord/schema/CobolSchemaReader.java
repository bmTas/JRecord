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

package net.sf.JRecord.schema;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.ICopybookLoaderCobol;
import net.sf.JRecord.External.base.Cb2xmlDocument;
import net.sf.JRecord.IO.builders.CblIOBuilderMultiSchemaBase;
import net.sf.JRecord.Option.IReformatFieldNames;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;
import net.sf.JRecord.schema.jaxb.Condition;
import net.sf.JRecord.schema.jaxb.Copybook;
import net.sf.JRecord.schema.jaxb.Item;

import org.w3c.dom.Document;


/**
 * Purpose: Read a Cobol-Copybook and create:
 * <ul>
 *   <li><b>JRecord-Layout</b> (file schema)
 *   <li><b>Cobol Item Tree</b> - A Cobol-Item tree complete with JRecord-Field Details / Array Details.
 *   This <b>Item Tree</b> can be used to create Xml (as in Cobol2Xml), JSon (forthcoming Cobol2Json utility),
 *   Avro etc.
 *   <li><b>IOBuilder</b> IOBuilder for the Layout.
 * </ul>   
 *   
 *  
 * @author Bruce Martin
 *
 */
public class CobolSchemaReader<T> extends CblIOBuilderMultiSchemaBase<T> implements ISchemaIOBuilder {
	

	private ExternalRecord externalRecord = null;
	private LayoutDetail schema;
	private Copybook copybook = null;
	//private UpdateSchemaItems itemDtls = null;
	
	private int tagFormat = IReformatFieldNames.RO_LEAVE_ASIS;
	
	private HashMap<String, IArrayItemCheck> arrayChecks = new HashMap<String, IArrayItemCheck>();
	
	private ISchemaIOBuilder iob = null;
	
	private CobolSchemaDetails cobolDtls = null;
	
	private List<Item> cobolItems = null;
	private String rootRecord;
	
	
	
	protected CobolSchemaReader(String copybookName, ICopybookLoaderCobol loader) {
		super(copybookName, loader);
		loader.setSaveCb2xmlDocument(true);
	}

 
	protected void clearLayout() {
		super.clearLayout();
		externalRecord = null;
		copybook = null;
		iob = null;
	}
	
	public CobolSchemaDetails getCobolSchemaDetails() throws IOException, JAXBException {
		if (copybook == null) {
			synchronized (this) {
				if (copybook == null) {
					externalRecord = super.getExternalRecord();
					schema = externalRecord.asLayoutDetail();
					iob = JRecordInterface1.SCHEMA.newIOBuilder(schema);
					
					List<Cb2xmlDocument> cb2xmlDocuments = externalRecord.getCb2xmlDocuments();
					
					if (cb2xmlDocuments.size() != 1) {
						throw new RuntimeException("Expecting 1 cb2xml document but got: " + cb2xmlDocuments.size());
					} 
					
			        JAXBContext jc = JAXBContext.newInstance(Condition.class, Copybook.class, Item.class);
			        
			        Unmarshaller unmarshaller = jc.createUnmarshaller();
			        JAXBElement<Copybook> jaxbCopybook = unmarshaller.unmarshal(((Document) cb2xmlDocuments.get(0).cb2xmlDocument), Copybook.class);
			        Copybook cpybook = jaxbCopybook.getValue();
			        this.cobolItems = cpybook.getCobolItems();
			        UpdateSchemaItems itemDtls = new UpdateSchemaItems(
			        		cpybook, schema, arrayChecks,
			        		super.isDropCopybookNameFromFields(), schema.getLayoutName(), tagFormat);
			        List<Item> recordItems = cobolItems;
			        if (schema.getRecordCount() == 1 && recordItems.size() > 1) {
			        	recordItems = new ArrayList<Item>(1);
			        	Item recItem = new Item();
			        	recItem.initFields(schema.getLayoutName(), cobolItems);
			        	recItem.nameToUse = itemDtls.updateName(recItem.fieldName);
			        	
			        	recordItems.add(recItem);
			        }
					cobolDtls = new CobolSchemaDetails(schema, recordItems, cpybook, iob, itemDtls);
					copybook = cpybook;
				}
			}
		}
		return cobolDtls;
	}

	public final T setArrayCheck(String arrayName, IArrayItemCheck check) {
		arrayChecks.put(arrayName.toUpperCase(), check);
		clearLayout();
		return super.self;
	}

	/* 
	 * @see net.sf.JRecord.IO.builders.CblIOBuilderMultiSchemaBase#setSplitCopybook(int)
	 */
	@Override
	public final T setSplitCopybook(int splitCopybook) {
		if (splitCopybook == CopybookLoader.SPLIT_REDEFINE ) {
			throw new RecordException("Split on redefines is not supported !!!");
		}
		return super.setSplitCopybook(splitCopybook);
	}
	
	

	public final ISchemaIOBuilder asIOBuilder() {
		if (iob != null) {
			return iob;
		}
		return this;
	}
	

	/**
	 * @see ICobolSchemaDetails#setTagFormat(int)
	 */
	public final T setTagFormat(int tagFormat) {
		this.tagFormat = tagFormat;
		copybook = null;
	
		return super.self;
	}

	/**
	 * @return the rootRecordName
	 */
	public final String getRootRecord() {
		return rootRecord;
	}


	public final T setRootRecord(String recordName) {
		this.rootRecord = recordName;
		
		return super.self;
	}


	/**
	 * Create a new <b>Extended Cobol Schema Reader</b>
	 * 
	 * @param cobolCopybook Cobol Copybook file name
	 * 
	 * @return Extended Cobol Schema Reader
	 */
	public static ICobolSchemaReader newCobolSchemaReader(String cobolCopybook) { 
		return new CobolSchemaReader<ICobolSchemaReader>(Conversion.getCopyBookId(cobolCopybook), new CobolCopybookLoader())
				.addCopyBook(cobolCopybook);
	}
	
	
	/**
	 * Create a new <b>Extended Cobol Schema Reader</b>
	 * @param cobolCopybookReader A Reader for the Cobol-Copybook
	 * @param copybookName Cobol Copybook file name
	 * @return Extended Cobol Schema Reader
	 */
	public static ICobolSchemaReader newCobolSchemaReader(Reader cobolCopybookReader, String copybookName) {
		return (new CobolSchemaReader<ICobolSchemaReader>(copybookName, new CobolCopybookLoader()))
						.addCopyBook(cobolCopybookReader, copybookName);
	}
}
