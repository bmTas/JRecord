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

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.ICopybookLoaderCobol;
import net.sf.JRecord.IO.builders.CblIOBuilderMultiSchemaBase;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;
import net.sf.JRecord.detailsBasic.IItemDetails;
import net.sf.JRecord.fieldNameConversion.IRenameField;
import net.sf.JRecord.schema.fieldRename.StdFieldRenameItems;
import net.sf.JRecord.schema.jaxb.Item;
import net.sf.JRecord.schema.jaxb.ItemRecordDtls;
import net.sf.JRecord.schema.jaxb.interfaces.IFormatField;
import net.sf.JRecord.schema.jaxb.interfaces.IRedefineSelection;
import net.sf.JRecord.schema.jaxb.interfaces.IWriteCheck;


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
//	private Copybook copybook = null;
	//private UpdateSchemaItems itemDtls = null;
	
//	private int tagFormat = IReformatFieldNames.RO_LEAVE_ASIS;
	private IRenameField renameFieldClass = StdFieldRenameItems.LEAVE_ASIS;
	
	//private HashMap<String, IArrayItemCheck> arrayChecks = new HashMap<String, IArrayItemCheck>();
	GroupUpdateDetails updateDetails = new GroupUpdateDetails();
	
	private ISchemaIOBuilder iob = null;
	
	private CobolSchemaDetails cobolDtls = null;
	
//	private List<Item> cobolItems = null;
	private String rootRecord;
	private boolean flatten = false;
	
	
	
	protected CobolSchemaReader(String copybookName, ICopybookLoaderCobol loader) {
		super(copybookName, loader);
		loader.setSaveCb2xmlDocument(true);
	}

 
	protected void clearLayout() {
		synchronized (this) {
			super.clearLayout();
			externalRecord = null;
			iob = null;
		}
	}
	
	protected IRenameField getRenameToCobol() throws IOException  {
		if (renameFieldClass == StdFieldRenameItems.LEAVE_ASIS) {
			return StdFieldRenameItems.LEAVE_ASIS;
		}
		RenameFieldsMap renameFields = new RenameFieldsMap();
		CobolSchemaDetails cblDtls = getCobolSchemaDetails();
		for (ItemRecordDtls ir : cblDtls.recordItems) {
			updateFieldMap(renameFields, ir.items);
		}
		return renameFields;
	}
	
	private void updateFieldMap(RenameFieldsMap renameFields, List<Item> childItems) {
		if (childItems != null) {
			for (Item item : childItems) {
				updateFieldMap(renameFields, item);
			}
		}
	}
	
	private void updateFieldMap(RenameFieldsMap renameFields, Item item) {
		List<Item> childItems = item.getChildItems();
		if (childItems == null || childItems.size() == 0) {
			renameFields.add(renameFieldClass.toFieldName(item.getName()).toUpperCase(), item.getName());
		} else {
			updateFieldMap(renameFields, childItems);
		}
	}
	
	public CobolSchemaDetails getCobolSchemaDetails() throws IOException {
		cobolDtls = getCobolSchemaDetails(updateDetails, cobolDtls);
		return cobolDtls;
	}
	
	public CobolSchemaDetails getCobolSchemaDetails(IGroupUpdateDetails updateDtls) throws IOException {
		return getCobolSchemaDetails(updateDtls, null);
	}
	
	private CobolSchemaDetails getCobolSchemaDetails(IGroupUpdateDetails updateDtls, CobolSchemaDetails cblDtls) throws IOException {
		if (cblDtls == null) {
			synchronized (this) {
				externalRecord = super.getExternalRecord();
				schema = externalRecord.asLayoutDetail();
				iob = JRecordInterface1.SCHEMA.newIOBuilder(schema);
				

		        List<ItemRecordDtls> recordItems = new ArrayList<ItemRecordDtls>(schema.getRecordCount());
		        for (int i = 0; i < schema.getRecordCount(); i++) {
		        	List<? extends IItemDetails> cobolItms = schema.getRecord(i).getCobolItems();
					recordItems.add(new ItemRecordDtls(i, schema.getRecord(i), cobolItms));
		        }
		        UpdateSchemaItems itemDtls = new UpdateSchemaItems(
		        		recordItems, schema, updateDtls,
		        		super.isDropCopybookNameFromFields(), schema.getLayoutName(), renameFieldClass, flatten);

		        cblDtls = new CobolSchemaDetails(schema, recordItems, iob, itemDtls);
			}
		}
		return cblDtls;
	}

	protected GroupUpdateDetails getUpdateDetails() {
		return updateDetails;
	}


	/**
	 * Set class to check wether array field should be priunted
	 * @param arrayName array to be checked
	 * @param arrayCheck class to check the array
	 * @return builder for more updates
	 */
	public final T setArrayCheck(String arrayName, IArrayItemCheck arrayCheck) {
		updateDetails.setArrayCheck(arrayName, arrayCheck);
		clearLayout();
		return super.self;
	}

	/**
	 * Set class to format a field before writing
	 * @param fieldName name of field to be formatted.
	 * @param formatField Class to format the field for writing
	 * @return builder for more updates
	 */
	public final T setFormatField(String fieldName, IFormatField formatField) {
		updateDetails.setFormatField(fieldName, formatField);
		clearLayout();
		return super.self;
	}

	/**
	 * Set a group/field write check method
	 * @param groupName group/field name
	 * @param writeCheck class to check wether to write the item 
	 * @return builder for more updates
	 */
	public final T setWriteCheck(String groupName, IWriteCheck writeCheck) {
		updateDetails.setWriteCheck(groupName, writeCheck);
		clearLayout();
		return super.self;
	}
	/**
	 * Set a Redefines Selection class. This class has a method to select which
	 * redefine-group to use
	 * @param groupName group/field name
	 * @param redefineSelection class to select which redefine-item to use
	 * @return builder for more updates
	 */
	public final T setRedefineSelection(String groupName, IRedefineSelection redefineSelection) {
		updateDetails.setRedefineSelection(groupName, redefineSelection);
		clearLayout();
		return super.self;
	}

	/**
	 * Set class to check wether array field should be priunted
	 * @param groupNames Cobol-names that identify the array to be checked
	 * @param arrayCheck class to check the array
	 * @return builder for more updates
	 */
	public final T setArrayCheck(List<String> groupNames, IArrayItemCheck arrayCheck) {
		updateDetails.setArrayCheck(groupNames, arrayCheck);
		clearLayout();
		return super.self;
	}

	/**
	 * Set class to format a field before writing
	 * @param groupNames Cobol-names that identify the field to be formatted.
	 * @param formatField Class to format the field for writing
	 * @return builder for more updates
	 */
	public final T setFormatField(List<String> groupNames, IFormatField formatField) {
		updateDetails.setFormatField(groupNames, formatField);
		clearLayout();
		return super.self;
	}

	/**
	 * Set a group/field write check method
	 * @param groupNames Cobol-names that identify  group/field to be check before it is written
	 * @param writeCheck class to check wether to write the item 
	 * @return builder for more updates
	 */
	public final T setWriteCheck(List<String> groupNames, IWriteCheck writeCheck) {
		updateDetails.setWriteCheck(groupNames, writeCheck);
		clearLayout();
		return super.self;
	}
	/**
	 * Set a Redefines Selection class. This class has a method to select which
	 * redefine-group to use
	 * @param groupNames Cobol-names that identify the redefines group/field 
	 * @param redefineSelection class to select which redefine-item to use
	 * @return builder for more updates
	 */
	public final T setRedefineSelection(List<String> groupNames, IRedefineSelection redefineSelection) {
		updateDetails.setRedefineSelection(groupNames, redefineSelection);
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
	
	

	public boolean isFlatten() {
		return flatten;
	}


	public T setFlattenStructure(boolean flatten) {
		this.flatten = flatten;
		return self;
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
		return setRenameFieldClass( StdFieldRenameItems.getRenameField(tagFormat));
	}

	public T setRenameFieldClass(IRenameField renameFieldClass) {
		this.renameFieldClass = renameFieldClass;
		synchronized (this) {
			this.cobolDtls = null;
		}
	
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

	public static class CblSchemaReader extends CobolSchemaReader<ICobolSchemaReader> implements ICobolSchemaReader {

		public CblSchemaReader(String copybookName, ICopybookLoaderCobol loader) {
			super(copybookName, loader);
		}
		
	}
	
	private static  class RenameFieldsMap implements IRenameField {
		private final HashMap<String, String> fieldNameMap = new HashMap<String, String>();

		
		public RenameFieldsMap add(String cobolName, String javaName) {
			fieldNameMap.put(cobolName.toUpperCase(), javaName);
			//cobolNameMap.put(javaName, cobolName);
			
			return this;
		}
		
		@Override
		public String toFieldName(String schemaFieldName) {
			String ret = fieldNameMap.get(schemaFieldName.toUpperCase());
			return ret == null ? schemaFieldName : ret ;
		}
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
		return (new CblSchemaReader(copybookName, new CobolCopybookLoader()))
						.addCopyBook(cobolCopybookReader, copybookName);
	}
}
