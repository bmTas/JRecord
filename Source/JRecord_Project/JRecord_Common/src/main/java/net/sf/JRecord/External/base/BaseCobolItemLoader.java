package net.sf.JRecord.External.base;

import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.External.Def.Cb2xmlJrConsts;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.External.cb2xml.IReadCopybook;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Numeric.ConversionManager;
import net.sf.JRecord.Numeric.Convert;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Option.ICobolSplitOptions;
import net.sf.cb2xml.analysis.BaseItem;
import net.sf.cb2xml.analysis.Copybook;
import net.sf.cb2xml.analysis.Item;
import net.sf.cb2xml.def.IItem;
import net.sf.cb2xml.def.IItemBase;
import net.sf.cb2xml.def.IItemJrUpd;

public class BaseCobolItemLoader<XRecord extends BaseExternalRecord<XRecord>> {


    private static final String STR_YES = "Y";
    private static final String STR_NO  = "N";    

    private final boolean useJRecordNaming ;
    private final IExernalRecordBuilder<XRecord> recBuilder;
    private final IReadCopybook readCopybook;
    
    
    private int stackSize = Cb2xmlJrConsts.CALCULATE_THREAD_SIZE;

    
    private boolean keepFiller,
    				dropCopybookFromFieldNames = CommonBits.isDropCopybookFromFieldNames(), 
    				saveCb2xml;
    
    
	public BaseCobolItemLoader(boolean useJRecordNaming, IExernalRecordBuilder<XRecord> recBuilder, IReadCopybook readCopybook) {
		super();
		this.useJRecordNaming = useJRecordNaming;
		this.recBuilder = recBuilder;
		this.readCopybook = readCopybook;
	}

	public void setKeepFillers(boolean keepFiller) {
		this.keepFiller = keepFiller;
	}

	public void setDropCopybookFromFieldNames(boolean dropCopybookFromFieldNames) {
		this.dropCopybookFromFieldNames = dropCopybookFromFieldNames;
	}

	public void setSaveCb2xmlDocument(boolean saveCb2xml) {
		this.saveCb2xml = saveCb2xml;
	}


	/**
	 * @param stackSize the stackSize to set
	 */
	public void setStackSize(int stackSize) {
		this.stackSize = stackSize;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.CopybookLoader#loadCopyBook(java.lang.String, int, int, java.lang.String, int, int, net.sf.JRecord.Log.AbsSSLogger)
	 */
	public final XRecord loadCopyBook(String copyBookFile,
			int splitCopybookOption, int dbIdx, String font, int binFormat,
			int systemId, AbsSSLogger log) throws IOException {
		
		return loadCopyBook(new FileReader(copyBookFile), Conversion.getCopyBookId(copyBookFile),
				splitCopybookOption, dbIdx,
				font, CommonBits.getDefaultCobolTextFormat(), binFormat, systemId, log);
		
	}
	

    /* (non-Javadoc)
	 * @see net.sf.JRecord.External.ICopybookLoaderStream#loadCopyBook(java.io.InputStream, java.lang.String, int, int, java.lang.String, int, int, int, net.sf.JRecord.Log.AbsSSLogger)
	 */
	public XRecord loadCopyBook(InputStream inputStream,
			String copyBookName, int splitCopybook, int dbIdx, String font,
			int copybookFormat, int binaryFormat, int systemId, AbsSSLogger log)
			throws IOException {
		return loadCopyBook(new InputStreamReader(inputStream), copyBookName, splitCopybook, dbIdx, font, copybookFormat, binaryFormat, systemId, log);
	}


	/**
     * Convert a XML Dom Copybook into the XRecord
     *
     * @param copyBookFile Copy Book file Name
     * @param splitCopybookOption wether to split a copy book on a redefine
     * @param dbIdx Database Index
     * @param font font name to use
     * @param binFormat binary format to use
     * @param systemId System Identifier
     * @param log log where any messages should be written
     *
     * @return record to be inserted
     *
     * @throws IOException error to be handled by calling program
     */
    public XRecord loadCopyBook(final String copyBookFile,
            					final int splitCopybookOption,
            					final int dbIdx,
            					final String font,
            					final int copybookFormat,
            					final int binFormat,
            					final int systemId,
            					final AbsSSLogger log)
    		throws IOException {

    	return loadCopyBook(new FileReader(copyBookFile), Conversion.getCopyBookId(copyBookFile),
    			splitCopybookOption, dbIdx, font, copybookFormat, binFormat, systemId, log);
    }


	

	public XRecord loadCopyBook(Reader reader,
			String copyBookName, int splitCopybook, int dbIdx, String font,
			int copybookFormat, int binaryFormat, int systemId, AbsSSLogger log)
			throws IOException {
		
		Copybook copybook = readCopybook.getCopybook(reader, copyBookName, binaryFormat, false, copybookFormat, stackSize);

//		try {
//			copybook = net.sf.JRecord.External.Def.Cb2Xml
//							.getCopybook(reader, copyBookName, binaryFormat, false, copybookFormat, stackSize);
//		} catch (ParserException e) {
//			throw new IOException(e);
//		} catch ( XMLStreamException e) {
//			throw new IOException(e);
//		} catch (LexerException e) {
//			throw new IOException(e);
//		}

		return loadCopybook(copybook, copyBookName, splitCopybook, dbIdx, font, binaryFormat, systemId);
	}

	/**
	 * @param copybook
	 * @param copyBookName
	 * @param splitCopybook
	 * @param dbIdx
	 * @param font
	 * @param binaryFormat
	 * @param systemId
	 * @return
	 */
	public XRecord loadCopybook(Copybook copybook, String copyBookName, int splitCopybook, int dbIdx, String font,
			int binaryFormat, int systemId) {
		XRecord ret;
       	List<? extends IItemJrUpd> copybookItems = copybook.getChildItems();
       	
       	allocDBs(dbIdx);
       	
        switch (splitCopybook) {
        case ICobolSplitOptions.SPLIT_NONE:   /* split copybook on first redefine*/
        	ret = createSingleRecordSchema(copyBookName, null, font, binaryFormat, copybookItems);
            break;
        case ICobolSplitOptions.SPLIT_REDEFINE:
        	ret = createSplitOnRedefinesSchema(copyBookName, font, binaryFormat, systemId, copybookItems);
            break;
        case ICobolSplitOptions.SPLIT_TOP_LEVEL:
           	ret = createGroupRecordSchema(copyBookName, null, font, binaryFormat, systemId, copybookItems, true);
            break;
        case ICobolSplitOptions.SPLIT_HIGHEST_REPEATING:
        	ArrayList<String> groups = new ArrayList<String>();
        	List<? extends IItemJrUpd> list = copybookItems;
        	while (list != null && list.size() == 1) {
        		IItemJrUpd itm = list.get(0);
        		String fn = itm.getFieldName();
        		if (fn != null && fn.length() > 0 && ! "filler".equalsIgnoreCase(fn)) {
        			groups.add(fn);
        		}
				list = itm.getChildItems();
        	}
           	String[] groupArray = groups.toArray(new String[groups.size()]);
        	if (list == null || list.size() < 2) {
            	ret = createSingleRecordSchema(copyBookName, groupArray, font, binaryFormat, copybookItems);
        	} else {
				ret = createGroupRecordSchema(
               			copyBookName, groupArray, 
               			font, binaryFormat, systemId, list, true);
        	}
            break;
        default:
        	ret = createGroupRecordSchema(copyBookName, null, font, binaryFormat, systemId, copybookItems, false);
        }
        
        
        ret.setCopybook(copybook);
        updateRecord(copyBookName, systemId, font, ret, STR_YES);
        
        Convert numTranslator = ConversionManager.getInstance().getConverter4code(binaryFormat) ;
        boolean multipleRecordLengths = false,
                binary = false;

        if (ret.getNumberOfRecords() == 0) {
            binary = ret.isBinary();
        } else {
            int len = getRecLength(ret.getRecord(0));
            binary = binary || ret.getRecord(0).isBinary();

            for (int i = 1; i < ret.getNumberOfRecords(); i++) {
                binary = binary || ret.getRecord(i).isBinary();
                multipleRecordLengths = multipleRecordLengths
                                     || (len != getRecLength(ret.getRecord(i)));
            }
        }
        ret.setFileStructure(numTranslator.getFileStructure(multipleRecordLengths, binary));

        freeDBs(dbIdx);       

        return ret;
	}
	

    private int getRecLength(XRecord rec) {
    	int ret = 0;

    	try {
	    	for (int i = 0; i < rec.getNumberOfRecordFields(); i++) {
	    		ExternalField recordField = rec.getRecordField(i);
				ret = Math.max(ret, recordField.getPos() + recordField.getLen() - 1);
	    	}
    	} catch (Exception e) {
			System.out.println("Error Finding Record Length Types");
		}

    	return ret;
    }

	
    protected void allocDBs(int pDbIdx) {

    }


    protected void freeDBs(int pDbIdx) {

    }


	private XRecord createSingleRecordSchema(String copyBookName, String[] groupArray, String font, int binaryFormat,
			List<? extends IItemJrUpd> copybookItems) {
		XRecord ret = createRecord(copyBookName, font, binaryFormat);
		
		//updateType(fldhelper, copybookItems);
		ret.setItems(copyBookName, groupArray, binaryFormat, copybookItems);
		ret.updateTypeOnCobolItems();
		return ret;
	}

	/**
	 * Create a `Group` record
	 */
	private XRecord createGroupRecordSchema(String copyBookName, String[] parentGroupNames, String font, int binaryFormat, int systemId,
			List<? extends IItemJrUpd> copybookItems, boolean updatePosition) {

		IItemJrUpd itm;
		XRecord childRec;
		XRecord ret = createGroupRecord(copyBookName, font, binaryFormat);

		for (int i = 0; i < copybookItems.size(); i++) {
			itm = copybookItems.get(i);
			childRec = createChildRecord(copyBookName, itm.getFieldName(), i, font, binaryFormat, systemId);
			
			if (updatePosition) {
				itm.updatePosition(- itm.getPosition() + 1);
			}

			childRec.setItems(copyBookName, parentGroupNames, binaryFormat, itm);
			childRec.updateTypeOnCobolItems();
			ret.addRecord(childRec);
		}
		return ret;
	}
	
	private XRecord createSplitOnRedefinesSchema(String copyBookName, String font, int binaryFormat, int systemId,
			List<? extends IItemJrUpd> copybookItems) {
		
		XRecord childRec;
		XRecord ret = createGroupRecord(copyBookName, font, binaryFormat);
		RedefineSearcher redef = new RedefineSearcher(copybookItems);
		
		if (redef.item == null) {
			return createSingleRecordSchema(copyBookName, null, font, binaryFormat, copybookItems);
		}
		
		int idx = 0;
		for (IItem redefItem : redef.redefineItems) {
			childRec = createChildRecord(copyBookName, redefItem.getFieldName(), idx++, font, binaryFormat, systemId);
			childRec.setItems(
					copyBookName,
					null,
					binaryFormat,
					createSplitOnRedefines_processList(
								redefItem, new Copybook("", ""),
								redef, copybookItems)
					);
			childRec.updateTypeOnCobolItems();
			ret.addRecord(childRec);
		}
		
		return ret;
	}


	String indent = "\t";
	private List<? extends IItemJrUpd> createSplitOnRedefines_processList(
			IItem redefItem, BaseItem parent, 
			RedefineSearcher redef, List<? extends IItemJrUpd> list) {
		if (list == null || list.size() == 0) { return list; }
		String oindent = indent;
		indent += "\t";
		List<IItemJrUpd> nList = new ArrayList<IItemJrUpd>(list.size());
		for (IItemJrUpd itm : list) {
			//System.out.print(indent + itm.getFieldName() + " " + list.size());
			if (itm == redefItem) {
				nList.add(itm);
			} else if (redef.parents.contains(itm)) {
				//System.out.println();
				Item newItem = new Item(parent, itm.getLevelNumber(), itm.getLevelString(), itm.getFieldName());
				newItem.set(itm);
				List<? extends IItemJrUpd> children = createSplitOnRedefines_processList(redefItem, newItem, redef, itm.getChildItems());
				for (IItemJrUpd ci : children) {
					newItem.addItem((Item)ci);
				}
				nList.add(newItem);
				//System.out.print(indent + itm.getFieldName() + " " + list.size());
			} else if (! redef.redefineItems.contains(itm)) {
				nList.add(itm);
			}
			//System.out.println();
		}
		indent = oindent;
		
		return nList;
	}

	private XRecord createChildRecord(String copyBookName, String fieldName, int idx, String font, int binaryFormat,
			int systemId) {
		XRecord childRec;
		String name = fieldName; 
		if (name == null || name.length() == 0 || "filler".equalsIgnoreCase(name)) {
			name = copyBookName + "_" + idx;
		} else 			if (useJRecordNaming) {
			name = name.trim();
    	} else {
      	   name = copyBookName.trim() + "-" + name.trim();
		}

		childRec = createRecord(name, font, binaryFormat);
		updateRecord(name, systemId, font, childRec, STR_NO);
		return childRec;
	}

	@SuppressWarnings("deprecation")
	protected void updateRecord(String copyBookName, int systemId, String font, XRecord ret, String list) {
		ret.setListChar(list);
        ret.setSystem(systemId);
        ret.setCopyBook(copyBookName);
        ret.setNew(true);
	}   

	private XRecord createRecord(String copyBookName, String font, int binaryFormat) {
        int rt = Constants.rtRecordLayout;
        if (binaryFormat == ICopybookDialects.FMT_MAINFRAME) {
            rt = Constants.rtBinaryRecord;
        }

       return  recBuilder.getNullRecord(
    		   					copyBookName,
				   				rt,
				   				font)
    		   			.setCobolConversionOptions(keepFiller, dropCopybookFromFieldNames, saveCb2xml, useJRecordNaming);
	}


	private XRecord createGroupRecord(String copyBookName, String font, int binaryFormat) {
        int rt = Constants.rtGroupOfRecords;
        if (binaryFormat == ICopybookDialects.FMT_MAINFRAME
        ||  binaryFormat == ICopybookDialects.FMT_BIG_ENDIAN) {
            rt = Constants.rtGroupOfBinaryRecords;
        }

       return  recBuilder.getNullRecord(
    		   					copyBookName,
				   				rt,
				   				font)
	   					.setCobolConversionOptions(keepFiller, dropCopybookFromFieldNames, saveCb2xml, useJRecordNaming);
	}

//	private void updateType(FieldCreatorHelper fldhelper, IItemJrUpd itm) {
//		
//		updateItemForType(fldhelper, itm);
//		updateType(fldhelper, itm.getChildItems());
//	}
//
//	private List<? extends IItemJrUpd> updateType(FieldCreatorHelper fldhelper, List<? extends IItemJrUpd> items) {
//		if (items != null) { 
//			for (IItemJrUpd itm : items) {
//				updateItemForType(fldhelper, itm);
//				updateType(fldhelper, itm.getChildItems());
//			}
//		}
//		
//		return items;
//	}
//	
//	private void updateItemForType(FieldCreatorHelper fldhelper, IItemJrUpd item) {
//		int typeId = Type.ftChar;
//		List<? extends IItemJrUpd> childItems = item.getChildItems();
//		if (childItems == null || childItems.size() == 0) {
//			typeId = fldhelper.deriveType(
//					item.getNumericClass().numeric, item.getUsage().getName(), item.getPicture(), 
//					item.getSignClause().signSeparate, item.getSignClause().signPosition.getName(), 
//					item.getJustified().isJustified); 
//		}
//		item.setType(typeId);
//	}


	/**
	 * Class to search for Redefines and
	 * store retrieved data in a useful for for
	 * later processing
	 * 
	 * @author Bruce Martin
	 *
	 */
	public static class RedefineSearcher {
		int level = Integer.MAX_VALUE;
		IItemJrUpd item;
		List<IItem> redefineItems;
		Set<IItem> redefSet;
		Set<IItemBase> parents;
		
		RedefineSearcher(List<? extends IItemJrUpd> items) {
			search(items);
			
			if (item != null) {
				redefineItems = new ArrayList<IItem>();
				//redefineItems.add(item);
				int pos = item.getPosition();
				for (IItem itm : item.getParent().getChildItems()) {
					if (itm.getPosition() == pos) {
						redefineItems.add(itm);
					} 
				}
				
				redefSet = new HashSet<IItem>(redefineItems);
				parents = new HashSet<IItemBase>();
				
				IItemBase p = item;
				while (p.getParent()!= null) {
					p = p.getParent();
					parents.add(p);
				}
			}
		}
		
		private void search(List<? extends IItemJrUpd> items) {
			if (items == null || items.size() == 0 || items.get(0).getLevelNumber() >= level) { return ;}
			
			for (IItemJrUpd itm : items) {
				if (itm.getLevelNumber() < level && (itm.isFieldRedefined() || itm.isFieldRedefines())) {
					level = itm.getLevelNumber();
					item = itm;
					return;
				}
			}
			
			for (IItemJrUpd itm : items) {
				search(itm.getChildItems());
			}
		}
	}
}
