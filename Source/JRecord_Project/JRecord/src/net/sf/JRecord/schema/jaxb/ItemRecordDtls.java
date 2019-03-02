package net.sf.JRecord.schema.jaxb;

import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.detailsBasic.IItemDetails;

public class ItemRecordDtls {

	public final int recordIndex;
	public final RecordDetail record;
	public final List<Item> items;
	
	public ItemRecordDtls(int recordIndex, RecordDetail record, List<? extends IItemDetails> itemDtls) {
		super();
		this.recordIndex = recordIndex;
		this.record = record;
		this.items = new ArrayList<Item>(itemDtls.size());
		for (IItemDetails itmDtls : itemDtls) {
			this.items.add(new Item(itmDtls));
		}
	}
}
