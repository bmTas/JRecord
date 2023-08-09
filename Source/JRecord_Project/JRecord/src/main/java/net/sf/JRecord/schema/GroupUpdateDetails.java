package net.sf.JRecord.schema;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import net.sf.JRecord.schema.jaxb.interfaces.IFormatField;
import net.sf.JRecord.schema.jaxb.interfaces.IRedefineSelection;
import net.sf.JRecord.schema.jaxb.interfaces.IWriteCheck;

public class GroupUpdateDetails implements IGroupUpdateDetails {
	

	public static final IItemUpdateDetails DEFAULT_UPDATE_DETAIL = new ItemUpdateDetails();

	private HashMap<String, ItemUpdateDetails> updates = new HashMap<>();
	private List<ItemListUpdateDetails> listOfUpdates = new ArrayList<>();
	
	public void setArrayCheck(String name, IArrayItemCheck arrayCheck) {
		ItemUpdateDetails updateDetails = getCreateGroupUpdate(name);
		if (updateDetails != null) {
			updateDetails.arrayCheck = arrayCheck;
		}
	}
	
	public void setFormatField(String name, IFormatField formatField) {
		ItemUpdateDetails updateDetails = getCreateGroupUpdate(name);
		if (updateDetails != null) {
			updateDetails.formatField = formatField;
		}
	}
	
	public void setRedefineSelection(String name, IRedefineSelection redefineSelection) {
		ItemUpdateDetails updateDetails = getCreateGroupUpdate(name);
		if (updateDetails != null) {
			updateDetails.redefineSelection = redefineSelection;
		}
	}
	
	public void setWriteCheck(String name, IWriteCheck writeCheck) {
		ItemUpdateDetails updateDetails = getCreateGroupUpdate(name);
		if (updateDetails != null) {
			updateDetails.writeCheck = writeCheck;
		}
	}
	
	public void setFormatField(List<String> names, IFormatField formatField) {
		ItemUpdateDetails updateDetails = getCreateGroupUpdate(names);
		if (updateDetails != null) {
			updateDetails.formatField = formatField;
		}
	}
	
	public void setArrayCheck(List<String> names, IArrayItemCheck arrayCheck) {
		ItemUpdateDetails updateDetails = getCreateGroupUpdate(names);
		if (updateDetails != null) {
			updateDetails.arrayCheck = arrayCheck;
		}
	}
	
	public void setRedefineSelection(List<String> names, IRedefineSelection redefineSelection) {
		ItemUpdateDetails updateDetails = getCreateGroupUpdate(names);
		if (updateDetails != null) {
			updateDetails.redefineSelection = redefineSelection;
		}
	}
	
	public void setWriteCheck(List<String> names, IWriteCheck writeCheck) {
		ItemUpdateDetails updateDetails = getCreateGroupUpdate(names);
		if (updateDetails != null) {
			updateDetails.writeCheck = writeCheck;
		}
	}

	private ItemUpdateDetails getCreateGroupUpdate(String name) {
		if (name == null || name.length() == 0) { return null; }
		String nameLC = name.toLowerCase();
		ItemUpdateDetails updateDetails = updates.get(nameLC);
		if (updateDetails == null) {
			updateDetails = new ItemUpdateDetails();
			updates.put(nameLC, updateDetails);
		}
		return updateDetails;
	}

	private ItemUpdateDetails getCreateGroupUpdate(List<String> names) {
		if (names == null || names.size() == 0) { return null; }
		String nameLC = toNamesString(names);
		
		for (ItemListUpdateDetails ld : listOfUpdates) {
			if (ld.groupNameStr.equals(nameLC)) {
				return ld.updateDetails;
			}
		}
		
		ItemListUpdateDetails ld = new ItemListUpdateDetails(names, nameLC);
		listOfUpdates.add(ld);
		
		return ld.updateDetails;
	}
	

	@Override
	public IItemUpdateDetails getUpdateDetails(List<String> names) {
		String lastName;
		if (names == null || names.size() == 0 || (lastName = names.get(names.size() - 1)) == null) { return DEFAULT_UPDATE_DETAIL; }
		String nameLC = toNamesString(names);
		
		for (ItemListUpdateDetails ld : listOfUpdates) {
			if (lastName.equalsIgnoreCase(ld.groupNames[ld.groupNames.length - 1]) 
				&& ld.isMatch(nameLC)) {
				return ld.updateDetails;
			}
		}


		ItemUpdateDetails updateDetails = updates.get(lastName.toLowerCase());
		return updateDetails == null ? DEFAULT_UPDATE_DETAIL : updateDetails;
	}
//	
//	public IArrayItemCheck getArrayCheck(String name) {
//		UpdateDetails ud = getUpdateDetails(name);
//		return ud == null ? null : ud.arrayCheck;
//	}
	
	static String toNamesString(List<String> names) {
		StringBuilder b = new StringBuilder(".");
		for (String name : names) {
			b.append(name).append('.');
		}
		return b.toString().toLowerCase();
	}

	private static class ItemUpdateDetails implements IItemUpdateDetails {
		private IArrayItemCheck arrayCheck;
		private IWriteCheck writeCheck;
		private IFormatField formatField ;
		private IRedefineSelection redefineSelection;
		
		@Override
		public IArrayItemCheck getArrayCheck() {
			return arrayCheck;
		}
		@Override
		public IWriteCheck getWriteCheck() {
			return writeCheck;
		}
		@Override
		public IFormatField getFormatField() {
			return formatField;
		}
		@Override
		public IRedefineSelection getRedefineSelection() {
			return redefineSelection;
		}
	}
	
	private static class ItemListUpdateDetails {
		final String[] groupNames;
		final String groupNameStr;
		final ItemUpdateDetails updateDetails = new ItemUpdateDetails();
		
		public ItemListUpdateDetails(List<String> groupNamesList, String groupNameStr) {
			super();
			this.groupNames = groupNamesList.toArray(new String[groupNamesList.size()]);
			this.groupNameStr = groupNameStr;
		}
		
		boolean isMatch(String namesStr) {
			if (groupNameStr.equals(namesStr)) { return true; }
			int st = 0;
			boolean ok = true;
			for (String n : groupNames) {
				if ((st = namesStr.indexOf( '.' + n.toLowerCase() + '.', st)) < 0) {
					ok = false;
					break;
				}
				st+= 1;
			}
			return ok;
		}
	}
}
