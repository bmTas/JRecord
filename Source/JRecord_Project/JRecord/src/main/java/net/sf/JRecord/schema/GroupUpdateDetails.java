package net.sf.JRecord.schema;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import net.sf.JRecord.schema.jaxb.interfaces.IFormatField;
import net.sf.JRecord.schema.jaxb.interfaces.IRedefineSelection;
import net.sf.JRecord.schema.jaxb.interfaces.IWriteCheck;

public class GroupUpdateDetails {
	

	private static final UpdateDetails DEFAULT_UPDATE_DETAIL = new UpdateDetails();

	private HashMap<String, UpdateDetails> updates = new HashMap<>();
	private List<GroupListUpdateDetails> listOfUpdates = new ArrayList<>();
	
	public void setArrayCheck(String name, IArrayItemCheck arrayCheck) {
		UpdateDetails updateDetails = getCreateGroupUpdate(name);
		if (updateDetails != null) {
			updateDetails.arrayCheck = arrayCheck;
		}
	}
	
	public void setFormatField(String name, IFormatField formatField) {
		UpdateDetails updateDetails = getCreateGroupUpdate(name);
		if (updateDetails != null) {
			updateDetails.formatField = formatField;
		}
	}
	
	public void setRedefineSelection(String name, IRedefineSelection redefineSelection) {
		UpdateDetails updateDetails = getCreateGroupUpdate(name);
		if (updateDetails != null) {
			updateDetails.redefineSelection = redefineSelection;
		}
	}
	
	public void setWriteCheck(String name, IWriteCheck writeCheck) {
		UpdateDetails updateDetails = getCreateGroupUpdate(name);
		if (updateDetails != null) {
			updateDetails.writeCheck = writeCheck;
		}
	}
	
	public void setFormatField(List<String> names, IFormatField formatField) {
		UpdateDetails updateDetails = getCreateGroupUpdate(names);
		if (updateDetails != null) {
			updateDetails.formatField = formatField;
		}
	}
	
	public void setArrayCheck(List<String> names, IArrayItemCheck arrayCheck) {
		UpdateDetails updateDetails = getCreateGroupUpdate(names);
		if (updateDetails != null) {
			updateDetails.arrayCheck = arrayCheck;
		}
	}
	
	public void setRedefineSelection(List<String> names, IRedefineSelection redefineSelection) {
		UpdateDetails updateDetails = getCreateGroupUpdate(names);
		if (updateDetails != null) {
			updateDetails.redefineSelection = redefineSelection;
		}
	}
	
	public void setWriteCheck(List<String> names, IWriteCheck writeCheck) {
		UpdateDetails updateDetails = getCreateGroupUpdate(names);
		if (updateDetails != null) {
			updateDetails.writeCheck = writeCheck;
		}
	}

	private UpdateDetails getCreateGroupUpdate(String name) {
		if (name == null || name.length() == 0) { return null; }
		String nameLC = name.toLowerCase();
		UpdateDetails updateDetails = updates.get(nameLC);
		if (updateDetails == null) {
			updateDetails = new UpdateDetails();
			updates.put(nameLC, updateDetails);
		}
		return updateDetails;
	}

	private UpdateDetails getCreateGroupUpdate(List<String> names) {
		if (names == null || names.size() == 0) { return null; }
		String nameLC = toNamesString(names);
		
		for (GroupListUpdateDetails ld : listOfUpdates) {
			if (ld.groupNameStr.equals(nameLC)) {
				return ld.updateDetails;
			}
		}
		
		GroupListUpdateDetails ld = new GroupListUpdateDetails(names, nameLC);
		listOfUpdates.add(ld);
		
		return ld.updateDetails;
	}
	

	public UpdateDetails getUpdateDetails(List<String> names) {
		String lastName;
		if (names == null || names.size() == 0 || (lastName = names.get(names.size() - 1)) == null) { return null; }
		String nameLC = toNamesString(names);
		
		for (GroupListUpdateDetails ld : listOfUpdates) {
			if (lastName.equalsIgnoreCase(ld.groupNames[ld.groupNames.length - 1]) 
				&& ld.isMatch(nameLC)) {
				return ld.updateDetails;
			}
		}


		UpdateDetails updateDetails = updates.get(lastName.toLowerCase());
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

	public static class UpdateDetails {
		private IArrayItemCheck arrayCheck;
		private IWriteCheck writeCheck;
		private IFormatField formatField ;
		private IRedefineSelection redefineSelection;
		
		public IArrayItemCheck getArrayCheck() {
			return arrayCheck;
		}
		public IWriteCheck getWriteCheck() {
			return writeCheck;
		}
		public IFormatField getFormatField() {
			return formatField;
		}
		public IRedefineSelection getRedefineSelection() {
			return redefineSelection;
		}
	}
	
	private static class GroupListUpdateDetails {
		final String[] groupNames;
		final String groupNameStr;
		final UpdateDetails updateDetails = new UpdateDetails();
		public GroupListUpdateDetails(List<String> groupNamesList, String groupNameStr) {
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
