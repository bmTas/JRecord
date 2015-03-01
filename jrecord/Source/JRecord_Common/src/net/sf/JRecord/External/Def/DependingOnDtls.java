package net.sf.JRecord.External.Def;

public final class DependingOnDtls {
	public final DependingOn dependingOn;
	public final int index;
	public final DependingOnDtls parent;
	public final boolean firstIdx;
	
	
	public DependingOnDtls(DependingOn dependingOn, int index,
			DependingOnDtls parent) {
		super();
		this.dependingOn = dependingOn;
		this.index = index;
		this.parent = parent;
		this.firstIdx = index == 0 
				     && (parent == null || parent.firstIdx);
	}
	
	
	public DependingOnDtls[] getTree() {
		return getTree(1);
	}
	
	
	private DependingOnDtls[] getTree(int lvl) {
		DependingOnDtls[] ret;
		if (parent == null) {
			ret = new DependingOnDtls[lvl];
		} else {
			ret = parent.getTree(lvl + 1);
		}
		ret[ret.length - lvl] = this;
		return ret;
	}

}
