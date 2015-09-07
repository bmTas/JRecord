/**
 * 
 */
package net.sf.JRecord.External;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Log.AbsSSLogger;

/**
 * @author Bruce Martin
 *
 */
public abstract class BaseCopybookLoader implements CopybookLoader {

	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.CopybookLoader#loadCopyBook(java.lang.String, int, int, java.lang.String, int, int, net.sf.JRecord.Log.AbsSSLogger)
	 */
	@Override
	public final ExternalRecord loadCopyBook(String copyBookFile,
			int splitCopybookOption, int dbIdx, String font, int binFormat,
			int systemId, AbsSSLogger log) throws Exception {
		// TODO Auto-generated method stub
		return loadCopyBook(copyBookFile, splitCopybookOption, dbIdx, font, CommonBits.getDefaultCobolTextFormat(), binFormat, systemId, log);
		
	}

}
