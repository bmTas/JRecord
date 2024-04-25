package net.sf.JRecord.External.base;

import java.util.Map;

public interface ILoadCopybook<XRecord extends BaseExternalRecord<XRecord>> {
	XRecord loadCopybook(
			String fileName,
			int dbIndex, 
			int systemId,
			String language, 
			int binaryFormat, 
			int fileStructure,
			Map<String, String> attributes) throws Exception;
}
