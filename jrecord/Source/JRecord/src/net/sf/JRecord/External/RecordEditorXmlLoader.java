package net.sf.JRecord.External;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.XmlConstants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.Def.AbstractConversion;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalGroupSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.IO.XmlLineReader;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.Convert;

/**
 * Class to Load a RecordLayout (Record or Line Description)
 * from an XML file (RecordEditor-XML)
 *
 * <pre>
 *   <b>Usage:</b>
 *        CopybookLoader loader = new RecordEditorXmlLoader();
 *        LayoutDetail layout = loader.loadCopyBook(copybookName, 0, 0, "", 0, 0, null).asLayoutDetail();
 * </pre>
 *
 * @author Bruce Martin
 *
 */
public class RecordEditorXmlLoader implements CopybookLoader {

	/**
	 * Load the Copybook
	 * @see CopybookLoader#loadCopyBook(String, int, int, String, int, int, AbsSSLogger)
	 */
	@Override
	public ExternalRecord loadCopyBook(String copyBookFile,
		int splitCopybookOption, int dbIdx, String font, int binFormat,
		int systemId, AbsSSLogger log) throws Exception {

        int rt = Constants.rtRecordLayout;
        if (binFormat == Convert.FMT_MAINFRAME) {
            rt = Constants.rtBinaryRecord;
        }

        ExternalRecord rec = ExternalRecord.getNullRecord(
        		Conversion.getCopyBookId(copyBookFile),
                rt,
                font);
        rec.setNew(true);

        XmlLineReader r = new XmlLineReader(true);
		r.open(copyBookFile);
		insertRecord(log, null, rec, r, dbIdx);
        r.close();

        return rec;
	}

	public ExternalRecord loadCopyBook(InputStream is, String layoutName) throws Exception {

	        int rt = Constants.rtRecordLayout;

	        ExternalRecord rec = ExternalRecord.getNullRecord(
	        		layoutName,
	                rt,
	                "");
	        rec.setNew(true);

	        XmlLineReader r = new XmlLineReader(true);
			r.open(is, (LayoutDetail) null);
			insertRecord(new TextLog(), null, rec, r, AbstractConversion.USE_DEFAULT_IDX);
	        r.close();

	        return rec;
	}


	/**
	 * Add  an External-Record (Sub-Record) to an External-Record
	 * @param log log to write any errors that occur
	 * @param parentRec parent Record
	 * @param childRec Child Record
	 * @param reader line reader to read record Details from.
	 * @param dbIdx Database index
	 *
	 * @return wether Record was inserted or not
	 *
	 * @throws Exception any error that occurs
	 */
	private boolean insertRecord(AbsSSLogger log, ExternalRecord parentRec, ExternalRecord childRec,
			XmlLineReader reader, int dbIdx)
	throws Exception {

		AbstractLine line = reader.read();
		String name;

		if (line == null || (name= line.getFieldValue(XmlConstants.XML_NAME).asString()).startsWith("/")) {
			//System.out.println("Exit Found " + name);
			return false;
		}

		if (XmlConstants.XML_START_DOCUMENT.equalsIgnoreCase(name)) {
			line = reader.read();
			name = line.getFieldValue(XmlConstants.XML_NAME).asString();
		}

		if (Constants.RE_XML_RECORD.equalsIgnoreCase(name)) {
			childRec.setRecordName(line.getFieldValue(Constants.RE_XML_RECORDNAME).asString());
			childRec.setCopyBook(line.getFieldValue(Constants.RE_XML_COPYBOOK).asString());
			childRec.setDelimiter(line.getFieldValue(Constants.RE_XML_DELIMITER).asString());
			childRec.setDescription(line.getFieldValue(Constants.RE_XML_DESCRIPTION).asString());
			childRec.setFileStructure(ExternalConversion.getFileStructure(dbIdx,
					line.getFieldValue(Constants.RE_XML_FILESTRUCTURE).asString()));
			childRec.setFontName(line.getFieldValue(Constants.RE_XML_FONTNAME).asString());
			childRec.setListChar(line.getFieldValue(Constants.RE_XML_LISTCHAR).asString());
			childRec.setParentName(line.getFieldValue(Constants.RE_XML_PARENT).asString());
			childRec.setQuote(line.getFieldValue(Constants.RE_XML_QUOTE).asString());
			childRec.setRecordStyle(ExternalConversion.getRecordStyle(dbIdx,
					line.getFieldValue(Constants.RE_XML_STYLE).asString()));
			childRec.setRecordType(ExternalConversion.getRecordType(dbIdx,
					line.getFieldValue(Constants.RE_XML_RECORDTYPE).asString()));
			childRec.setEmbeddedCr(
					"Y".equalsIgnoreCase(
							line.getFieldValue(Constants.RE_XML_EMBEDDED_CR).asString()));
			//rec.setSystem((int) line.getFieldValue(Constants.RE_XML_COPYBOOK).asLong());


			ExternalFieldSelection fs = new ExternalFieldSelection();
			fs.setFieldName(line.getFieldValue(Constants.RE_XML_TESTFIELD).asString());
			fs.setFieldValue(line.getFieldValue(Constants.RE_XML_TESTVALUE).asString());

			if ("".equals(fs.getFieldName())) {
				if ("*".equals(fs.getFieldValue())) {
					childRec.setDefaultRecord(true);
				}
			} else {
				childRec.setRecordSelection(fs);
			}

			try {
				AbstractFieldValue val = line.getFieldValue(Constants.RE_XML_LINE_NO_FIELD_NAME);
				if (parentRec == null && val != null && ! "".equals(val.asString())) {
					childRec.setLineNumberOfFieldNames(val.asInt());
				}
			} catch (Exception e) {
			}

			if (parentRec != null) {
				parentRec.addRecord(childRec);
			}
		} else {
			String s = "Error loading copybook; Expected " + Constants.RE_XML_RECORD + " Tag, but got " + name;
			log.logMsg(AbsSSLogger.ERROR, s);
			throw new RuntimeException(s);
		}

		line = reader.read();

		if (Constants.RE_XML_RECORDS.equalsIgnoreCase(line.getFieldValue(XmlConstants.XML_NAME).asString())) {
			ExternalRecord newRec;
			childRec.setRecordType(Constants.rtGroupOfRecords);

			do {
				newRec = ExternalRecord.getNullRecord(
		        		"",
		        		Constants.rtRecordLayout,
		                childRec.getFontName());
				newRec.setNew(true);
				//rec.addRecord(newRec);
			} while (insertRecord(log, childRec, newRec, reader, dbIdx));
			childRec.setParentsFromName();

			line = reader.read();
		}

		line = addFieldTsts(childRec, reader, line, dbIdx);
		line = addFields(childRec, reader, line, dbIdx);
		line = addFieldTsts(childRec, reader, line, dbIdx);

		return true;
	}


	private AbstractLine addFields(
			ExternalRecord childRec,
			XmlLineReader reader,
			AbstractLine line,
			int dbIdx)
	throws IOException {

		if (Constants.RE_XML_FIELDS.equalsIgnoreCase(line.getFieldValue(XmlConstants.XML_NAME).asString())) {
			ExternalField fld;
			String fldName, s, cobolName;
			int decimal, len;
			line = reader.read();
			while (line != null && ! line.getFieldValue(XmlConstants.XML_NAME).asString().startsWith("/")) {
				s = null;
				cobolName = "";
				try {
					decimal = getIntFld(line, Constants.RE_XML_DECIMAL, 0);
					len     = getIntFld(line, Constants.RE_XML_LENGTH, Constants.NULL_INTEGER);
					fldName = line.getFieldValue(Constants.RE_XML_NAME).asString();
					try {
						s =line.getFieldValue(Constants.RE_XML_DEFAULT).asString();
						cobolName =line.getFieldValue(Constants.RE_XML_COBOLNAME).asString();
					} catch (Exception e) {}
					fld = new ExternalField(
						getIntFld(line, Constants.RE_XML_POS),
						len,
						fldName,
						line.getFieldValue(Constants.RE_XML_DESCRIPTION).asString(),
						ExternalConversion.getType(dbIdx,
								line.getFieldValue(Constants.RE_XML_TYPE).asString()),
						decimal,
		                ExternalConversion.getFormat(dbIdx,
								line.getFieldValue(Constants.RE_XML_CELLFORMAT).asString()),
						line.getFieldValue(Constants.RE_XML_PARAMETER).asString(),
						line.getFieldValue(Constants.RE_XML_DEFAULT).asString(),
		                cobolName,
		                0
					);

					if (! "".equals(s)) {
						fld.setDefault(s);
					}
					childRec.addRecordField(fld);
				} catch (Exception e) { }

				line = reader.read();
			}
			line = reader.read();
		}
		return line;
	}


	private AbstractLine addFieldTsts(
			ExternalRecord childRec,
			XmlLineReader reader,
			AbstractLine line,
			int dbIdx)
	throws IOException {
		String name = line.getFieldValue(XmlConstants.XML_NAME).asString();
		if (Constants.RE_XML_TST_FIELDS.equalsIgnoreCase(name)
		||  Constants.RE_XML_AND_FIELDS.equalsIgnoreCase(name)) {
			setDefault(childRec, line.getFieldValue(Constants.RE_XML_DEFAULTREC).asString());
			childRec.setRecordSelection(getGroup(reader, ExternalSelection.TYPE_AND));

			line = reader.read();
		} else if (Constants.RE_XML_OR_FIELDS.equalsIgnoreCase(name)) {
			setDefault(childRec, line.getFieldValue(Constants.RE_XML_DEFAULTREC).asString());
			childRec.setRecordSelection(getGroup(reader, ExternalSelection.TYPE_OR));

			line = reader.read();
		}
		return line;
	}

	private void setDefault(ExternalRecord childRec, String s) {
		if (s != null && "Y".equalsIgnoreCase(s)) {
			childRec.setDefaultRecord(true);
		}
	}


	private ExternalGroupSelection<ExternalSelection> getGroup(XmlLineReader reader, int type) throws IOException {

		ExternalGroupSelection<ExternalSelection> g = new ExternalGroupSelection<ExternalSelection>();
		String name;

		g.setType(type);

		AbstractLine line = reader.read();
		while (line != null && ! line.getFieldValue(XmlConstants.XML_NAME).asString().startsWith("/")) {
			name = line.getFieldValue(XmlConstants.XML_NAME).asString();
			try {
				if (Constants.RE_XML_TST_FIELD.equalsIgnoreCase(name)) {
					g.add(new ExternalFieldSelection(
						line.getFieldValue(Constants.RE_XML_NAME).asString(),
						line.getFieldValue(Constants.RE_XML_VALUE).asString(),
						line.getFieldValue(Constants.RE_XML_OPERATOR).asString()));
				} else if (Constants.RE_XML_AND_FIELDS.equalsIgnoreCase(name)) {
					g.add(getGroup(reader, ExternalSelection.TYPE_AND));
				} else if (Constants.RE_XML_OR_FIELDS.equalsIgnoreCase(name)) {
					g.add(getGroup(reader, ExternalSelection.TYPE_OR));
				}
			} catch (Exception e) { }

			line = reader.read();
		}

		return g;

	}

	/**
	 * Get integer field from the line
	 * @param line line to access
	 * @param fldName field name required
	 * @return requested field
	 * @throws Exception any error
	 */
	private int getIntFld(AbstractLine line, String fldName) throws Exception {
		try {
			return line.getFieldValue(fldName).asInt();
		} catch (Exception e) {
			System.out.println("Error getting field " + fldName + ": " + e.getMessage() );
			throw(e);
		}
	}


	/**
	 * Get integer field from the line
	 * @param line line to access
	 * @param fldName field name required
	 * @return requested field
	 * @throws Exception any error
	 */
	private int getIntFld(AbstractLine line, String fldName, int defaultValue)  {
		int ret = defaultValue;
		AbstractFieldValue value = line.getFieldValue(fldName);
		if (value != null && !"".equals(value.asString())) {
			try {
				ret = value.asInt();
			} catch (Exception e) {
			}
		}
		return ret;
	}

	public static ExternalRecord getExternalRecord(String xml, String name) throws Exception {
		ByteArrayInputStream bs = new ByteArrayInputStream(xml.getBytes());

		RecordEditorXmlLoader loader = new RecordEditorXmlLoader();

		return loader.loadCopyBook(bs, name);
	}
}
