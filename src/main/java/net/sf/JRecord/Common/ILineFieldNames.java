package net.sf.JRecord.Common;

import java.util.Iterator;

public interface ILineFieldNames extends AbstractIndexedLine {

	/**
	 * Get a fields value
	 *
	 * @param fieldName field to retrieve
	 *
	 * @return fields Value
	 *
	 * @deprecated use {@link AbstractLine#getFieldValue(String)}
	 */
	Object getField(String fieldName);

	/**
	 * Get a fields value
	 *
	 * @param fieldName field to retrieve
	 *
	 * @return fields Value
	 */
	AbstractFieldValue getFieldValue(String fieldName);

	/**
	     * Get a fields value
	     *
	     * @param fieldName field to retrieve
	     *
	     * @return fields Value
	     */
	AbstractFieldValue getFieldValueIfExists(String fieldName);

	Iterator<AbstractFieldValue> getFieldIterator();
}