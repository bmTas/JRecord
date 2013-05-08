/*
 * @Author Bruce Martin
 * Created on 10/09/2005
 *
 * Purpose:
 *   RecordDecider's are used decide what RecordDetail
 * should be used to display a line (or record)
 */
package net.sf.JRecord.Details;


/**
 * RecordDecider's are used decide which specific RecordDetail
 * should be used to format a line (or data record). It allow
 * you to write Java Code to decide which particular Record Should Be used.
 *
 * @author Bruce Martin
 *
 */
public interface RecordDecider {

    /**
     * Get the prefered Layout
     *
     * @param line to decide what the prefered layout is
     *
     * @return the prefered layout
     */
    public abstract int getPreferedIndex(AbstractLine line);
}
