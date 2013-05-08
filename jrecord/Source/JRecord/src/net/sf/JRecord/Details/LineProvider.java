/*
 * @Author Bruce Martin
 * Created on 29/08/2005
 *
 * Purpose:
 */
package net.sf.JRecord.Details;




/**
 * A <b>LineProvider</b> creates lines for the calling program.
 * By creating your own <b>LineProvider</b>, you can use your
 * own <b>Line's</b> rather than the System <b>Line</b> / XmlLine class.
 *
 * @author Bruce Martin
 *
 */
public interface LineProvider {

    /**
     * Create a null line
     *
     * @param recordDescription record description
     *
     * @return new line
     */
    public abstract AbstractLine getLine(LayoutDetail recordDescription);


    /**
     * Line Providers provide lines to the calling program
     *
     * @param recordDescription record layout details
     * @param linesText text to create the line from
     *
     * @return line
     */
    public abstract AbstractLine getLine(LayoutDetail recordDescription, String linesText);

    /**
     * Build a Line
     *
     * @param recordDescription record layout details
     * @param lineBytes bytes to create the line from
     *
     * @return line
     */
    public abstract AbstractLine getLine(LayoutDetail recordDescription, byte[] lineBytes);
}