/*
 * @Author Bruce Martin
 * Created on 18/03/2007
 *
 * Purpose:
 */
package net.sf.JRecord.zTest.Common;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.math.BigInteger;

import net.sf.JRecord.ByteIO.AbstractByteWriter;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.IO.AbstractLineWriter;

/**
 *
 *
 * @author Bruce Martin
 *
 */
public class IO {

    /**
     *
     */
    private IO() {
        super();
    }

    /**
     * writes byte array to a fixed record length file
     *
     * @param name major part of the file name
     * @param bytes data to write to the file
     *
     * @throws IOException any IO errors
     */
    public static void writeFbFile(String name, byte[][] bytes)
    throws IOException  {
        int i;
        FileOutputStream f = new FileOutputStream(name);
        BufferedOutputStream outputStream = new BufferedOutputStream(f);
        System.out.print("writeAFile " + bytes.length);

		for (i = 0; i < bytes.length; i++) {
		    outputStream.write(bytes[i]);
		}

		System.out.println("writeAFile end loop");

		outputStream.close();

        f.close();
//        System.out.println("writeAFile exit");
    }


    /**
     * writes byte array to a  VB file
     *
     * @param name major part of the file name
     * @param bytes data to write to the file
     *
     * @throws IOException any IO errors
     */
    public static void writeVbFile(String name, byte[][] bytes)
    throws IOException  {
        int i;
        byte[] len = new byte[4];
        FileOutputStream f = new FileOutputStream(name);
        BufferedOutputStream outputStream = new BufferedOutputStream(f);
        //System.out.print("writeAFile " + bytes.length);
        len[0] = 0;
        len[2] = 0;
        len[3] = 0;

		for (i = 0; i < bytes.length; i++) {
		    len[1] = (byte) (bytes[i].length + 4);
		    outputStream.write(len);
		    outputStream.write(bytes[i]);
		}

		outputStream.close();

        f.close();
        //System.out.println("writeAFile exit");
    }

    private static final int NUMBER_IN_BLOCK = 30;
    /**
     * writes byte array to a  VB Dump file
     *
     * @param name major part of the file name
     * @param bytes data to write to the file
     *
     * @throws IOException any IO errors
     */
    public static void writeVbDumpFile(String name, byte[][] bytes)
    throws IOException  {
        int i, j, k, blockLength;
        byte[] tmp;
        byte[] len = new byte[4];
        byte[] bdw = new byte[4];
        FileOutputStream f = new FileOutputStream(name);
        BufferedOutputStream outputStream = new BufferedOutputStream(f);
        //System.out.print("writeAFile " + bytes.length);
        len[0] = 0;
        len[2] = 0;
        len[3] = 0;
        bdw[2] = 0;
        bdw[3] = 0;

		for (i = 0; i < bytes.length; i++) {
		    if (i % NUMBER_IN_BLOCK == 0) {
		        blockLength = 0;
		        for (j = i; j < bytes.length && j < i + NUMBER_IN_BLOCK; j++) {
		            blockLength += bytes[j].length + 4;
		        }
		        tmp = BigInteger.valueOf(blockLength).toByteArray();
		        k = 1;
		        bdw[0] = 0;
		        bdw[1] = 0;
		        for (j = tmp.length - 1; j >= 0 && k >= 0; j--) {
		            bdw[k--] = tmp[j];
		        }
			    outputStream.write(bdw);
			    if (i < 5) {
			       System.out.println(">> " + blockLength + " " + bdw[0] + " " + bdw[1]);
			    }
		    }
		    len[1] = (byte) (bytes[i].length + 4);
		    outputStream.write(len);
		    outputStream.write(bytes[i]);
		}

		outputStream.close();

        f.close();
        //System.out.println("writeAFile exit");
    }


    /**
     * writes byte array to a  VB file
     *
     * @param name major part of the file name
     * @param bytes data to write to the file
     *
     * @throws IOException any IO errors
     */
    public static void writeFujitsuVbFile(String name, byte[][] bytes)
    throws IOException  {
        int i;
        byte[] len = new byte[4];
        FileOutputStream f = new FileOutputStream(name);
        BufferedOutputStream outputStream = new BufferedOutputStream(f);
        //System.out.print("writeAFile " + bytes.length);
        len[0] = 0;
        len[1] = 0;
        len[2] = 0;
        len[3] = 0;

		for (i = 0; i < bytes.length; i++) {
		    len[0] = (byte) (bytes[i].length);

		    outputStream.write(len);
		    outputStream.write(bytes[i]);
		    outputStream.write(len);
		}

		outputStream.close();

        f.close();
        //System.out.println("writeAFile exit");
    }



    /**
     * writes byte array to a file
     *
     * @param name major part of the file name
     * @param bytes data to write to the file
     * @param details file layout details
     *
     * @throws IOException any IO errors
     */
    public static void writeAFile(AbstractByteWriter writer, String name,
            byte[][] bytes)
    throws IOException  {
        int i;
        writer.open(name);

        for (i = 0; i < bytes.length; i++) {
            writer.write(bytes[i]);
        }
        writer.close();
        System.out.println("Written ... " + bytes.length);
    }


    /**
     * writes byte array to a file
     *
     * @param name major part of the file name
     * @param bytes data to write to the file
     * @param details file layout details
     *
     * @throws IOException any IO errors
     */
    public static void writeAFile(AbstractLineWriter writer, String name, byte[][] bytes, LayoutDetail details)
    throws IOException  {
        int i;

        writer.open(name);

        for (i = 0; i < bytes.length; i++) {
            writer.write(new Line(details, bytes[i]));
        }
        writer.close();
    }


    /**
     * writes byte array to a file
     *
     * @param name major part of the file name
     * @param line data to write to the file
     * @param eol end of line character string
     *
     * @throws IOException any IO errors
     */
    public static void writeAFile(String name, String[] line, String eol)
    throws IOException  {
        int i;
        FileWriter f = new FileWriter(name);

		for (i = 0; i < line.length; i++) {
		    f.write(line[i]);
		    f.write(eol);
		}

        f.close();
    }

}
