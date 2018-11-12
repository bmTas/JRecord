/*
1 * Purpose: Record oriented reading of Variable Record Length Binary files
 *
 * @Author Bruce Martin
 * Created on 27/08/2005
 *
 */
/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.ByteIO;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;




/**
 * This class performs record oriented reading of:
 * </ul compact>
 *   <li>Mainframe Variable Record Length Binary files
 *   <li>Mainframe Variable Record Length Binary files with imbedded block lengths
 * </ul>
 *
 * into Array of Bytes
 *
 * @author Bruce Martin
 * @version 0.55
 *
 */
public class VbByteReader extends AbstractByteReader {

	private static final int LAST_7_BITS_SET = 127;
	
	public static final int MODE_NO_BLOCK_LENGTH = 0;
	public static final int MODE_ORIG_BLOCK_LENGTH = 1;
	public static final int MODE_BLOCK_LENGTH_2 = 2;


    private InputStream inStream;
	private BufferedInputStream stream = null;

	private int lineNumber = 0;
	private int rdwAdjust = 0;


	/**
	 * record descriptor word, it consists of
	 * 2 bytes length
	 * 2 bytes (hex zero)
	 */
	private byte[] rdw = new byte[4];
//	private byte[] rdwLength = new byte[2];


	private int blockMode = MODE_NO_BLOCK_LENGTH;
	private byte[] bdw = new byte[4];
//	private byte[] bdwLength = new byte[2];


	private int blockLength = -1;
	private int bytesReadFromBlock = 0;



	/**
	 * This class provides record oriented reading of Variable
	 * Record Length Binary files where the record length is held in
	 * 4 byte Record-Descriptor-Word at the start of the record.
	 */
	public VbByteReader() {
	    this(MODE_NO_BLOCK_LENGTH, true);
	}


	/**
	 * This class provides record oriented reading of Variable
	 * Record Length Binary files where the record length is held in
	 * 4 byte Record-Descriptor-Word at the start of the record.
	 *
	 * @param includesBlockLength weather the file contains block length
	 * (ie it is a Dump of the Mainframe VB file)
	 */
	public VbByteReader(final boolean includesBlockLength) {
	    this(includesBlockLength, true);
	}

	public VbByteReader(final boolean includesBlockLength, boolean lengthIncludesRDW) {
		this(includesBlockLength ? MODE_ORIG_BLOCK_LENGTH : MODE_NO_BLOCK_LENGTH, lengthIncludesRDW);
	}

	public VbByteReader(final int includesBlockLength, boolean lengthIncludesRDW) {
	    super();

	    blockMode = includesBlockLength;
	    if (lengthIncludesRDW) {
	    	rdwAdjust = 4;
	    }
	}

	/**
     * @see AbstractByteReader#open(java.io.InputStream)
     */
    public void open(InputStream inputStream) {

        inStream = inputStream;

        if (inputStream instanceof BufferedInputStream) {
        	stream = (BufferedInputStream) inputStream;
        } else {
        	stream = new BufferedInputStream(inputStream, BUFFER_SIZE);
        }

        checkForBlockLength();
    }


    /**
     * @see AbstractByteReader#read()
     */
    public byte[] read()  throws IOException {
        byte[] ret = null;

        if (stream == null) {
            throw new IOException(AbstractByteReader.NOT_OPEN_MESSAGE);
        }

        checkForBlockLength();

        lineNumber += 1;
        if (readBuffer(stream, rdw) > 0) {
//            rdwLength[0] = rdw[0];
//            rdwLength[1] = rdw[1];
//
//        	int lineLength = (new BigInteger(rdwLength)).intValue() - rdwAdjust;
         	int lineLength = ((rdw[0] & 0xFF) << 8) + (rdw[1] & 0xFF) - rdwAdjust;
            if (rdw[2] != 0  || rdw[3] != 0) {
//            	System.out.println();
//            	System.out.println("-----------------------------------------------------------------");
//               	System.out.println();
//               	System.out.println(rdw[0] + " " + rdw[1] + " " + rdw[2] + " " + rdw[3]
//               			+ " \t " + blockMode + " && " + bytesReadFromBlock + " >= " + blockLength);
              	byte[] b = new byte[256];
            	readBuffer(stream, b);
//            	String string = new String(b, "cp037");
//    			System.out.print(string);
//    			 System.out.println();
//    			 System.out.println();
//    			for (int i = 0; i < b.length; i++) {
//    				if (i % 8 == 0) System.out.println();
//    				System.out.print("\t>" + b[i] + "\t" + string.charAt(i));
//    			}
//    			 System.out.println();
//    			 System.out.println();
//              if ((rdw[2] != 0 &&  rdw[2] != 1 && rdw[2] != 2) || rdw[3] != 0) {
                throw new IOException(
                          "Invalid Record Descriptor word at line "
                        + lineNumber + " " + lineLength + "\t" + rdw[2] + " " + rdw[3]
                      );
            }

        	if (lineLength < 0) {
        		throw new IOException("Invalid Line Length: " + lineLength + " For line " + lineNumber); 
        	}
            byte[] inBytes = new byte[lineLength];

            if (readBuffer(stream, inBytes) >= 0) {
                ret = inBytes;

                if (blockMode > 0) {
                	bytesReadFromBlock += lineLength + 4;
                }
			}
        }

        return ret;
    }


    /**
     * reads the blocklength if necessary
     */
    private void checkForBlockLength() {

        if (blockMode > 0 && bytesReadFromBlock >= blockLength) {
        	try {
    			bytesReadFromBlock = 4;
        		if ((readBuffer(stream, bdw) > 0)) {
        			if (bdw[0] >= 0 || blockMode == MODE_BLOCK_LENGTH_2) {
//        	            bdwLength[0] = bdw[0];
//        	            bdwLength[1] = bdw[1];
//
//        	        	blockLength = (new BigInteger(bdwLength)).intValue();
        	        	blockLength = ((bdw[0] & 0xFF) << 8) + (bdw[1] & 0xFF);
//        	        	System.out.println("BlkLength 1  >> " + blockLength);
        			} else {
//       				int bl =  ((bdw[0] & 0xFF) << 8) + (bdw[1] & 0xFF);
//        	        	System.out.println("~~~" + bdw[0] + ", " + bdw[1] + ", "+ bdw[02] + ", "+ bdw[03] + " ~~ "
//        	        			+ bl);
        				bdw[0] &= LAST_7_BITS_SET;
        				blockLength = ((bdw[0] & 0xFF) << 24) + ((bdw[1] & 0xFF) << 16)
								+ ((bdw[2] & 0xFF) << 8) + ((bdw[3] & 0xFF));
//       	        	blockLength = (new BigInteger(bdw)).intValue();
//        	        	System.out.println("BlkLength 2  >> " + blockLength
//        	        			+ " ~ " + (((bdw[0] & 0xFF) << 8) + (bdw[1] & 0xFF) ));
//        	        	System.out.println("~~~" + bdw[0] + ", " + bdw[1] + ", "+ bdw[02] + ", "+ bdw[03] + ", ");
        			}
        		}
        	} catch (Exception e) {
				e.printStackTrace();
			}
        }
    }

    /**
     * @see AbstractByteReader#close()
     */
    public void close() throws IOException {

        inStream.close();
        stream = null;
    }

}
