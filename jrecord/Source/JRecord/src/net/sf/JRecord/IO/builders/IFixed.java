package net.sf.JRecord.IO.builders;

import net.sf.JRecord.def.IO.builders.IDefineFixedFieldsByLength;
import net.sf.JRecord.def.IO.builders.IDefineFixedFieldsByPosition;
import net.sf.JRecord.def.IO.builders.IFixedWidthIOBuilder;

public interface IFixed  extends IFixedWidthIOBuilder, IDefineFixedFieldsByPosition, IDefineFixedFieldsByLength {

}
