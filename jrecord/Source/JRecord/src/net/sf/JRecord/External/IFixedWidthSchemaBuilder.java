package net.sf.JRecord.External;

public interface IFixedWidthSchemaBuilder extends FixedWidthSchemaBuilders.IByLengthBuilder, FixedWidthSchemaBuilders.IByPositionBuilder {

	public IFixedWidthSchemaBuilder addField(String name, int type, int pos, int length, int decimal);
}
