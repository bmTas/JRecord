package net.sf.JRecord.cg.details.xml;

import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

public class  ReadSkelGenDefinition {

    private final XMLStreamReader reader;
    
    public ReadSkelGenDefinition(XMLStreamReader reader) {
        this.reader = reader;
    }
    

    public SkelGenDefinition readXml() throws XMLStreamException {
    
    
   
        int type = reader.getEventType();
        String name;
        ReaderXml rx = new ReaderXml(reader);
        IProcessXmlTag processTheTag = null;
        SkelGenDefinition ret = null;
        
        while (type != XMLStreamConstants.END_DOCUMENT) {
            switch (type) {
            case (XMLStreamConstants.START_ELEMENT) :
                name = reader.getName().toString().toLowerCase();
            
                if (processTheTag == null) {
                   if (name.equalsIgnoreCase("SkelGenDefinition")) {
                       ProcessSkelGenDefinition pt = new ProcessSkelGenDefinition(null, rx);
                       processTheTag = pt;
                       ret = pt.itm;
                   }
                } else {
                      IProcessXmlTag newProcessTag = processTheTag.processTag(name, reader);
                      processTheTag = newProcessTag == null ? new ProcessXmlTag(processTheTag) : newProcessTag;
                }
                break;
            case (XMLStreamConstants.END_ELEMENT):
                if (processTheTag != null) {
                    processTheTag = processTheTag.getParent();
                    if (processTheTag  == null) {
                        return ret;
                    }
                }
                break;
            }
            type = reader.next();
        }
        return ret;
    }

     
    private static interface IProcessXmlTag {
       IProcessXmlTag getParent();
       
       IProcessXmlTag processTag(String name, XMLStreamReader reader);
   }

   
   private static class ProcessXmlTag implements IProcessXmlTag {
       private final IProcessXmlTag parent;
       public ProcessXmlTag(IProcessXmlTag parent) {
           super();
           this.parent = parent;
       }

       @Override
       public IProcessXmlTag getParent() {
           return parent;
       }

       @Override
       public IProcessXmlTag processTag(String name, XMLStreamReader reader) {
           return new ProcessXmlTag(this);
       }
   }


    private static class ProcessSkelGenDefinition extends ProcessXmlTag {
        private final SkelGenDefinition itm = new SkelGenDefinition();
        private final ReaderXml readerXml;
        public ProcessSkelGenDefinition(IProcessXmlTag parent, ReaderXml readerXml) {
            super(parent);
            
            this.readerXml = readerXml;
        }
    

       @Override
       public IProcessXmlTag processTag(String name, XMLStreamReader reader) {
           IProcessXmlTag ret = null;

           switch (name) {
           case "layoutskeltons": 
               ProcessSkeltons rLayoutSkeltons = new ProcessSkeltons (this, readerXml);
               ret  = rLayoutSkeltons;
               itm.cLayoutSkeltons = rLayoutSkeltons.itm;
               break;
           case "recordskeltons": 
               ProcessSkeltons rRecordSkeltons = new ProcessSkeltons (this, readerXml);
               ret  = rRecordSkeltons;
               itm.cRecordSkeltons = rRecordSkeltons.itm;
               break;
           case "options": 
               ProcessOptions rOptions = new ProcessOptions (this, readerXml);
               ret  = rOptions;
               itm.cOptions = rOptions.itm;
               break;
           }
           
           return ret;
       }
    }

    private static class ProcessSkeltons extends ProcessXmlTag {
        private final Skeltons itm = new Skeltons();
        private final ReaderXml readerXml;
        public ProcessSkeltons(IProcessXmlTag parent, ReaderXml readerXml) {
            super(parent);
            
            this.readerXml = readerXml;
        }
    

       @Override
       public IProcessXmlTag processTag(String name, XMLStreamReader reader) {
           IProcessXmlTag ret = null;

           switch (name) {
           case "skelton": 
               ProcessSkelton rSkelton = new ProcessSkelton (this, readerXml);
               ret  = rSkelton;
               itm.cSkelton.add(rSkelton.itm);
               break;
           }
           
           return ret;
       }
    }

    private static class ProcessSkelton extends ProcessXmlTag {
        private final Skelton itm = new Skelton();
        private final ReaderXml readerXml;
        public ProcessSkelton(IProcessXmlTag parent, ReaderXml readerXml) {
            super(parent);
            
            this.readerXml = readerXml;
	        itm.description = readerXml.getString("description");
	        itm.output = readerXml.getString("output");
	        itm.template = readerXml.getString("template");
        }
    

       @Override
       public IProcessXmlTag processTag(String name, XMLStreamReader reader) {
           IProcessXmlTag ret = null;

           
           return ret;
       }
    }

    private static class ProcessOptions extends ProcessXmlTag {
        private final Options itm = new Options();
        private final ReaderXml readerXml;
        public ProcessOptions(IProcessXmlTag parent, ReaderXml readerXml) {
            super(parent);
            
            this.readerXml = readerXml;
        }
    

       @Override
       public IProcessXmlTag processTag(String name, XMLStreamReader reader) {
           IProcessXmlTag ret = null;

           switch (name) {
           case "option": 
               ProcessOption rOption = new ProcessOption (this, readerXml);
               ret  = rOption;
               itm.cOption.add(rOption.itm);
               break;
           }
           
           return ret;
       }
    }

    private static class ProcessOption extends ProcessXmlTag {
        private final Option itm = new Option();
        private final ReaderXml readerXml;
        public ProcessOption(IProcessXmlTag parent, ReaderXml readerXml) {
            super(parent);
            
            this.readerXml = readerXml;
	        itm.value = readerXml.getString("Value");
	        itm.id = readerXml.getString("id");
        }
    

       @Override
       public IProcessXmlTag processTag(String name, XMLStreamReader reader) {
           IProcessXmlTag ret = null;

           
           return ret;
       }
    }

	private static class ReaderXml {
		final XMLStreamReader reader; 
		
		public ReaderXml(XMLStreamReader reader) {
			super();
			this.reader = reader;

		}
		
		public String getString(String localName) {
			return reader.getAttributeValue(null, localName);
		}
		
//		public Integer getInteger(String localName) {
//			String s = getString(localName);
//			if (s != null && s.length() > 0) {
//				return Integer.valueOf(s);
//			}
//			return null;
//		}
//
//				
//		public Double getDouble(String localName) {
//			String s = getString(localName);
//			if (s != null && s.length() > 0) {
//				return Double.valueOf(s);
//			}
//			return null;
//		}
//				
//		public Float getFloat(String localName) {
//			String s = getString(localName);
//			if (s != null && s.length() > 0) {
//				return Float.valueOf(s);
//			}
//			return null;
//		}
//
//		public boolean getboolean(String localName) {
//			String s = getString(localName);
//			return "true".equalsIgnoreCase(s);
//		}
	}

}
