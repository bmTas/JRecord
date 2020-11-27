package  net.sf.JRecord.cg.details.xml;

import java.util.List;


public class  SkelGenDefinition implements  ISkelGenDefinition {


    Skeltons cLayoutSkeltons = new Skeltons();
    Skeltons cRecordSkeltons = new Skeltons();
    Options cOptions = new Options();


    public SkelGenDefinition() {
    }
    
    public SkelGenDefinition(List<Skelton> skels, List<Skelton> recordSkels) {
    	cLayoutSkeltons = getSkeltons(skels);
    	cRecordSkeltons = getSkeltons(recordSkels);
    }
    
    
    private Skeltons getSkeltons(List<Skelton> skels) {
    	Skeltons skeltons = new Skeltons();
    	
    	skeltons.cSkelton = skels;
    	
    	return skeltons;
    }


    public List<Skelton> getLayoutSkeltons() {
	   return cLayoutSkeltons.getSkelton();
	}
    public List<Skelton> getRecordSkeltons() {
	   return cRecordSkeltons.getSkelton();
	}
    

    public Skeltons getLayoutISkeltons() {
	   return cLayoutSkeltons;
	}
    public Skeltons getRecordISkeltons() {
	   return cRecordSkeltons;
	}

    public Options getOptions() {
	   return cOptions;
	}

}