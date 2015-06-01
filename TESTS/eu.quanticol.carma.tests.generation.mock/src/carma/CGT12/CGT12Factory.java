package carma.CGT12;
import org.cmg.ml.sam.sim.*;
import eu.quanticol.carma.simulator.*;
public class CGT12Factory implements SimulationFactory<CarmaSystem> {

	public CGT12Factory() {
		
	}
	
	@Override
	public CarmaSystem getModel() {
		return new Simple();
	}

}
