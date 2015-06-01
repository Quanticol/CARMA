package carma.CGT13;
import org.cmg.ml.sam.sim.*;
import eu.quanticol.carma.simulator.*;
public class CGT13Factory implements SimulationFactory<CarmaSystem> {

	public CGT13Factory() {
		
	}
	
	@Override
	public CarmaSystem getModel() {
		return new Simple();
	}

}
