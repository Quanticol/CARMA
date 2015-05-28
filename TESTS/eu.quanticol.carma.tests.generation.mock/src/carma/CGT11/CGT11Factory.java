package carma.CGT11;
import org.cmg.ml.sam.sim.*;
import eu.quanticol.carma.simulator.*;
public class CGT11Factory implements SimulationFactory<CarmaSystem> {

	public CGT11Factory() {
		
	}
	
	@Override
	public CarmaSystem getModel() {
		return new Simple();
	}

}
