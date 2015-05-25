package carma.CGT8;
import org.cmg.ml.sam.sim.SimulationFactory;

import eu.quanticol.carma.simulator.CarmaSystem;
public class CGT8Factory implements SimulationFactory<CarmaSystem> {

	public CGT8Factory() {
		
	}
	
	@Override
	public CarmaSystem getModel() {
		return new Simple();
	}

}
