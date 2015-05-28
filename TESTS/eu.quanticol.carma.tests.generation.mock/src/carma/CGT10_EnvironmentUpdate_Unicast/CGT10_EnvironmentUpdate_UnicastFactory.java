package carma.CGT10_EnvironmentUpdate_Unicast;
import org.cmg.ml.sam.sim.*;
import eu.quanticol.carma.simulator.*;
public class CGT10_EnvironmentUpdate_UnicastFactory implements SimulationFactory<CarmaSystem> {

	public CGT10_EnvironmentUpdate_UnicastFactory() {
		
	}
	
	@Override
	public CarmaSystem getModel() {
		return new Simple();
	}

}
