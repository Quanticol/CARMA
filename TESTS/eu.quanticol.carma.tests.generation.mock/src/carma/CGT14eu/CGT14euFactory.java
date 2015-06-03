package carma.CGT14eu;
import org.cmg.ml.sam.sim.*;
import eu.quanticol.carma.simulator.*;
public class CGT14euFactory implements SimulationFactory<CarmaSystem> {

	public CGT14euFactory() {
		
	}
	
	@Override
	public CarmaSystem getModel() {
		return new Simple();
	}

}
