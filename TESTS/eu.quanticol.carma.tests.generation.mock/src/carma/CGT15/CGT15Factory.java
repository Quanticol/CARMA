package carma.CGT15;
import org.cmg.ml.sam.sim.*;
import eu.quanticol.carma.simulator.*;
public class CGT15Factory implements SimulationFactory<CarmaSystem> {

	public CGT15Factory() {
		
	}
	
	@Override
	public CarmaSystem getModel() {
		return new Simple();
	}

}
