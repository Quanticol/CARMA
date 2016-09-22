/**
 * 
 */
package eu.quanticol.carma.examples.smarttaxis;

import org.cmg.ml.sam.sim.SimulationFactory;
import org.cmg.ml.sam.sim.sampling.Measure;

import eu.quanticol.carma.simulator.CarmaSystem;

/**
 * @author loreti
 *
 */
public class SmartTaxisFactory implements SimulationFactory<CarmaSystem> {

	private int users;
	private int taxis;

	public SmartTaxisFactory( int taxis , int users ) {
		this.taxis = taxis;
		this.users = users;
	}
	
	@Override
	public CarmaSystem getModel() {
		return new SmartTaxis(taxis, users);
	}

	@Override
	public Measure<CarmaSystem> getMeasure(String name) {
		// TODO Auto-generated method stub
		//FIXME!!!!
		return null;
	}

}
