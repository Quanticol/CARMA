/**
 * 
 */
package eu.quanticol.carma.examples.smarttaxis;

import org.cmg.ml.sam.sim.SimulationFactory;

import eu.quanticol.carma.simulator.CarmaSystem;

/**
 * @author loreti
 *
 */
public class SmartTaxiesFactory implements SimulationFactory<CarmaSystem> {

	private int users;
	private int taxis;

	public SmartTaxiesFactory( int taxis , int users ) {
		this.taxis = taxis;
		this.users = users;
	}
	
	@Override
	public CarmaSystem getModel() {
		return new SmartTaxies(taxis, users);
	}

}
