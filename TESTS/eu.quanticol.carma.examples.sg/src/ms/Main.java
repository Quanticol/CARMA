/**
 * 
 */
package ms;

import org.cmg.ml.sam.sim.SimulationEnvironment;

import eu.quanticol.carma.simulator.CarmaSystem;

/**
 * @author loreti
 *
 */
public class Main {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Example m = new Example();
		SimulationEnvironment<CarmaSystem> sim = new SimulationEnvironment<>(m.getFactory("Scenario1"));
		sim.simulate(100);

	}

}
