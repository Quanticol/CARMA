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
		Model m = new Model();
		SimulationEnvironment<CarmaSystem> sim = new SimulationEnvironment<>(m.getFactory("Simple"));
		sim.simulate(100);

	}

}
