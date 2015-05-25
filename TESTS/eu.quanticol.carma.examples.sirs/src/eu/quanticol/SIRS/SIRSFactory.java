/**
 * 
 */
package eu.quanticol.SIRS;

import java.util.ArrayList;

import org.cmg.ml.sam.sim.Agent;
import org.cmg.ml.sam.sim.SimulationFactory;

import eu.quanticol.carma.simulator.CarmaSystem;

/**
 * @author loreti
 *
 */
public class SIRSFactory implements SimulationFactory<CarmaSystem> {
	
	private int susceptibles = 0;
	private int infectives = 0;
	private int recovereds = 0;
	private int zones = 0;
	
	public SIRSFactory(int susceptibles , int infectives, int recovereds , int zones) {
		this.susceptibles = susceptibles;
		this.infectives = infectives;
		this.recovereds = recovereds;
		this.zones = zones;
	}

	@Override
	public CarmaSystem getModel() {
		return new SIRS( susceptibles, infectives, recovereds, zones );
	}

}
