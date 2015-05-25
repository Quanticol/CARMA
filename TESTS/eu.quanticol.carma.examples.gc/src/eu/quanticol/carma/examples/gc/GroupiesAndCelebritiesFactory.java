/**
 * 
 */
package eu.quanticol.carma.examples.gc;

import java.util.ArrayList;

import org.cmg.ml.sam.sim.Agent;
import org.cmg.ml.sam.sim.SimulationFactory;

import eu.quanticol.carma.simulator.CarmaSystem;

/**
 * @author loreti
 *
 */
public class GroupiesAndCelebritiesFactory implements SimulationFactory<CarmaSystem>{

	private int groupies;
	private int celebrities;

	public GroupiesAndCelebritiesFactory(int groupies , int celebrities) {
		this.groupies = groupies;
		this.celebrities = celebrities;
	}

	@Override
	public CarmaSystem getModel() {
		return new GroupiesAndCelebrities( groupies , celebrities );
	}
	
//	@Override
//	public CaspaSystem getSimulationContext() {
//	}
//
//	@Override
//	public ArrayList<Agent<CaspaSystem>> getAgents(CaspaSystem context) {
//		return context.getAgents();
//	}

}
