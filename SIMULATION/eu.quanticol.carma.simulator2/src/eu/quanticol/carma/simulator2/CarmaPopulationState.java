/**
 * 
 */
package eu.quanticol.carma.simulator2;

import java.util.HashMap;

import org.cmg.ml.sam.sim.pm.PopulationModel;
import org.cmg.ml.sam.sim.pm.PopulationState;

/**
 * @author loreti
 *
 */
public class CarmaPopulationState extends PopulationState<Component> {

	private Store globalStore;
	
	public CarmaPopulationState() {
		this(new Store());
	}
	
	public CarmaPopulationState(Store globalStore) {
		this.globalStore = globalStore;
	}
	
	public CarmaPopulationState(HashMap<Component,Integer> population) {
		this(new Store(),population);
	}	
	
	public CarmaPopulationState(Store globalStore, HashMap<Component,Integer> population) {
		super(population);
		this.globalStore = globalStore;
	}
	
	public Store getStore() {
		return globalStore;
	}
	
}
