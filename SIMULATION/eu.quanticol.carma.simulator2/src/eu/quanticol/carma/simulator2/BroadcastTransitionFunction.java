/**
 * 
 */
package eu.quanticol.carma.simulator2;

import java.util.LinkedList;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.pm.PopulationDrift;
import org.cmg.ml.sam.sim.pm.PopulationModel;

/**
 * @author loreti
 *
 */
public class BroadcastTransitionFunction {
	
	private final PopulationModel<Component, CarmaPopulationState> model;
	
	private final Component source;
	
	private final OutputStepData output;
	
	private final LinkedList<InputActionData> inputs;

	/**
	 * @param source
	 * @param output
	 * @param inputs
	 */
	public BroadcastTransitionFunction(PopulationModel<Component, CarmaPopulationState> model, Component source, OutputStepData output, LinkedList<InputActionData> inputs) {
		super();
		this.source = source;
		this.output = output;
		this.inputs = inputs;
		this.model = model;
	}

	public PopulationDrift<Component> compute( RandomGenerator r ) {
		//TODO: complete this method.
		PopulationDrift<Component> drift = new PopulationDrift<>();
//		drift.setDrift( source , source.apply(step, update))
//		for (InputActionData inputActionData : inputs) {
//			
//		}
		
		return drift;
	}
	
}
