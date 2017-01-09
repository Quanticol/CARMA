/**
 * 
 */
package eu.quanticol.carma.simulator2;

import java.util.function.Consumer;
import java.util.function.Function;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.pm.PopulationDrift;
import org.cmg.ml.sam.sim.pm.PopulationModel;
import org.cmg.ml.sam.sim.pm.PopulationTransition;

/**
 * @author loreti
 *
 */
public class ActivityTransition extends PopulationTransition<Component, CarmaPopulationState>{

	private final Component source;
	private final String activity;
	
	public ActivityTransition(
			PopulationModel<Component, CarmaPopulationState> model, 
			Component source,
			String activity,
			double rate,
			Function<RandomGenerator, PopulationDrift<Component>> transitionDriftFunction,
			Consumer<CarmaPopulationState> postTransitionAction) {
		super(model, rate, transitionDriftFunction, postTransitionAction);
		this.source = source;
		this.activity = activity;
	}

	@Override
	public String getInfo() {
		return source.toString()+":"+activity;
	}

}
