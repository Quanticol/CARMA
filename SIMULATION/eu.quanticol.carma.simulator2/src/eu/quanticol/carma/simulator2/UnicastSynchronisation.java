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
public class UnicastSynchronisation extends PopulationTransition<Component, CarmaPopulationState>{

	private final Component source;
	private final Object value;
	private final Component target;
	private final String activity;
	
	public UnicastSynchronisation(
			PopulationModel<Component, CarmaPopulationState> model, 
			Component source,
			String activity,
			Object value,
			Component target,
			double rate,
			Function<RandomGenerator, PopulationDrift<Component>> transitionDriftFunction,
			Consumer<CarmaPopulationState> postTransitionAction) {
		super(model, rate, transitionDriftFunction, postTransitionAction);
		this.source = source;
		this.activity = activity;
		this.value = value;
		this.target = target;
	}

	@Override
	public String getInfo() {
		return source.toString()+":"+activity+"@"+target.toString();
	}

	/**
	 * @return the source
	 */
	public Component getSource() {
		return source;
	}

	/**
	 * @return the value
	 */
	public Object getValue() {
		return value;
	}

	/**
	 * @return the target
	 */
	public Component getTarget() {
		return target;
	}

	/**
	 * @return the activity
	 */
	public String getActivity() {
		return activity;
	}

}
