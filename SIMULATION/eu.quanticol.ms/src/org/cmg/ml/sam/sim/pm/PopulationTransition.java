/**
 * 
 */
package org.cmg.ml.sam.sim.pm;

import java.util.function.Consumer;
import java.util.function.Function;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.Activity;

/**
 * @author loreti
 *
 */
public abstract class PopulationTransition<S,T extends PopulationState<S>> implements Activity {

	private Function<RandomGenerator,PopulationDrift<S>> transitionDriftFunction;
	private Consumer<T> postTransitionAction;
	private double rate;
	private PopulationModel<S, T> model;

	public PopulationTransition(
			PopulationModel<S, T> model,
			double rate,
			Function<RandomGenerator,PopulationDrift<S>> transitionDriftFunction, 
			Consumer<T> postTransitionAction
			) {
		this.model = model;
		this.rate = rate;
		this.transitionDriftFunction = transitionDriftFunction;
		this.postTransitionAction = postTransitionAction;
		this.rate = rate;
	}

	public double getRate() {
		return rate;
	}

	@Override
	public String getName() {
		return getInfo();
	}

	public abstract String getInfo();

	@Override
	public boolean execute(RandomGenerator r) {
		PopulationDrift<S> drift = transitionDriftFunction.apply(r);
		model.apply(drift,postTransitionAction);
		return false;
	}
	
	

}
