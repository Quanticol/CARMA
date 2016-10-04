/**
 * 
 */
package org.cmg.ml.sam.sim.pm;

import java.util.HashSet;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;

import org.apache.commons.math3.random.RandomGenerator;

/**
 * @author loreti
 *
 */
public class PopulationRule<S> {
	
	private String name;
	
	private Predicate<PopulationState<S>> isEnabled;

	private BiFunction<RandomGenerator,PopulationState<S>,PopulationTransition<S>> transitionFunction;
		
	private Function<PopulationState<S>,Set<PopulationState<S>>> nextStateFunction;
	
	public PopulationRule(String name,
			Predicate<PopulationState<S>> isEnabled,
			BiFunction<RandomGenerator,PopulationState<S>,PopulationTransition<S>> transitionFunction,
			Function<PopulationState<S>,Set<PopulationState<S>>> nextStateFunction) {
		super();
		this.name = name;
		this.isEnabled = isEnabled;
		this.transitionFunction = transitionFunction;
		this.nextStateFunction = nextStateFunction;
	}
	
	/**
	 * 
	 * @param state
	 * @return
	 */
	public boolean isEnabled( PopulationState<S> state ) {
		return this.isEnabled.test( state );
	}
	
	/**
	 * 
	 * 
	 * @param r
	 * @param state
	 * @return
	 */
	public PopulationTransition<S> apply( RandomGenerator r , PopulationState<S> state ) {
		if (isEnabled(state)) {
			return transitionFunction.apply(r, state);
		} else {
			return null;
		}
	}


	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * 
	 * 
	 * @param state
	 * @return
	 */
	public Set<PopulationState<S>> next( PopulationState<S> state ) {
		if (isEnabled(state)) {
			return nextStateFunction.apply(state);
		} else {
			return new HashSet<>();
		}
	}
	
	
}
