/**
 * 
 */
package org.cmg.ml.sam.sim.pm;

import java.util.List;
import java.util.function.Function;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.Activity;

/**
 * @author loreti
 *
 */
public class PopulationTransition {

	private final Function<RandomGenerator,Update> transitionDriftFunction;
	private final double rate;
	private final String name;

	public PopulationTransition(
			String name,
			double rate,
			Function<RandomGenerator,Update> transitionDriftFunction
			) {
		this.name = name;
		this.transitionDriftFunction = transitionDriftFunction;
		this.rate = rate;
	}

	public double getRate() {
		return rate;
	}

	public Update apply( RandomGenerator r ) {
		return transitionDriftFunction.apply(r);
	}

	public String getName() {
		return name;
	}

}
