/**
 * 
 */
package org.cmg.ml.sam.sim.pm;

import java.util.function.Function;

import org.apache.commons.math3.random.RandomGenerator;

/**
 * @author loreti
 *
 */
public class PopulationTransition<S> {

	private PopulationState<S> pre;
	private Function<RandomGenerator,PopulationState<S>> post;
	private double rate;

	public PopulationTransition(PopulationState<S> pre, Function<RandomGenerator,PopulationState<S>> post, double rate) {
		this.pre = pre;
		this.post = post;
		this.rate = rate;
	}

	/**
	 * @return the pre
	 */
	public PopulationState<S> getPre() {
		return pre;
	}

	/**
	 * @return the post
	 */
	public PopulationState<S> getPost( RandomGenerator r ) {
		return post.apply(r);
	}

	/**
	 * @return the rate
	 */
	public double getRate() {
		return rate;
	}

	public PopulationState<S> apply(PopulationState<S> currentState, RandomGenerator r ) {
		return currentState.remove(pre).add(getPost(r));
	}
	
	

}
