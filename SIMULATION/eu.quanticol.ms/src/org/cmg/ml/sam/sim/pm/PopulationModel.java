/**
 * 
 */
package org.cmg.ml.sam.sim.pm;

import java.util.LinkedList;
import java.util.List;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.Activity;
import org.cmg.ml.sam.sim.ModelI;
import org.cmg.ml.sam.sim.util.WeightedElement;
import org.cmg.ml.sam.sim.util.WeightedLinkedList;
import org.cmg.ml.sam.sim.util.WeightedStructure;

/**
 * 
 * This class implements a population model.
 * 
 * @author loreti
 *
 */
public class PopulationModel<S> implements ModelI {

	private LinkedList<PopulationRule<S>> rules;
	
	private PopulationState<S> currentState;
	
	private double time;
	
	
	@Override
	public WeightedStructure<Activity> getActivities( RandomGenerator r ) {
		WeightedLinkedList<Activity> activities = new WeightedLinkedList<>();
		for (PopulationRule<S> rule : rules) {
			PopulationTransition<S> tra = rule.apply(r, currentState);
			activities.add( 
				new WeightedElement<Activity>(
					tra.getRate(), 
					new Activity() {
						@Override
						public boolean execute(RandomGenerator r) {
							apply( tra , r );
							return false;
						}						
					}
				) 
			);
		}
		return activities;
	}

	protected void apply(PopulationTransition<S> tra, RandomGenerator r) {
		this.currentState = tra.apply( this.currentState , r);
	}

	@Override
	public void timeStep(double dt) {
		this.time += dt;
	}
	
	
}
