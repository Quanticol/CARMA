package org.cmg.ml.sam.sim.pm;

import java.util.LinkedList;
import java.util.List;
import java.util.function.Consumer;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.Activity;
import org.cmg.ml.sam.sim.ModelI;
import org.cmg.ml.sam.sim.util.WeightedElement;
import org.cmg.ml.sam.sim.util.WeightedLinkedList;
import org.cmg.ml.sam.sim.util.WeightedStructure;

/**
 * 
 * This class implements a population model. This class is parametrised with
 * respect to types <code>S</code> and and <code>T</code>. The former is the
 * data type used to identify population species in the population vector.
 * Parameter <code>T</code> identifies environment
 * 
 * @author loreti
 *
 */
public class PopulationModel implements ModelI {

	private LinkedList<PopulationRule> rules;

	private double time;
	
	private PopulationState currentState;

	
	public PopulationModel( LinkedList<PopulationRule> rules, PopulationState currentState) {
		this.rules = rules;
		this.currentState = currentState;
	}
	
	public PopulationState getCurrentState() {
		return currentState;
	}

	protected void setState(PopulationState newState) {
		this.currentState = newState;
	}

	@Override
	public WeightedStructure<Activity> getActivities(RandomGenerator r) {
		WeightedLinkedList<Activity> activities = new WeightedLinkedList<>();
		PopulationState currentState = getCurrentState();
		for (PopulationRule rule : rules) {
			PopulationTransition tra = rule.apply(r, currentState);
			if (tra != null) {
				activities.add(
					new WeightedElement<Activity>(
						tra.getRate(), 
						new Activity() {

							@Override
							public String getName() {
								return tra.getName();
							}

							@Override
							public boolean execute(RandomGenerator r) {
								PopulationModel.this.currentState = 
										PopulationModel.this.currentState.apply(tra.apply(r));
								return false;
							}
							
						}
					)						
				);
			}
		}
		return activities;
	}

	@Override
	public void timeStep(double dt) {
		this.time += dt;
	}

	protected void apply(Update drift) {
		setState(getCurrentState().apply(drift));
	}

	public double getTime() {
		return time;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return currentState.toString();
	}
}
