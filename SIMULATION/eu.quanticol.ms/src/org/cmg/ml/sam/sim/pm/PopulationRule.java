/**
 * 
 */
package org.cmg.ml.sam.sim.pm;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.util.WeightedStructure;

/**
 * @author loreti
 *
 */
public interface PopulationRule {
	
	/**
	 * 
	 * 
	 * @param r
	 * @param state
	 * @return null if the rule cannot be applied.
	 */
	public PopulationTransition apply( RandomGenerator r , PopulationState state );
	
}
