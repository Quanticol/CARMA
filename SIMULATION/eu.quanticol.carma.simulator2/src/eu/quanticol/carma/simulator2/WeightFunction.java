/**
 * 
 */
package eu.quanticol.carma.simulator2;

/**
 * Function used to compute weigh value that can be used to compute receiving probability.
 * 
 * 
 * @author loreti
 *
 */
public interface WeightFunction {

	/**
	 * This method must return a real value greater or equal to 0.0. This value will be used
	 * to compute the probability that a component with store <code>target</code> 
	 * receives <code>value</code> sent by a component with store <code>source</code> 
	 * in the system configuration <code>state</code>.
	 * 
	 * @param state current system configuration
	 * @param source store of sending component
	 * @param value sent value
	 * @param target store of receiving component
	 * @return
	 */
	public double compute( CarmaPopulationState state, Store source , Object value , Store target );
	
}
