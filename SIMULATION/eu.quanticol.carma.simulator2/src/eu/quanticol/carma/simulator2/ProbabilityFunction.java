/**
 * 
 */
package eu.quanticol.carma.simulator2;

/**
 * Function used to compute receiving probability of a broadcast message.
 * 
 * 
 * @author loreti
 *
 */
public interface ProbabilityFunction {

	/**
	 * This method must return a value in [0,1] representing the probability 
	 * that a component with store <code>target</code> receives <code>value</code>
	 * sent by a component with store <code>source</code> in the system configuration 
	 * <code>state</code>.
	 * 
	 * @param state current system configuration
	 * @param source store of sending component
	 * @param value sent value
	 * @param target store of receiving component
	 * @return
	 */
	public double compute( CarmaPopulationState state , Store source , Object value , Store target );
	
}
