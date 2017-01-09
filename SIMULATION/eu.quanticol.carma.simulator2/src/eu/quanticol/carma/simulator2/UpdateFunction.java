/**
 * 
 */
package eu.quanticol.carma.simulator2;

import java.util.function.Consumer;

/**
 * @author loreti
 *
 */
public interface UpdateFunction {
	
	/**
	 * This method must return the function that is used to update the state (either 
	 * population or store) when a action is executed by a component with store <code>source</code> 
	 * in the system configuration <code>state</code>.
	 * 
	 * @param state current system configuration
	 * @param source store of sending component
	 * @param value sent value
	 * @return function that performs the update on the populatio and on the global store after the action execution.
	 */
	public Consumer<CarmaPopulationState> compute( CarmaPopulationState state, Store source, Object value );

}
