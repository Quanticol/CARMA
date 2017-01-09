/**
 * 
 */
package eu.quanticol.carma.simulator2;

/**
 * @author loreti
 *
 */
public interface RateFunction {


	/**
	 * This method must return a real value greater or equal to 0.0. This value represent
	 * the rate of an unicast/broadcast output executed by a component with store <code>source</code> 
	 * that sends <code>value</code>.
	 * 
	 * @param state current system configuration
	 * @param source store of sending component
	 * @param value sent value
	 * @return rate of output action
	 */
	public double compute(CarmaPopulationState state, Store source, Object value);
	
}
