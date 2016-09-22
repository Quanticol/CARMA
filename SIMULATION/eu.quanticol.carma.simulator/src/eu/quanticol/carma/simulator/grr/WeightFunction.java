/**
 * 
 */
package eu.quanticol.carma.simulator.grr;

/**
 * @author loreti
 *
 */
@FunctionalInterface
public interface WeightFunction {
	
	public double apply( Population population , Configuration configuration );

}
