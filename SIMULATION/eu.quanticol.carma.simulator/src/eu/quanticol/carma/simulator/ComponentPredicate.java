/**
 * 
 */
package eu.quanticol.carma.simulator;

/**
 * @author loreti
 *
 */
public interface ComponentPredicate {

	public boolean eval(double now,CarmaComponent c);
	
}
