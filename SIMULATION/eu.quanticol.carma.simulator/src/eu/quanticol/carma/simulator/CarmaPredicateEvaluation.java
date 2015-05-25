/**
 * 
 */
package eu.quanticol.carma.simulator;

/**
 * @author loreti
 *
 */
public interface CarmaPredicateEvaluation {

	public CarmaPredicate eval( CarmaStore localStore , Object ... values );
	
}
