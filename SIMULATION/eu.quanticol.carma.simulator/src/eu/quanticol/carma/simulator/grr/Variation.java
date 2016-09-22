/**
 * 
 */
package eu.quanticol.carma.simulator.grr;

/**
 * @author loreti
 *
 */
public class Variation {

	private final Instance instance;
	private final int variation;
	
	public Variation( Instance instance , int variation ) {
		this.instance = instance;
		this.variation = variation;
	}

	/**
	 * @return the instance
	 */
	public Instance getInstance() {
		return instance;
	}

	/**
	 * @return the variation
	 */
	public int getVariation() {
		return variation;
	}
	
	
}
