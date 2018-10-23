/**
 * 
 */
package org.cmg.ml.sam.sim.pm;

import java.util.function.Function;

import org.apache.commons.math3.random.RandomGenerator;

/**
 * @author loreti
 *
 */
public class ReactionRule implements PopulationRule {
	
	private final Specie[] reactants;
	
	private final Specie[] products;

	private final Function<PopulationState,Double> rateFunction;
	
	private final String name;

	private Update update;
	
	/**
	 * @param reactants
	 * @param products
	 * @param rate
	 */
	public ReactionRule(String name, Specie[] reactants, Specie[] products, Function<PopulationState, Double> rateFunction) {
		super();
		this.reactants = reactants;
		this.products = products;
		this.rateFunction = rateFunction;
		this.name = name;
		this.update = new Update(name);
		initDrift();
	}

	private void initDrift() {
		for( int i=0 ; i<reactants.length ; i++ ) {
			this.update.consume(reactants[i].index, reactants[i].size);
		}
		for( int i=0 ; i<products.length ; i++ ) {
			this.update.produce(products[i].index, products[i].size);
		}
	}

	@Override
	public PopulationTransition apply(RandomGenerator r, PopulationState state) {
		if (isEnabled(state)) {
			double rate = rateFunction.apply(state);
			if (rate>0) {
				return new PopulationTransition(
						name, 
						rate, 
						(rg -> update)
				);
			}
		}
		return null;
	}
	
	private boolean isEnabled(PopulationState state) {
		for( int i=0 ; i<reactants.length ; i++ ) {
			if (state.getOccupancy(reactants[i].index)<reactants[i].size) {
				return false;
			}
		}
		return true;
	}

	public static class Specie {
		
		private int index;

		private int size;
		
		/**
		 * @param index
		 * @param size
		 */
		public Specie(int index, int size) {
			super();
			this.index = index;
			this.size = size;
		}

	}
	
}
