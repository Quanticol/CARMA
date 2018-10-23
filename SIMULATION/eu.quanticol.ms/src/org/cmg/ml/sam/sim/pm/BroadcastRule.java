	/**
 * 
 */
package org.cmg.ml.sam.sim.pm;

import java.util.LinkedList;
import java.util.List;
import java.util.function.Function;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.util.WeightedStructure;

/**
 * @author loreti
 *
 */
public class BroadcastRule implements PopulationRule {
	
	final private Function<PopulationState,Double> rateFunction;
	
	final private int senderIndex;
	
	final private Function<RandomGenerator,Integer> step;
	
	final private BroadcastReceiver[] receivers;
	
	final private String name;

	/**
	 * @param rateFunction
	 * @param senderIndex
	 * @param receivers
	 */
	public BroadcastRule(String name, 
			Function<PopulationState, Double> rateFunction, 
			int senderIndex,
			Function<RandomGenerator,Integer> step,
			BroadcastReceiver ... receivers) {
		super();
		this.rateFunction = rateFunction;
		this.senderIndex = senderIndex;
		this.receivers = receivers;
		this.step = step;
		this.name = name;
	}


	@Override
	public PopulationTransition apply(RandomGenerator r, PopulationState state) {
		if (state.getOccupancy(senderIndex)>0) {
			double rate = rateFunction.apply(state);
			if (rate > 0.0) {
				return new PopulationTransition(
					this.name, 
					state.getOccupancy(senderIndex)*rate, 
					(rg -> BroadcastRule.getDrift(name,rg,senderIndex,state,step,receivers))
				);
			}
		}
		return null;
	}

	public static Update getDrift(
			String name,
			RandomGenerator r, 
			int sender, 
			PopulationState state,
			Function<RandomGenerator,Integer> step, 
			BroadcastReceiver[] receivers
	) {
		Update result = new Update(name);
		result.consume(sender,1);
		result.produce(step.apply(r), 1);
		for( int i=0 ; i<receivers.length ; i++ ) {
			double pop = state.getOccupancy(receivers[i].receiver);
			if (receivers[i].receiver==sender) {
				pop = pop -1 ;
			}
			int counter = 0;
			double rp = receivers[i].receivingProbability.apply(state);
			for ( int j=0 ; j<pop ; j++ ) {
				if (r.nextDouble()<rp) {
					counter++;
					result.produce(receivers[i].step.apply(r), 1);
				}
			}
			result.consume(receivers[i].receiver, counter);
			if ( state.getOccupancy(receivers[i].receiver)+result.get(receivers[i].receiver)<0  ) {
				throw new IllegalArgumentException("!!!!");
			}
		}		
		return result;
	}
	
	public static class BroadcastReceiver {
		
		private final int receiver;
		
		private final Function<PopulationState,Double> receivingProbability;
		
		private final Function<RandomGenerator,Integer> step;
		
		/**
		 * @param receiver
		 * @param receivingProbability
		 */
		public BroadcastReceiver(int receiver, 
				Function<PopulationState, Double> receivingProbability,
				Function<RandomGenerator,Integer> step) {
			super();
			this.receiver = receiver;
			this.receivingProbability = receivingProbability;
			this.step = step;
		}
		
		/**
		 * @return the receiver
		 */
		public int getReceiver() {
			return receiver;
		}



		/**
		 * @return the receivingProbability
		 */
		public Function<PopulationState, Double> getReceivingProbability() {
			return receivingProbability;
		}
			
	}
}
