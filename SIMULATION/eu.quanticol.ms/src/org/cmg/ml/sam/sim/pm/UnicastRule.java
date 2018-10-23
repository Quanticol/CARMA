/**
 * 
 */
package org.cmg.ml.sam.sim.pm;

import java.util.function.Function;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.util.ComposedWeightedStructure;
import org.cmg.ml.sam.sim.util.WeightedStructure;

/**
 * @author loreti
 *
 */
public class UnicastRule implements PopulationRule {
	
	final private Function<PopulationState,Double> rateFunction;
	
	final private int senderIndex;
	
	final private Function<RandomGenerator,Integer> step;
	
	final private UnicastReceiver[] receivers;
	
	final private String name;

	/**
	 * @param rateFunction
	 * @param senderIndex
	 * @param receivers
	 */
	public UnicastRule(String name, 
			Function<PopulationState, Double> rateFunction, 
			int senderIndex,
			Function<RandomGenerator,Integer> step,
			UnicastReceiver ... receivers) {
		super();
		this.rateFunction = rateFunction;
		this.senderIndex = senderIndex;
		this.receivers = receivers;
		this.step = step;
		this.name = name;
	}

	public boolean isEnabled( PopulationState state ) {
		if (state.getOccupancy(senderIndex)==0) {
			return false;
		}
		for( int i=0 ; i<receivers.length ; i++ ) {
			if ((state.getOccupancy(receivers[i].receiver)>0)&&(receivers[i].receivingWeight.apply(state)>0)) {
				return true;
			}
		}
		
		return false;
	}

	@Override
	public PopulationTransition apply(RandomGenerator r, PopulationState state) {
		if (isEnabled(state)) {
			double rate = rateFunction.apply(state);
			if (rate > 0.0) {
				return new PopulationTransition(
					this.name, 
					rate*state.getOccupancy(senderIndex), 
					(rg -> UnicastRule.getDrift(name,rg,senderIndex,state,step,receivers))
				);
			}
		}
		return null;
	}

	public static Update getDrift( String name,
			RandomGenerator r, 
			int sender, 
			PopulationState state,
			Function<RandomGenerator,Integer> step, 
			UnicastReceiver[] receivers
	) {
		Update result = new Update(name);
		result.consume(sender,1);
		result.produce(step.apply(r), 1);
		WeightedStructure<UnicastReceiver> receiverSelector = new ComposedWeightedStructure<>(); 
		for( int i=0 ; i<receivers.length ; i++ ) {
			int receiverIndex = receivers[i].receiver;
			if (state.getOccupancy(receiverIndex)-(receiverIndex==sender?1:0)>0) {
				receiverSelector = receiverSelector.add(state.getOccupancy(receivers[i].receiver)*receivers[i].receivingWeight.apply(state),receivers[i]);
			}
		}
		UnicastReceiver ur = receiverSelector.select(r.nextDouble()*receiverSelector.getTotalWeight()).getElement();
		result.consume(ur.receiver, 1);
		result.produce(ur.step.apply(r),1);
		return result;
	}
	
	public static class UnicastReceiver {
		
		private final int receiver;
		
		private final Function<PopulationState,Double> receivingWeight;
		
		private final Function<RandomGenerator,Integer> step;
		
		/**
		 * @param receiver
		 * @param receivingProbability
		 */
		public UnicastReceiver(int receiver, 
				Function<PopulationState,Double> receivingProbability,
				Function<RandomGenerator,Integer> step) {
			super();
			this.receiver = receiver;
			this.receivingWeight =receivingProbability;
			this.step = step;
		}
		
		/**
		 * @return the receiver
		 */
		public int getReceiver() {
			return receiver;
		}
			
	}
}
