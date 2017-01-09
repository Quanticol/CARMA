/**
 * 
 */
package eu.quanticol.carma.simulator2;

import java.util.LinkedList;
import java.util.Map.Entry;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.pm.PopulationRule;
import org.cmg.ml.sam.sim.pm.PopulationTransition;

/**
 * @author loreti
 *
 */
public class BroadcastRule implements PopulationRule<Component, CarmaPopulationState>{

	private final OutputActionData output;
	
	private final LinkedList<InputActionData> inputs;

	private TransitionFunctionGenerator transitionGenerationFunction; 
		
	private RateFunction rate;
	
	private ProbabilityFunction probability;
	
	private UpdateFunction update;
	
	public BroadcastRule( 
			OutputActionData output , 
			LinkedList<InputActionData> inputs , 
			RateFunction rate , 
			ProbabilityFunction probability ,
			UpdateFunction update ) {
		this.output = output;
		this.inputs = inputs;
		this.transitionGenerationFunction = new TransitionFunctionGenerator();
		this.probability = probability;
		this.rate = rate;
		this.update = update;
	}

	@Override
	public LinkedList<PopulationTransition<Component, CarmaPopulationState>> apply(RandomGenerator r,
			CarmaPopulationState state) {
		LinkedList<PopulationTransition<Component, CarmaPopulationState>> toReturn = new LinkedList<>();
		
		/*
		 * Retrieve all the components involved in the rule. These are the ones that
		 * have the same componendId of this.output.componentId and that have the
		 * process in position this.output.getAgentIndex() equal to output.getAgentId()
		 *  
		 */
		Stream<Entry<Component, Integer>> activeComponents  = state.select(s -> (
				(s.getConfiguration().getComponentId()==output.getComponentId())&&
				(s.getConfiguration().getAgentId(output.getAgentIndex())==output.getAgentId()))						
		);
		
		
		
		
		return toReturn;
	}
	
	public class TransitionFunctionGenerator implements BiFunction<CarmaPopulationState,Entry<Component, Integer> , LinkedList<PopulationTransition<Component, CarmaPopulationState>>> {

		@Override
		public LinkedList<PopulationTransition<Component, CarmaPopulationState>> apply(CarmaPopulationState s, Entry<Component, Integer> e) {
			
//			Component c = e.getKey();
//			int multiplicity = e.getValue();
//			LinkedList<OutputStepData> steps = output.getSteps().apply(c.getStore());
//			
//			
//			steps.stream().map(mapper)
			// TODO Auto-generated method stub
			return null;
		}
		
		
		
		
	}
	
	
	
}
