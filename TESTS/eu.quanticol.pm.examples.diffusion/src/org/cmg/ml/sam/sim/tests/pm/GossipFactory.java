/**
 * 
 */
package org.cmg.ml.sam.sim.tests.pm;

import java.util.LinkedList;
import java.util.function.Function;

import org.cmg.ml.sam.sim.SimulationFactory;
import org.cmg.ml.sam.sim.pm.BroadcastRule;
import org.cmg.ml.sam.sim.pm.PopulationModel;
import org.cmg.ml.sam.sim.pm.PopulationRule;
import org.cmg.ml.sam.sim.pm.PopulationState;
import org.cmg.ml.sam.sim.pm.ReactionRule;
import org.cmg.ml.sam.sim.sampling.Measure;

/**
 * @author loreti
 *
 */
public class GossipFactory implements SimulationFactory<PopulationModel> {
	
	public static final int SIZE = 100;
	
	public static final int PI_INDEX = 0;
	public static final int PU_INDEX = 1;
	public static final int AI_INDEX = 2;
	public static final int AU_INDEX = 3;
	
	public static final double P_RATE = 0.1;
	public static final double C_RATE = 1.0;
	
	public static final double K = 5;
	
//	public static final double REC_PROB = 0.2;
	public static final double REC_PROB = 0.2;
	
	private LinkedList<PopulationRule> rules;

	private Function<PopulationState, Double> diffRate;

	private Function<PopulationState, Double> passRate;	
	
	private Function<PopulationState, Double> recProb;

	private int[] initial;

	public GossipFactory(
			int[] initial,
			Function<PopulationState,Double> diffRate ,
			Function<PopulationState,Double> passRate ,
			Function<PopulationState,Double> recProb
	) {
		this.initial = initial;
		this.recProb = recProb;
		this.passRate = passRate;
		this.diffRate = diffRate;
		this.rules = buildRules();
	}

	private LinkedList<PopulationRule> buildRules() {
		LinkedList<PopulationRule> rules = new LinkedList<>();
		
		rules.add( 
			new BroadcastRule(
				"i", 
				this.diffRate, 
				AI_INDEX, 
				rg -> PI_INDEX, 
				new BroadcastRule.BroadcastReceiver(PU_INDEX, this.recProb, rgi -> PI_INDEX ) ,
				new BroadcastRule.BroadcastReceiver(PI_INDEX, this.recProb, rgi -> PI_INDEX ) 
			)
		);
		
		rules.add( 
				new BroadcastRule(
					"u", 
					this.diffRate, 
					AU_INDEX, 
					rg -> PU_INDEX, 
					new BroadcastRule.BroadcastReceiver(PU_INDEX, this.recProb, rgi -> PU_INDEX ) ,
//					new BroadcastRule.BroadcastReceiver(PI_INDEX, this.recProb, rgi -> (rgi.nextDouble()<0.75?PU_INDEX:PI_INDEX) ) 
					new BroadcastRule.BroadcastReceiver(PI_INDEX, this.recProb, rgi -> PU_INDEX ) 
				)
			);

		rules.add( 
				new BroadcastRule(
					"p_to_a1", 
					this.passRate, 
					PU_INDEX, 
					rg -> AU_INDEX
				)
			);

		rules.add( 
				new BroadcastRule(
					"p_to_a2", 
					this.passRate, 
					PI_INDEX, 
					rg -> AI_INDEX
				)
			);

		return rules;
	}

	@Override
	public PopulationModel getModel() {
		return new PopulationModel(
			this.rules ,
			new PopulationState( initial )
		);
	}

	@Override
	public Measure<PopulationModel> getMeasure(String name) {
		// TODO Auto-generated method stub
		return null;
	}

}
