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
public class InfoDiffusionFactory implements SimulationFactory<PopulationModel> {
	
	public static final int SIZE = 100;
	
	public static final int U_INDEX = 0;
	public static final int B_INDEX = 1;
	public static final int I_INDEX = 2;
	
	public static final double INFO_RATE = 0.1;
	public static final double DIFF_RATE = 0.2;
	
	public static final double K = 5;
	
//	public static final double REC_PROB = 0.2;
	public static final double REC_PROB = 0.2;
	
	private LinkedList<PopulationRule> rules;

	private Function<PopulationState, Double> diffRate;

	private Function<PopulationState, Double> recProb;

	private int[] initial;

	public InfoDiffusionFactory(
			int[] initial,
			Function<PopulationState,Double> diffRate ,
			Function<PopulationState,Double> recProb
	) {
		this.initial = initial;
		this.recProb = recProb;
		this.diffRate = diffRate;
		this.rules = buildRules();
	}

	private LinkedList<PopulationRule> buildRules() {
		LinkedList<PopulationRule> rules = new LinkedList<>();
		
		rules.add( 
			new BroadcastRule(
				"i", 
				this.diffRate, 
				B_INDEX, 
				rg -> I_INDEX, 
				new BroadcastRule.BroadcastReceiver(U_INDEX, this.recProb, rgi -> B_INDEX ) 
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
