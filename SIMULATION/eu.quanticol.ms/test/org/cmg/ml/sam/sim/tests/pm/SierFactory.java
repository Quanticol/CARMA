/**
 * 
 */
package org.cmg.ml.sam.sim.tests.pm;

import java.util.LinkedList;

import org.cmg.ml.sam.sim.SimulationFactory;
import org.cmg.ml.sam.sim.pm.PopulationModel;
import org.cmg.ml.sam.sim.pm.PopulationRule;
import org.cmg.ml.sam.sim.pm.PopulationState;
import org.cmg.ml.sam.sim.pm.ReactionRule;
import org.cmg.ml.sam.sim.sampling.Measure;

/**
 * @author loreti
 *
 */
public class SierFactory implements SimulationFactory<PopulationModel> {
	
	public static final int S_INDEX = 0;
	public static final int I_INDEX = 1;
	public static final int R_INDEX = 2;
	
	public static final double ALPHA = 0.1;
	public static final double BETA = 0.1;
	public static final double GAMMA = 0.2;
	
	private int size;
	private LinkedList<PopulationRule> rules;

	public SierFactory(int size) {
		this.size = size;
		this.rules = buildRules();
	}

	private LinkedList<PopulationRule> buildRules() {
		LinkedList<PopulationRule> rules = new LinkedList<>();
		
		rules.add( new ReactionRule("S,I->I,I", 
			new ReactionRule.Specie[] {new ReactionRule.Specie(S_INDEX,1), new ReactionRule.Specie(I_INDEX, 1)}, 
			new ReactionRule.Specie[] {new ReactionRule.Specie(I_INDEX,2)},
			(s -> s.getOccupancy(S_INDEX)*s.getOccupancy(I_INDEX)*BETA))
		);
		rules.add( new ReactionRule("S->I", 
				new ReactionRule.Specie[] {new ReactionRule.Specie(S_INDEX,1)}, 
				new ReactionRule.Specie[] {new ReactionRule.Specie(I_INDEX,1)},
				(s -> (s.getOccupancy(I_INDEX)==0?s.getOccupancy(S_INDEX)*BETA:0.0)))
			);
		rules.add( new ReactionRule("I->R", 
				new ReactionRule.Specie[] {new ReactionRule.Specie(I_INDEX, 1)}, 
				new ReactionRule.Specie[] {new ReactionRule.Specie(R_INDEX, 1)},
				(s -> s.getOccupancy(I_INDEX)*ALPHA))
			);
		rules.add( new ReactionRule("R->S", 
				new ReactionRule.Specie[] {new ReactionRule.Specie(R_INDEX, 1)}, 
				new ReactionRule.Specie[] {new ReactionRule.Specie(S_INDEX, 1)},
				(s -> s.getOccupancy(R_INDEX)*GAMMA))
			);
		
		return rules;
	}

	@Override
	public PopulationModel getModel() {
		return new PopulationModel(
			this.rules ,
			new PopulationState( new int[] { size-1,1,0 } )
		);
	}

	@Override
	public Measure<PopulationModel> getMeasure(String name) {
		// TODO Auto-generated method stub
		return null;
	}

}
