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
public class GossipFactoryFOSSACS_B implements SimulationFactory<PopulationModel> {
	
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


	private int[] initial;

	private double p_d;

	private double lambda_s;

	private double lambda_a;

	private double k;

	public GossipFactoryFOSSACS_B(
			int[] initial,
			int k,
			double p_d,
			double lambda_s,
			double lambda_a
	) {
		this.initial = initial;
		this.p_d = p_d;
		this.k = k; 
		this.lambda_s = lambda_s;
		this.lambda_a = lambda_a;
		this.rules = buildRules();
	}

	private LinkedList<PopulationRule> buildRules() {
		LinkedList<PopulationRule> rules = new LinkedList<>();
		
		rules.add( 
			new BroadcastRule(
				"AI_spread^*_i", 
				s -> this.p_d*this.lambda_s, 
				AI_INDEX, 
				rg -> PI_INDEX, 
				new BroadcastRule.BroadcastReceiver(
					PU_INDEX, 
					(PopulationState s) -> (s.getOccupancy(PU_INDEX)>0?(k/(s.getOccupancy(PI_INDEX)+s.getOccupancy(PU_INDEX))):0.0), 
					rgi -> PI_INDEX 
				) 
			)
		);

		rules.add( 
				new BroadcastRule(
					"AI_spread^*_u", 
					s -> (1.0-this.p_d)*this.lambda_s, 
					AI_INDEX, 
					rg -> PI_INDEX, 
					new BroadcastRule.BroadcastReceiver(
						PI_INDEX, 
						(PopulationState s) -> (s.getOccupancy(PI_INDEX)>0?(this.p_d*k/(s.getOccupancy(PI_INDEX)+s.getOccupancy(PU_INDEX))):0.0), 
						rgi -> PU_INDEX 
					) 
				)
			);

		rules.add( 
				new BroadcastRule(
					"AU_spread^*_u", 
					s -> this.lambda_s, 
					AU_INDEX,
					rg -> PU_INDEX, 
					new BroadcastRule.BroadcastReceiver(PI_INDEX, (PopulationState s) -> Math.min(this.p_d, this.p_d*(k/(s.getOccupancy(PI_INDEX)+s.getOccupancy(PU_INDEX)))), rgi -> PU_INDEX ) 
				)
			);

		rules.add( 
				new BroadcastRule(
					"PU_beact*", 
					s -> this.lambda_a, 
					PU_INDEX, 
					rg -> AU_INDEX
				)
			);

		rules.add( 
				new BroadcastRule(
					"PI_beact*", 
					s -> this.lambda_a, 
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
