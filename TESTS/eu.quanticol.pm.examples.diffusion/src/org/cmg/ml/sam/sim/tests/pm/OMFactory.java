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
import org.cmg.ml.sam.sim.pm.UnicastRule;
import org.cmg.ml.sam.sim.pm.UnicastRule.UnicastReceiver;
import org.cmg.ml.sam.sim.sampling.Measure;

/**
 * @author loreti
 *
 */
public class OMFactory implements SimulationFactory<PopulationModel> {

	public static final int SIZE = 100;

	public static final int CGP_INDEX = 0;
	public static final int CGM_INDEX = 1;
	public static final int CBP_INDEX = 2;
	public static final int CBM_INDEX = 3;
	public static final int G_INDEX = 4;
	public static final int B_INDEX = 5;

	public static final double SA_RATE = 0.1;
	public static final double CA_RATE = 1.0;	
	public static final double SR_RATE = 1.0;

	public static final double K = 5;

	//	public static final double REC_PROB = 0.2;
	public static final double REC_PROB = 0.2;

	private LinkedList<PopulationRule> rules;


	private int[] initial;

	private double p_sg;

	private double p_sb;

	private double lambda_sa;

	private double lambda_ca;

	private double lambda_sr;

	private double k;

	public OMFactory(
			int[] initial,
			int k,
			double p_sg,
			double p_sb,
			double lambda_sa,
			double lambda_ca,
			double lambda_sr
			) {
		this.initial = initial;
		this.p_sg = p_sg;
		this.p_sb = p_sb;
		this.k = k; 
		this.lambda_sa = lambda_sa;
		this.lambda_ca = lambda_ca;
		this.lambda_sr = lambda_sr;
		this.rules = buildRules();
	}

	private LinkedList<PopulationRule> buildRules() {
		LinkedList<PopulationRule> rules = new LinkedList<>();

		//CGP START
		rules.add(
				new UnicastRule(
						"search_g", 
						s -> lambda_sr, 
						CGP_INDEX, 
						rg  -> (rg.nextDouble()<p_sg?CGP_INDEX:CBM_INDEX), 
						new UnicastReceiver(
								G_INDEX,
								s -> 1.0,
								rg -> G_INDEX
								)
						)								
				);

		rules.add(
				new UnicastRule(
						"share_gp", 
						s -> lambda_ca, 
						CGP_INDEX, 
						rg  -> CGP_INDEX, 
						new UnicastReceiver(
								CGP_INDEX,
								s -> 1.0,
								rg -> CGP_INDEX
								),
						new UnicastReceiver(
								CGM_INDEX,
								s -> 1.0,
								rg -> CGP_INDEX
								),
						new UnicastReceiver(
								CBP_INDEX,
								s -> 1.0,
								rg -> CGP_INDEX
								),
						new UnicastReceiver(
								CBM_INDEX,
								s -> 1.0,
								rg -> CGP_INDEX
								)
						)								
				);
		//CGP END

		//CGM START
		rules.add(
				new UnicastRule(
						"search_g", 
						s -> lambda_sr, 
						CGM_INDEX, 
						rg  -> (rg.nextDouble()<p_sg?CGP_INDEX:CBM_INDEX), 
						new UnicastReceiver(
								G_INDEX,
								s -> 1.0,
								rg -> G_INDEX
								)
						)								
				);

		rules.add(
				new UnicastRule(
						"share_bm", 
						s -> lambda_ca, 
						CGM_INDEX, 
						rg  -> CGM_INDEX, 
						new UnicastReceiver(
								CGP_INDEX,
								s -> 1.0,
								rg -> CGP_INDEX
								),
						new UnicastReceiver(
								CGM_INDEX,
								s -> 1.0,
								rg -> CGM_INDEX
								),
						new UnicastReceiver(
								CBP_INDEX,
								s -> 1.0,
								rg -> CGM_INDEX
								),
						new UnicastReceiver(
								CBM_INDEX,
								s -> 1.0,
								rg -> CGM_INDEX
								)
						)								
				);
		//CGM END

		//CBP START
		rules.add(
				new UnicastRule(
						"search_b", 
						s -> lambda_sr, 
						CBP_INDEX, 
						rg  -> (rg.nextDouble()<p_sg?CBP_INDEX:CGM_INDEX), 
						new UnicastReceiver(
								B_INDEX,
								s -> 1.0,
								rg -> B_INDEX
								)
						)								
				);

		rules.add(
				new UnicastRule(
						"share_bp", 
						s -> lambda_ca, 
						CBP_INDEX, 
						rg  -> CBP_INDEX, 
						new UnicastReceiver(
								CGP_INDEX,
								s -> 1.0,
								rg -> CBP_INDEX
								),
						new UnicastReceiver(
								CGM_INDEX,
								s -> 1.0,
								rg -> CBP_INDEX
								),
						new UnicastReceiver(
								CBP_INDEX,
								s -> 1.0,
								rg -> CBP_INDEX
								),
						new UnicastReceiver(
								CBM_INDEX,
								s -> 1.0,
								rg -> CBP_INDEX
								)
						)								
				);
		//CBP END

		//CBM START
		rules.add(
				new UnicastRule(
						"search_b", 
						s -> lambda_sr, 
						CBM_INDEX, 
						rg  -> (rg.nextDouble()<p_sg?CBP_INDEX:CGM_INDEX), 
						new UnicastReceiver(
								B_INDEX,
								s -> 1.0,
								rg -> B_INDEX
								)
						)								
				);

		rules.add(
				new UnicastRule(
						"share_gm", 
						s -> lambda_ca, 
						CBM_INDEX, 
						rg  -> CBM_INDEX, 
						new UnicastReceiver(
								CGP_INDEX,
								s -> 1.0,
								rg -> CBM_INDEX
								),
						new UnicastReceiver(
								CGM_INDEX,
								s -> 1.0,
								rg -> CBM_INDEX
								),
						new UnicastReceiver(
								CBP_INDEX,
								s -> 1.0,
								rg -> CBP_INDEX
								),
						new UnicastReceiver(
								CBM_INDEX,
								s -> 1.0,
								rg -> CBM_INDEX
								)
						)								
				);
		//CBP END

		//G START
		rules.add( 
				new BroadcastRule(
						"giggle*", 
						s -> lambda_sa, 
						G_INDEX, 
						rg -> G_INDEX, 
						new BroadcastRule.BroadcastReceiver(
								CGP_INDEX,
								s -> broadcastProb(CGP_INDEX, s), 
								rg -> CGP_INDEX							
								),
						new BroadcastRule.BroadcastReceiver(
								CGM_INDEX,
								s -> broadcastProb(CGM_INDEX, s), 
								rg -> CGM_INDEX							
								),											
						new BroadcastRule.BroadcastReceiver(
								CBP_INDEX,
								s -> broadcastProb(CBP_INDEX, s), 
								rg -> rg.nextDouble()<p_sb?CGM_INDEX:CBP_INDEX							
								),
						new BroadcastRule.BroadcastReceiver(
								CBM_INDEX,
								s -> broadcastProb(CBM_INDEX, s), 
								rg -> rg.nextDouble()<p_sb?CGM_INDEX:CBM_INDEX							
								)																	
						)				
				);
		//G END

		//B START
		rules.add( 
				new BroadcastRule(
						"bong*", 
						s -> lambda_sa, 
						B_INDEX, 
						rg -> B_INDEX, 
						new BroadcastRule.BroadcastReceiver(
								CGP_INDEX,
								s -> broadcastProb(CGP_INDEX, s), 
								rg -> rg.nextDouble()<p_sb?CBM_INDEX:CGP_INDEX							
								),
						new BroadcastRule.BroadcastReceiver(
								CGM_INDEX,
								s -> broadcastProb(CGM_INDEX, s), 
								rg -> rg.nextDouble()<p_sb?CBM_INDEX:CGM_INDEX							
								),											
						new BroadcastRule.BroadcastReceiver(
								CBP_INDEX,
								s -> broadcastProb(CBP_INDEX, s), 
								rg -> CBP_INDEX
								),
						new BroadcastRule.BroadcastReceiver(
								CBM_INDEX,
								s -> broadcastProb(CBM_INDEX, s), 
								rg -> CBM_INDEX							
								)																	
						)				
				);
		//B END
		
		
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
	
	private double broadcastProb( int idx , PopulationState s ) {
		double pop = s.getOccupancy(idx);
		if (pop == 0.0) {
			return 0.0;
		}
		double tot = s.getOccupancy(CBM_INDEX)+s.getOccupancy(CBP_INDEX)+s.getOccupancy(CGM_INDEX)+s.getOccupancy(CGP_INDEX);
		//return pop*k/(tot*tot);
		return k/tot;
	}

}
