/**
 * 
 */
package org.cmg.ml.sam.sim.tests.pm;

import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.LinkedList;
import java.util.function.Function;

import org.cmg.ml.sam.sim.SimulationEnvironment;
import org.cmg.ml.sam.sim.pm.PopulationModel;
import org.cmg.ml.sam.sim.pm.PopulationState;
import org.cmg.ml.sam.sim.sampling.Measure;
import org.cmg.ml.sam.sim.sampling.SamplingCollection;
import org.cmg.ml.sam.sim.sampling.SamplingFunction;
import org.cmg.ml.sam.sim.sampling.SimulationTimeSeries;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;

/**
 * @author loreti
 *
 */
public class OMModel {
	
	private static final int SIM_TIME = 20;
	
	private static final int SAMPLINGS = 100;

	private static final double DEADLINE = 10.0;
	private static final int ITERATIONS = 1;

	private static final int G_INIT_SIZE = 1;
	private static final int B_INIT_SIZE = 1;
	private static final int CGP_INIT_SIZE = 100;
	private static final int CGM_INIT_SIZE = 0;
	private static final int CBP_INIT_SIZE = 100;
	private static final int CBM_INIT_SIZE = 0;

	
	public static void main(String[] argv) throws FileNotFoundException {
//		run("const_",ITERATIONS,DEADLINE,SAMPLINGS,s -> DIFF_RATE,s -> PASS_RATE, s-> PROB,getInitState(1));
//
		run(
				"om_1_",
				ITERATIONS,
				DEADLINE,SAMPLINGS,
				getInitState(1)
			);
	
		run(
				"om_1_",
				ITERATIONS,
				DEADLINE,SAMPLINGS,
				getInitState(1)
			);

		run(
				"om_10_",
				ITERATIONS,
				DEADLINE,SAMPLINGS,
				getInitState(10)
			);


		run(
				"om_100_",
				ITERATIONS,
				DEADLINE,SAMPLINGS,
				getInitState(100)
			);

//		run(
//				"om_1000_",
//				ITERATIONS,
//				DEADLINE,SAMPLINGS,
//				getInitState(1000)
//			);

//		run(
//				"om_10000_",
//				ITERATIONS,
//				DEADLINE,SAMPLINGS,
//				getInitState(10000)
//			);

}
	
	private static int[] getInitState(int scale) {
		int[] result = new int[6];
		result[OMFactory.G_INDEX] = scale*G_INIT_SIZE;
		result[OMFactory.B_INDEX] = scale*B_INIT_SIZE;
		result[OMFactory.CGP_INDEX] = scale*CGP_INIT_SIZE;
		result[OMFactory.CGM_INDEX] = scale*CGM_INIT_SIZE;
		result[OMFactory.CBP_INDEX] = scale*CBP_INIT_SIZE;
		result[OMFactory.CBM_INDEX] = scale*CBM_INIT_SIZE;
		return result;
	}

	public static void run( 
			String label , 
			int iterations, 
			double deadline,
			int samplings,
			int[] initial
	) throws FileNotFoundException {
		SimulationEnvironment<PopulationModel> sim = new SimulationEnvironment<PopulationModel>(
				new OMFactory(initial,100,0.75,0.25,0.1,0.1,1.0)
			);
			StatisticSampling<PopulationModel> gSamp = getMeasure(samplings,deadline,"G",OMFactory.G_INDEX);
			StatisticSampling<PopulationModel> bSamp = getMeasure(samplings,deadline,"B",OMFactory.B_INDEX);
			StatisticSampling<PopulationModel> cgpSamp = getMeasure(samplings,deadline,"CGP",OMFactory.CGP_INDEX);
			StatisticSampling<PopulationModel> cgmSamp = getMeasure(samplings,deadline,"CGM",OMFactory.CGM_INDEX);
			StatisticSampling<PopulationModel> cbpSamp = getMeasure(samplings,deadline,"CBP",OMFactory.CBP_INDEX);
			StatisticSampling<PopulationModel> cbmSamp = getMeasure(samplings,deadline,"CBM",OMFactory.CBM_INDEX);



			long start = System.currentTimeMillis();
			sim.setSampling(new SamplingCollection<>(gSamp, bSamp, cgpSamp, cgmSamp, cbpSamp, cbmSamp));
			sim.simulate(iterations,DEADLINE);
			System.out.println("Time: "+(System.currentTimeMillis()-start));
			gSamp.printTimeSeries(new PrintStream("data/"+label+"_g_.data"));
			bSamp.printTimeSeries(new PrintStream("data/"+label+"_b_.data"));
			cgpSamp.printTimeSeries(new PrintStream("data/"+label+"_cgp_.data"));
			cgmSamp.printTimeSeries(new PrintStream("data/"+label+"_cgm_.data"));
			cbpSamp.printTimeSeries(new PrintStream("data/"+label+"_cbp_.data"));
			cbmSamp.printTimeSeries(new PrintStream("data/"+label+"_cbm_.data"));
	}

	private static StatisticSampling<PopulationModel> getMeasure(int samplings, double deadline, String name, int idx) {
		return new StatisticSampling<PopulationModel>(samplings, deadline/samplings, 
				new Measure<PopulationModel>() {

			@Override
			public double measure(PopulationModel t) {
				// TODO Auto-generated method stub
				return t.getCurrentState().getOccupancy(idx);
			}

			@Override
			public String getName() {
				return name;
			}

		});
	}
}
