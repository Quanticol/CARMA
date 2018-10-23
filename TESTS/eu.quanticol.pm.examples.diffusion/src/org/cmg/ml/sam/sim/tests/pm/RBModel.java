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
public class RBModel {
	
	private static final int SIM_TIME = 20;
	
	private static final int SAMPLINGS = 100;

	private static final double DEADLINE = 10.0;
	private static final int ITERATIONS = 1;

	private static final int R_INIT_SIZE = 97;
	private static final int B_INIT_SIZE = 1;
	private static final int CR_INIT_SIZE = 1;
	private static final int CB_INIT_SIZE = 1;

	
	public static void main(String[] argv) throws FileNotFoundException {
//		run("const_",ITERATIONS,DEADLINE,SAMPLINGS,s -> DIFF_RATE,s -> PASS_RATE, s-> PROB,getInitState(1));
//
		run(
				"rb_1_",
				ITERATIONS,
				DEADLINE,SAMPLINGS,
				getInitState(1)
			);
	
		run(
				"rb_1_",
				ITERATIONS,
				DEADLINE,SAMPLINGS,
				getInitState(1)
			);

		run(
				"rb_10_",
				ITERATIONS,
				DEADLINE,SAMPLINGS,
				getInitState(10)
			);


		run(
				"rb_100_",
				ITERATIONS,
				DEADLINE,SAMPLINGS,
				getInitState(100)
			);

		run(
				"rb_1000_",
				ITERATIONS,
				DEADLINE,SAMPLINGS,
				getInitState(1000)
			);

//		run(
//				"om_10000_",
//				ITERATIONS,
//				DEADLINE,SAMPLINGS,
//				getInitState(10000)
//			);

}
	
	private static int[] getInitState(int scale) {
		int[] result = new int[4];
		result[RBFactory.R_INDEX] = scale*R_INIT_SIZE;
		result[RBFactory.B_INDEX] = scale*B_INIT_SIZE;
		result[RBFactory.BT_INDEX] = scale*CR_INIT_SIZE;
		result[RBFactory.RT_INDEX] = scale*CB_INIT_SIZE;
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
				new RBFactory(initial,10,1,1,0.5,0.5)
			);
			StatisticSampling<PopulationModel> rSamp = getMeasure(samplings,deadline,"R",RBFactory.R_INDEX);
			StatisticSampling<PopulationModel> bSamp = getMeasure(samplings,deadline,"B",RBFactory.B_INDEX);
			StatisticSampling<PopulationModel> btSamp = getMeasure(samplings,deadline,"BT",RBFactory.BT_INDEX);
			StatisticSampling<PopulationModel> rtSamp = getMeasure(samplings,deadline,"RT",RBFactory.RT_INDEX);
			StatisticSampling<PopulationModel> red = getMeasure(samplings,deadline,"RED",s -> s.getOccupancy(RBFactory.R_INDEX)+s.getOccupancy(RBFactory.RT_INDEX));
			StatisticSampling<PopulationModel> blue = getMeasure(samplings,deadline,"BLUE",s -> s.getOccupancy(RBFactory.B_INDEX)+s.getOccupancy(RBFactory.BT_INDEX));



			long start = System.currentTimeMillis();
			sim.setSampling(new SamplingCollection<>(rSamp, bSamp, btSamp, rtSamp,red,blue));
			sim.simulate(iterations,DEADLINE);
			System.out.println("Time: "+(System.currentTimeMillis()-start));
			rSamp.printTimeSeries(new PrintStream("data/"+label+"_r_.data"));
			bSamp.printTimeSeries(new PrintStream("data/"+label+"_b_.data"));
			btSamp.printTimeSeries(new PrintStream("data/"+label+"_bt_.data"));
			rtSamp.printTimeSeries(new PrintStream("data/"+label+"_rt_.data"));
			red.printTimeSeries(new PrintStream("data/"+label+"_red_.data"));
			blue.printTimeSeries(new PrintStream("data/"+label+"_blue_.data"));
	}

	private static StatisticSampling<PopulationModel> getMeasure(int samplings, double deadline, String name, int idx) {
//		return new StatisticSampling<PopulationModel>(samplings, deadline/samplings, 
//				new Measure<PopulationModel>() {
//
//			@Override
//			public double measure(PopulationModel t) {
//				// TODO Auto-generated method stub
//				return t.getCurrentState().getOccupancy(idx);
//			}
//
//			@Override
//			public String getName() {
//				return name;
//			}
//
//		});
		return getMeasure(samplings, deadline, name, s -> s.getOccupancy(idx));
	}

	private static StatisticSampling<PopulationModel> getMeasure(int samplings, double deadline, String name, Function<PopulationState,Double> m) {
		return new StatisticSampling<PopulationModel>(samplings, deadline/samplings, 
				new Measure<PopulationModel>() {

			@Override
			public double measure(PopulationModel t) {
				// TODO Auto-generated method stub
				return m.apply( t.getCurrentState() );
			}

			@Override
			public String getName() {
				return name;
			}

		});
	}


}
