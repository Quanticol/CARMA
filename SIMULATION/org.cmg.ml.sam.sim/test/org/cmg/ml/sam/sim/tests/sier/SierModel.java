/*******************************************************************************
 * Copyright (c) 2015 QUANTICOL EU Project.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 *
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Michele Loreti (University of Firenze) - initial API and implementation
 *******************************************************************************/
package org.cmg.ml.sam.sim.tests.sier;

import org.cmg.ml.sam.sim.AgentBasedModel;
import org.cmg.ml.sam.sim.ModelI;
import org.cmg.ml.sam.sim.SimulationEnvironment;
import org.cmg.ml.sam.sim.sampling.Measure;
import org.cmg.ml.sam.sim.sampling.SamplingCollection;
import org.cmg.ml.sam.sim.sampling.StatisticSampling;

/**
 * @author loreti
 *
 */
public class SierModel extends AgentBasedModel<SeirState> {
	
	public final static Measure<SierModel> mS = new Measure<SierModel>() {

		@Override
		public double measure(SierModel t) {
			return t.getData().getInS();
		}

		@Override
		public String getName() {
			return "S";
		}
	};

	public final static Measure<SierModel> mE = new Measure<SierModel>() {

		@Override
		public double measure(SierModel t) {
			return t.getData().getInE();
		}

		@Override
		public String getName() {
			return "E";
		}
	};
	
	public final static Measure<SierModel> mI = new Measure<SierModel>() {

		@Override
		public double measure(SierModel t) {
			return t.getData().getInI();
		}

		@Override
		public String getName() {
			return "I";
		}
	};

	public final static Measure<SierModel> mR = new Measure<SierModel>() {

		@Override
		public double measure(SierModel t) {
			return t.getData().getInR();
		}

		@Override
		public String getName() {
			return "R";
		}
	};


	public SierModel(int sSize, int iSize, int eSize, int rSize) {
		super(new SeirState(sSize, iSize, eSize, rSize));
		for( int i=0 ; i<sSize ; i++ ) {
			addAgent( new SeirAgent(SeirAgentState.S_STATE) );
		}
		for( int i=0 ; i<iSize ; i++ ) {
			addAgent( new SeirAgent(SeirAgentState.I_STATE) );
		}
		for( int i=0 ; i<eSize ; i++ ) {
			addAgent( new SeirAgent(SeirAgentState.E_STATE) );
		}
		for( int i=0 ; i<rSize ; i++ ) {
			addAgent( new SeirAgent(SeirAgentState.R_STATE) );
		}
	}


	public static void main( String[] argv) {		
		StatisticSampling<SierModel> sS = new StatisticSampling<SierModel>(1000, 0.1, mS);
		StatisticSampling<SierModel> sE = new StatisticSampling<SierModel>(1000, 0.1, mE);
		StatisticSampling<SierModel> sI = new StatisticSampling<SierModel>(1000, 0.1, mI);
		StatisticSampling<SierModel> sR = new StatisticSampling<SierModel>(1000, 0.1, mR);
		SimulationEnvironment<SierModel> simEnv = new SimulationEnvironment<SierModel>(new SeirSimulationFactory(100,0,1,0));
		simEnv.setSampling(new SamplingCollection<SierModel>(sS,sI,sE,sR));
		simEnv.simulate(100,100.0);
		sE.printTimeSeries(System.out);
	}
	

}
