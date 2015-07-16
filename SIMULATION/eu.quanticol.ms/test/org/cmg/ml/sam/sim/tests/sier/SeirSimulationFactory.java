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

import org.cmg.ml.sam.sim.SimulationFactory;
import org.cmg.ml.sam.sim.sampling.Measure;

public class SeirSimulationFactory implements SimulationFactory<SierModel> {

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
	private int sSize;
	private int eSize;
	private int iSize;
	private int rSize;
	
	public SeirSimulationFactory( int sSize , int eSize , int iSize , int rSize ) {
		this.sSize = sSize;
		this.eSize = eSize;
		this.iSize = iSize;
		this.rSize = rSize;
	}

	@Override
	public SierModel getModel() {
		return new SierModel(sSize, iSize, eSize, rSize);
	}

	@Override
	public Measure<SierModel> getMeasure(String name) {
		if (name == null) {
			return null;
		}
		if (name.equals("mS")) {
			return mS;
		}
		if (name.equals("mE")) {
			return mE;
		}
		if (name.equals("mI")) {
			return mI;
		}
		if (name.equals("mR")) {
			return mR;
		}
		return null;
	}
	
}