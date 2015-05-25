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

public class SeirSimulationFactory implements SimulationFactory<SierModel> {
	
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
	
}