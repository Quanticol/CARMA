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

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.Activity;
import org.cmg.ml.sam.sim.Agent;
import org.cmg.ml.sam.sim.util.WeightedElement;
import org.cmg.ml.sam.sim.util.WeightedStructure;

public class SeirAgent extends Agent<SeirState> {
	
	private SeirAgentState state;

	public SeirAgent() {
		this(SeirAgentState.S_STATE);
	}

	public SeirAgent(SeirAgentState state) {
		this.state = state;
	}

	@Override
	public WeightedStructure<Activity> getActivities(final SeirState data) {
		switch (state) {
		case S_STATE:
			return new WeightedElement<Activity>(
					data.fromStoERate() ,
					new Activity() {

						@Override
						public boolean execute(
								RandomGenerator r ) {
							data.fromStoE();
							state = SeirAgentState.E_STATE;
							return false;
						}
						
					}
			);

		case E_STATE:
			return new WeightedElement<Activity>(
					data.fromEtoIRate() ,
					new Activity() {

						@Override
						public boolean execute(
								RandomGenerator r ) {
							data.fromEtoI();
							state = SeirAgentState.I_STATE;
							return false;
						}
						
					}

			);

		case I_STATE:
			return new WeightedElement<Activity>(
					data.fromItoRRate() ,
					new Activity() {

						@Override
						public boolean execute(
								RandomGenerator r ) {
							data.fromItoR();
							state = SeirAgentState.R_STATE;
							return false;
						}
						
					}

			);

		case R_STATE:
			return new WeightedElement<Activity>(
					data.fromRtoIRate() ,
					new Activity() {

						@Override
						public boolean execute(
								RandomGenerator r ) {
							data.fromRtoI();
							state = SeirAgentState.I_STATE;
							return false;
						}
						
					}

			);

		default:
			return null;
		}

	}
	
}