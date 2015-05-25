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
/**
 * 
 */
package org.cmg.ml.sam.sim;

import java.util.LinkedList;

import org.cmg.ml.sam.sim.util.WeightedLinkedList;
import org.cmg.ml.sam.sim.util.WeightedStructure;

/**
 * @author loreti
 *
 */
public class AgentBasedModel<S> implements ModelI {

	protected LinkedList<Agent<S>> agents;
	protected S data;

	public AgentBasedModel(S data) {
		this.data = data;
		this.agents = new LinkedList<Agent<S>>();
	}

	@Override
	public WeightedStructure<Activity> getActivities() {
		WeightedStructure<Activity> toReturn = new WeightedLinkedList<Activity>();
		for (Agent<S> agent : agents) {
			WeightedStructure<Activity> local = agent.getActivities(data);
			if (local != null) {
				toReturn = toReturn.add(local);
			}
		}
		return toReturn;
	}

	public S getData() {
		return data;
	}

	public void addAgent(Agent<S> agent) {
		this.agents.add(agent);
	}

	public boolean removeAgent(Agent<S> agent) {
		return this.agents.remove(agent);
	}

	@Override
	public void timeStep(double dt) {
	}

}
