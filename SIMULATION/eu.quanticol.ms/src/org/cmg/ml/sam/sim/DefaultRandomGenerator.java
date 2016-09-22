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
package org.cmg.ml.sam.sim;

import java.util.Random;

import org.apache.commons.math3.random.AbstractRandomGenerator;

/**
 * @author loreti
 *
 */
public class DefaultRandomGenerator extends AbstractRandomGenerator {

	private Random random = new Random();

	@Override
	public void setSeed(long seed) {
		clear();
		random.setSeed(seed);
	}

	@Override
	public double nextDouble() {
		return random.nextDouble();
	}

}
