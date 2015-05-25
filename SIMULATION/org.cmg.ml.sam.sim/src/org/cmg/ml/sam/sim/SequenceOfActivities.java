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

import org.apache.commons.math3.random.RandomGenerator;

/**
 * @author loreti
 *
 */
public class SequenceOfActivities implements Activity {

	private Activity[] activities;

	public SequenceOfActivities(Activity... activities) {
		this.activities = activities;
	}

	@Override
	public boolean execute(RandomGenerator r) {
		boolean result = true;
		for (Activity activity : activities) {
			result = activity.execute(r);
			if (!result) {
				return result;
			}
		}
		return result;
	}

}
