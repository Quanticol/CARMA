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
package org.cmg.ml.sam.sim.tests;

/**
 * @author loreti
 *
 */
public class TestMain {
	
	public static void main( String[] argv ) {
		
		Integer i1 = 4;
		Integer i2 = 5;
		float x = 2.0F;
		float y = 3.0F;
		System.out.println("Equals: "+i1.getClass().equals(i2.getClass()));
		System.out.println("==: "+(i1.getClass() == i2.getClass()));
		System.out.println((0.1+0.2)-0.3);

	}

}
