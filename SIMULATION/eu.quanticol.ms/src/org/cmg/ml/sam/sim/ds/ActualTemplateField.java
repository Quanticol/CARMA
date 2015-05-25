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
package org.cmg.ml.sam.sim.ds;

/**
 * @author loreti
 *
 */
public class ActualTemplateField implements TemplateField {

	private Object o;

	public ActualTemplateField(Object o) {
		this.o = o;
	}

	@Override
	public boolean match(Object o) {
		if (this.o == null) {
			return o == null;
		}
		return this.o.equals(o);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return (o == null ? 0 : o.hashCode());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof ActualTemplateField) {
			Object other = ((ActualTemplateField) obj).o;
			return (o == null ? other == null : o.equals(other));
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return (o == null ? "null" : o.toString());
	}

	@Override
	public boolean implies(TemplateField f) {
		if (f instanceof ActualTemplateField) {
			return this.equals(f);
		}
		if (f instanceof FormalTemplateField) {
			return ((FormalTemplateField) f).clazz.isInstance(o);
		}
		return false;
	}

}
