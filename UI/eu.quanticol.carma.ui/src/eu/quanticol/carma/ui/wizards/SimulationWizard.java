/**
 * 
 */
package eu.quanticol.carma.ui.wizards;

import java.util.LinkedList;

import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;

import eu.quanticol.carma.core.carma.Model;
import eu.quanticol.carma.ui.util.CarmaUiUtils;

/**
 * @author loreti
 *
 */
public class SimulationWizard {//extends Wizard {

//	protected SelectModelAndSystemPage modelAndSystemPage;
//	
//	public SimulationWizard() {
//		super();
//		setNeedsProgressMonitor(true);
//	}
//	
//	@Override
//	public void addPages() {
//		modelAndSystemPage = new SelectModelAndSystemPage();
//		addPage(modelAndSystemPage);
//	}
//
//	@Override
//	public boolean performFinish() {
//		return false;
//	}
//	
//	
//	public class SelectModelAndSystemPage extends WizardPage {
//		
//		private Composite container;
//		private LinkedList<Model> models;
//
//		public SelectModelAndSystemPage() {
//			super("Select Model and System");
//			setTitle("Select Model and System");
//			setDescription("Select Model and System");
//		}		
//		
//		@Override
//		public void createControl(Composite parent) {
//		    this.models = CarmaUiUtils.getActiveModels();
//			this.container = new Composite(parent, SWT.NONE);
//			 GridLayout layout = new GridLayout();
//			    container.setLayout(layout);
//			    layout.numColumns = 2;
//			    Label label1 = new Label(container, SWT.NONE);
//			    label1.setText("Model:");
//			    Combo combo = new Combo(container, SWT.NONE);
//			    for (Model model : models) {
//					combo.add(model.eResource().getURI().toFileString());
//				}
//			    setControl(container);
//			    setPageComplete(false);
//
//		}
//		
//	}

}
