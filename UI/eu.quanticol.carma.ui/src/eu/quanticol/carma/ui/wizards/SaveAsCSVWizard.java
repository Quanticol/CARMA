package eu.quanticol.carma.ui.wizards;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;

import eu.quanticol.carma.ui.laboratory.ExperimentJob;

public class SaveAsCSVWizard extends Wizard {
	
	private IStructuredSelection selection;
	private ExperimentJob experimentJob;
	private SaveAsCSVWizardPage saveAsCSVWizardPage;
	private IWorkbench workbench;
	
	public class SaveAsCSVWizardPage extends WizardNewFileCreationPage {
		
		public SaveAsCSVWizardPage(String pageName,
				IStructuredSelection selection) {
			
			super(pageName, selection);
			DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd_HH:mm:ss_");
			Date date = new Date();
			String newFilename = dateFormat.format(date) + experimentJob.getModelName() + ".csv";
			setFileName(newFilename);
		}
	}
	
	public SaveAsCSVWizard(ExperimentJob experimentJob) {
		this.experimentJob = experimentJob;
		this.selection = StructuredSelection.EMPTY;
		saveAsCSVWizardPage = new SaveAsCSVWizardPage("Save as...", selection);
		addPage(saveAsCSVWizardPage);
		
	}

	@Override
	public boolean performFinish() {
		IFile file = saveAsCSVWizardPage.createNewFile();
		if (file != null)
			return true;
		else
			return false;
	}

}