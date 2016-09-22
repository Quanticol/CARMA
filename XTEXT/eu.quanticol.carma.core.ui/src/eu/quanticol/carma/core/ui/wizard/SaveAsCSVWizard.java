package eu.quanticol.carma.core.ui.wizard;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;

import org.cmg.ml.sam.sim.sampling.StatisticSampling;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;

import eu.quanticol.carma.core.ui.jobs.ExperimentJob;
import eu.quanticol.carma.simulator.CarmaSystem;

public class SaveAsCSVWizard extends Wizard {
	
	private IStructuredSelection selection;
	private ExperimentJob experimentJob;
	private ArrayList<?> results;
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
		
		@Override
		protected InputStream getInitialContents() {
			InputStream in = null;
			String str = experimentJob.toCSVString();
//			for(ResultsProvider rp : results){
//				str = str + "\n \n" + rp.toCSVString();
//			}
			try {
				in = new ByteArrayInputStream(str.getBytes("UTF-8"));
			} catch (UnsupportedEncodingException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			return in;
		}
	}
	
	public SaveAsCSVWizard(ExperimentJob experimentJob) {
		this.experimentJob = experimentJob;
//		this.results = new ArrayList<ResultsProvider>();
		
//		for(int i = 0; i < experimentJob.getCollection().size(); i++){
//			results.add(new ResultsProvider(((StatisticSampling<CarmaSystem>) experimentJob.getCollection().get(i))
//					,experimentJob.getSamples()));
//		}
		
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