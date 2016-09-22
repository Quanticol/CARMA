/**
 * 
 */
package eu.quanticol.carma.core.ui.views;

import java.util.LinkedList;

import org.cmg.ml.sam.sim.sampling.SimulationTimeSeries;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import eu.quanticol.carma.core.ui.data.SimulationOutcome;

/**
 * @author loreti
 *
 */
public class ShowDataDialog extends Dialog {

	private SimulationOutcome result;

	public ShowDataDialog(Shell shell, SimulationOutcome result) {
		super(shell);
		this.result = result;
	}

	@Override
    protected Control createDialogArea(Composite parent) {
            Composite container = (Composite) super.createDialogArea(parent);
            container.setLayout(new FillLayout());
            CTabFolder folder = new CTabFolder(parent, SWT.BORDER);
            for (SimulationTimeSeries serie : result.getCollectedData()) {
        		CTabItem item = new CTabItem(folder, SWT.CLOSE);
        		item.setText(serie.getName());
        		Composite composite = new Composite(folder, SWT.NONE);
        		composite.setLayout(new FillLayout());
        		TableViewer resultViewer = new TableViewer(composite, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
        		resultViewer.setContentProvider(ArrayContentProvider.getInstance());
        		resultViewer.setInput(new LinkedList<>());
        		item.setControl(composite);
        	}
            return container;
    }

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		// create OK and Cancel buttons by default
		createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL,
				true);
//		createButton(parent, IDialogConstants.CANCEL_ID,
//				IDialogConstants.CANCEL_LABEL, false);
	}
}
