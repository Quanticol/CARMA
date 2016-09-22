/**
 * 
 */
package eu.quanticol.carma.core.ui.views;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * @author loreti
 *
 */
public class MeasureParametersDialog  extends TitleAreaDialog {

	private Composite area;
	private Composite container;
	private String[] parameters;
	private Map<String, Class<?>> types;
	private Text[] parametersFields;
	private boolean withError = false;
	private Map<String, Object> data;

	public MeasureParametersDialog(Shell parentShell, String[] parameters, Map<String, Class<?>> types) {
		super(parentShell);
		this.parameters = parameters;
		this.types = types;
	}

	@Override
	public void create() {
	    super.create();
	    setTitle("Set measure parameters...");
	    setMessage("Insert measure parameters", IMessageProvider.INFORMATION);
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		area = (Composite) super.createDialogArea(parent);
	    container = new Composite(area, SWT.NONE);
	    container.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
	    GridLayout layout = new GridLayout(2, false);
	    container.setLayout(layout);
	
	    parametersFields = new Text[parameters.length];
	
	    for( int i=0 ; i<parameters.length ; i++ ) {
		    Label lbtSimulationTime = new Label(container, SWT.NONE);
		    lbtSimulationTime.setText("Parameter "+parameters[i]+" ("+getTypeOf(i)+"):");

		    GridData dataSimulationTime = new GridData();
		    dataSimulationTime.grabExcessHorizontalSpace = true;
		    dataSimulationTime.horizontalAlignment = GridData.FILL;

		    parametersFields[i] = new Text(container, SWT.BORDER | SWT.LEFT);;	
		    parametersFields[i].setLayoutData(dataSimulationTime);
		    parametersFields[i].addModifyListener( new ModifyListener() {
				
				@Override
				public void modifyText(ModifyEvent e) {
					checkParameterType();				
				}
				
			});
	    }
	    
	    
	    return area;
	}

	private String getTypeOf(int i) {
		Class<?> type = types.get(parameters[i]);
		if (Integer.class.equals( type )) {
			return "int";
		}
		if (Double.class.equals( type )) {
			return "real";
		}
		return "?";
	}

	protected void checkParameterType() {
		for( int i=0 ; i<parametersFields.length ; i++ ) {
			String txt = parametersFields[i].getText();
			if (!txt.isEmpty()) {
				try {
					if ("int".equals(getTypeOf(i))) {
						Integer.parseInt(txt);
					}
					if ("real".equals(getTypeOf(i))) {
						Double.parseDouble(txt);
					}
				} catch (NumberFormatException e) {
					setMessage("Wrong type for parameter "+parameters[i]+"!",IMessageProvider.ERROR);
					withError = true;
				}
			}
		}
		withError = false;
	}

	public boolean isValid() {
		if (withError) {
			return false;
		}
		for (Text text : parametersFields) {
			if (text.getText().isEmpty()) {
				return false;
			}
		}
		return true;
	}

	public Map<String,Object> getParameters() {
		return data;
	}

	private Object getValue(int i) {
		if ("int".equals(getTypeOf(i))) {
			return Integer.parseInt(parametersFields[i].getText());
		}
		if ("real".equals(getTypeOf(i))) {
			return Double.parseDouble(parametersFields[i].getText());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#okPressed()
	 */
	@Override
	protected void okPressed() {
		if (isValid()) {
			saveInput();
			super.okPressed();
		} else {
			if (withError) {
				MessageDialog.openError(this.getShell(), "Error...", "Provided data are not correct!");
			} else {
				MessageDialog.openError(this.getShell(), "Error...", "Fill all the required data!");
			}
		}
	}

	private void saveInput() {
		data = new HashMap<String, Object>();
		for( int i=0 ; i<parameters.length ; i++ ) {
			data.put(parameters[i], getValue(i));
		}
	}
	
}
