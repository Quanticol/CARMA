/**
 * 
 */
package eu.quanticol.carma.core.ui.views;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.FillLayout;
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

	private static final String RANGE_INDICATOR = "..";
	private static final String SEPARATOR = ",";
	private Composite area;
	private Composite container;
	private String[] parameters;
	private Map<String, Class<?>> types;
	private Text[] parametersFields;
	private boolean withError = false;
	private List<Map<String, Object>> data;

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
	    Label descriptionLabel = new Label(container, SWT.NONE);
	    descriptionLabel.setText("You can provide multiple comma-separated values as well as "
	    		+ "(for integer parameters) ranges, such as 2, 5..8, 10");
	    GridData descriptionLabelData = new GridData();
	    //descriptionLabelData.verticalAlignment = GridData.FILL;
	    descriptionLabelData.horizontalSpan = 2;
	    descriptionLabelData.verticalIndent = 10;
	    descriptionLabel.setLayoutData(descriptionLabelData);
	    
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
						for (String seg : txt.split(SEPARATOR)) {
							int idx = seg.indexOf(RANGE_INDICATOR);
							if (idx < 0) {
								Integer.parseInt(seg.trim());
							}
							else {
								// check that range does not have repeated separators eg 1-5-10
								int lastIdx = seg.lastIndexOf(RANGE_INDICATOR);
								if (lastIdx != idx) {
									setMessage("Malformed range for parameter " + parameters[i] +".",
											IMessageProvider.ERROR);
									withError = true;
									return;
								}
								else {
									Integer.parseInt(seg.substring(0, idx).trim());
									Integer.parseInt(seg.substring(idx+RANGE_INDICATOR.length()).trim());
								}
							}
						}
					}
					if ("real".equals(getTypeOf(i))) {
						for (String seg : txt.split(SEPARATOR)) {
							if (seg.contains(RANGE_INDICATOR)) {
								setMessage("Ranges cannot be given for real-valued parameters.",
										IMessageProvider.ERROR);
								withError = true;
								return;
							}
							else {
								Double.parseDouble(seg);
							}
						}
						Double.parseDouble(txt);
					}
				} catch (NumberFormatException e) {
					setMessage("Wrong type for parameter "+parameters[i]+"!",IMessageProvider.ERROR);
					withError = true;
					return;
				}
			}
		}
		withError = false;
		setMessage("");
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

	public List<Map<String,Object>> getParameters() {
		return data;
	}
	
	/*
	private Object getValue(int i) {
		if ("int".equals(getTypeOf(i))) {
			return Integer.parseInt(parametersFields[i].getText());
		}
		if ("real".equals(getTypeOf(i))) {
			return Double.parseDouble(parametersFields[i].getText());
		}
		return null;
	}
	*/
	
	private List<? extends Object> getValues(int i) {
		if ("int".equals(getTypeOf(i))) {
			return parseTextForInteger(parametersFields[i].getText());
		}
		if ("real".equals(getTypeOf(i))) {
			return parseTextForDouble(parametersFields[i].getText());
		}
		return null;
	}

	private List<Integer> parseTextForInteger(String txt) {
		List<Integer> values = new ArrayList<Integer>();
		for (String seg : txt.split(SEPARATOR)) {
			int idx = seg.indexOf(RANGE_INDICATOR);
			if (idx < 0) {
				values.add(Integer.parseInt(seg.trim()));
			}
			else {
				int low = Integer.parseInt(seg.substring(0, idx).trim());
				int high =  Integer.parseInt(seg.substring(idx+RANGE_INDICATOR.length()).trim());
				while (low <= high) {
					values.add(low++);
				}
			}
		}
		return values;
	}
	
	private List<Double> parseTextForDouble(String txt) {
		List<Double> values = new ArrayList<Double>();
		for (String seg : txt.split(SEPARATOR)) {
			values.add(Double.parseDouble(seg.trim()));
		}
		return values;
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

	/*
	private void saveInput() {
		data = new ArrayList<Map<String,Object>>();
		data.add(new HashMap<String, Object>());
		for( int i=0 ; i<parameters.length ; i++ ) {
			data.get(0).put(parameters[i], getValue(i));
		}
	}
	*/
	
	private void saveInput() {
		data = new ArrayList<Map<String,Object>>(combineEntries());
	}
	
	private List<Map<String,Object>> combineEntries() {
		List<Map<String,Object>> combs = new ArrayList<Map<String,Object>>();
		combs.add(new HashMap<String,Object>());
		for( int i=0 ; i<parameters.length ; i++ ) {
			List<Map<String,Object>> newCombs = new ArrayList<Map<String,Object>>();
			for (Map<String,Object> previous : combs) {
				for (Object val : getValues(i)) {
					Map<String,Object> updated = new HashMap<String,Object>(previous);
					updated.put(parameters[i], val);
					newCombs.add(updated);
				}
			}
			combs = newCombs;
		}
		return combs;
	}
	
}
