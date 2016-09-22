package eu.quanticol.carma.core.ui.wizard;

import java.util.Collection;
import java.util.List;

import eu.quanticol.carma.core.ui.nature.CarmaNature;

public class CustomCARMAProjectCreator extends CARMAProjectCreator {
	
	
	private static final String CARMA_CORE 	= "eu.quanticol.carma.core";
	private static final String CARMA_SIM	= "eu.quanticol.carma.simulator";
	private static final String CARMA_MS	= "eu.quanticol.ms";
	
	/**
	 * @param bundles 
	 * @return
	 */
	public static Collection<? extends String> addCARMABundles(List<String> bundles ) {
		bundles.add(CARMA_CORE);
		bundles.add(CARMA_SIM);
		bundles.add(CARMA_MS);
		return bundles;
	}

	@Override
	protected List<String> getRequiredBundles() {
		List<String> result = super.getRequiredBundles();
		result.addAll(addCARMABundles(result));
		return result;
	}

	@Override
	protected String[] getProjectNatures() {
		String[] parentNatures = super.getProjectNatures();
		String[] natures = new String[parentNatures.length+1];
		for( int i=0 ; i<parentNatures.length ; i++) {
			natures[i] = parentNatures[i];
		}
		natures[parentNatures.length] = CarmaNature.ID;
		return natures;
	}
	
	

}
