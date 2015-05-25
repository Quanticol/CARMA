package eu.quanticol.carma.core.generator

import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.System
import com.google.inject.Inject
import eu.quanticol.carma.core.typing.TypeProvider
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.utils.Util
import static extension org.eclipse.xtext.EcoreUtil2.*

class GenerateFactory {
	
		@Inject extension TypeProvider
		@Inject extension LabelUtil
		@Inject extension Util
	
		def String compileFactory(Model model, String packageName, String className){
		'''
			«packageName»;
			import org.cmg.ml.sam.carma.CarmaSystem;
			import org.cmg.ml.sam.sim.SimulationFactory;
			public class «className»Factory implements SimulationFactory<CarmaSystem> {
			
				public «className»Factory() {
					
				}
				
				@Override
				public CarmaSystem getModel() {
					return new «model.eAllOfType(System).get(0).label»();
				}
			
			}
		'''
	}
	
}