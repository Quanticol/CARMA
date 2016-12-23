package eu.quanticol.carma.core.tests

import org.junit.runner.RunWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.InjectWith
import eu.quanticol.carma.core.CARMAInjectorProvider
import com.google.inject.Inject
import org.eclipse.xtext.junit4.util.ParseHelper
import eu.quanticol.carma.core.carma.Model
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.junit.Test
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper
import static extension org.junit.Assert.*
import eu.quanticol.carma.simulator.CarmaModel
import eu.quanticol.carma.core.carma.CarmaPackage
import eu.quanticol.carma.core.validation.CARMAValidator

@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProviderCustom))
class TestExpressionContext {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
		
	@Test
	def void test_Wrong_use_of_now(){
		'''
		fun real testFunction( ) {
			return now;
		}
		'''.parse.assertError(CarmaPackage::eINSTANCE.atomicNow,CARMAValidator::ERROR_Bad_use_of_now,"")
	}

	@Test
	def void test_Wrong_use_of_global(){
		'''
		fun real testFunction( ) {
			return global.x;
		}
		'''.parse.assertError(CarmaPackage::eINSTANCE.globalContext,CARMAValidator::ERROR_Bad_use_of_global,"")
	}
	
}