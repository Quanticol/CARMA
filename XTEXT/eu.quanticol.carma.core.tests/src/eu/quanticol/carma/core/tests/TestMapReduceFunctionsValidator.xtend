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
class TestMapReduceFunctionsValidator {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper	
	@Inject extension CompilationTestHelper
		
	@Test
	def void test_Map_Argument_Error_Arg1(){
		'''
		fun int testFunction( int x ) {
			return x;
		}
		
		const test = map( 3 , 1 );
		'''.parse.assertError(CarmaPackage::eINSTANCE.mapFunction,CARMAValidator::ERROR_Expression_type_error,"")
	}

	@Test
	def void test_Map_Nested_Error_Arg2(){
		'''
		const test = map( {: 3 :} , map( {: 2 :} , 2 )+ {: 1 :} );
		'''.parse.assertError(CarmaPackage::eINSTANCE.mapFunction,CARMAValidator::ERROR_LambdaContext_NoNested_Arg2,"")
	}

	@Test
	def void test_Filter_Argument_Error_Arg1(){
		'''
		fun int testFunction( int x ) {
			return x;
		}
		
		const test = filter( 3 , 3 );
		'''.parse.assertError(CarmaPackage::eINSTANCE.filterFunction,CARMAValidator::ERROR_Expression_type_error,"")
	}

	@Test
	def void test_Exists_Argument_Error_Arg1(){
		'''
		fun int testFunction( int x ) {
			return x;
		}
		
		const test = exist( 3 , testFunction( * ) );
		'''.parse.assertError(CarmaPackage::eINSTANCE.existsFunction,CARMAValidator::ERROR_Expression_type_error,"")
	}

	@Test
	def void test_ForAll_Argument_Error_Arg1(){
		'''
		fun int testFunction( int x ) {
			return x;
		}
		
		const test = forall( 3 , testFunction( * )  );
		'''.parse.assertError(CarmaPackage::eINSTANCE.forAllFunction,CARMAValidator::ERROR_Expression_type_error,"")
	}

	@Test
	def void test_Size_Argument_Error_Arg1(){
		'''
		fun int testFunction( int x ) {
			return x;
		}
		
		const test = size( 3 );
		'''.parse.assertError(CarmaPackage::eINSTANCE.sizeFunction,CARMAValidator::ERROR_Expression_type_error,"")
	}

	@Test
	def void test_Select_Argument_Error_Arg1(){
		'''
		fun int testFunction( int x ) {
			return x;
		}
		
		const test = select( 3 , true  );
		'''.parse.assertError(CarmaPackage::eINSTANCE.selectFunction,CARMAValidator::ERROR_Expression_type_error,"")
	}

	@Test
	def void test_Filter_Argument_Error_Arg2(){
		'''
		fun int testFunction( int x ) {
			return x;
		}
		
		const test = filter( {: 3 :}  , 3 );
		'''.parse.assertError(CarmaPackage::eINSTANCE.filterFunction,CARMAValidator::ERROR_Expression_type_error,"")
	}

	@Test
	def void test_Exists_Argument_Error_Arg2(){
		'''
		fun int testFunction( int x ) {
			return x;
		}
		
		const test = exist( {: 3 :}  , 3 );
		'''.parse.assertError(CarmaPackage::eINSTANCE.existsFunction,CARMAValidator::ERROR_Expression_type_error,"")
	}

	@Test
	def void test_ForAll_Argument_Error_Arg2(){
		'''
		fun int testFunction( int x ) {
			return x;
		}
		
		const test = forall( {: 3 :}  , 3 );
		'''.parse.assertError(CarmaPackage::eINSTANCE.forAllFunction,CARMAValidator::ERROR_Expression_type_error,"")
	}

	@Test
	def void test_Select_Argument_Error_Arg2(){
		'''
		fun int testFunction( int x ) {
			return x;
		}
		
		const test = select( {: 3 :}  , true );
		'''.parse.assertError(CarmaPackage::eINSTANCE.selectFunction,CARMAValidator::ERROR_Expression_type_error,"")
	}
	
	
}