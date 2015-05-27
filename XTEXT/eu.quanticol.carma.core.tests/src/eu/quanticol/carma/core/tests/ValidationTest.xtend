package eu.quanticol.carma.core.tests
//
import com.google.inject.Inject
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.junit.Test
import org.junit.runner.RunWith
import eu.quanticol.carma.core.CARMAInjectorProvider
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.validation.CARMAValidator
import eu.quanticol.carma.core.carma.CarmaPackage

//
@RunWith(typeof(XtextRunner))
@InjectWith(typeof(CARMAInjectorProvider))
class ValidationTest {
	
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper
	
	
	/**
	 * 		
	    fun integer Test(integer v){
			return v + 1;
		}
		
		fun integer Test2(integer v){
			return v + 1;
		}
				
		component Comp1(){
			store{
				enum a := 0;
				record b := {x := 1, y :=1}; 
			}
				
			behaviour{
				P = [b == {x := 1, y:= 1}] nothing*<a,b,b>{a := a + 1}.P;
			}
			
			init{
				P;
			}
		}
		
		component Comp2(Z){
			store{
				enum a := 0;
				enum e := 0;
			}
				
			behaviour{
				P =  nothing*(b,c).P;
			}
			
			init{
				Z;
			}
		}
		
		Q = Q;
				
		system Bleh{
			collective{
				new Comp1();
				new Comp2(P);
			}
			environment{
				store{
					record loc := {x := 1, y :=1}; 
				}
			}
		}
	 */
	
	
	@Test
	def void test_ERROR_ActionStub_reference(){
		'''
		component Producer(){
		    store{
		        enum product := 0;
		    }
		
		    behaviour{
		        Produce = produce*<>{my.product := my.product + 1}.Produce;
		    }
		
		    init{
		        Produce;
		    }
		
		}
		
		system SimpleMove{
		
		    collective{
		        new Producer();
		    }
		
		    environment{
		        rate{
		        //problem here...
		        [True] ed* := 1;
		        }
		    }
		}
		'''.parse.assertError(CarmaPackage::eINSTANCE.actionStub,
			CARMAValidator::ERROR_ActionStub_reference,
			CARMAValidator::ERROR_ActionStub_reference)
	}
	
	@Test
	def void test_ERROR_ActionStub(){
		'''
		component Producer(){
		    store{
		        enum product := 0;
		    }
		
		    behaviour{
		        Produce = produce*.Produce;
		    }
		
		    init{
		        Produce;
		    }
		
		}
		
		component ProducerT(){
		    store{
		        enum product := 0;
		    }
		
		    behaviour{
		        Produce = produce*.Produce;
		    }
		
		    init{
		        Produce;
		    }
		
		}
		
		system SimpleMove{
		
		    collective{
		        new Producer();
		        new ProducerT();
		    }
		
		    environment{
		        rate{
		        [True] edward* := 1;
		        }
		    }
		}
		'''.parse.assertError(CarmaPackage::eINSTANCE.actionStub,
			CARMAValidator::ERROR_ActionStub_reference,
			CARMAValidator::ERROR_ActionStub_reference)
	}
	
	@Test
	def void test_ERROR_BooleanExpression_expression_boolean_type(){
		'''
		fun integer Test(integer v){
			return v + 1;
		}
		
		fun integer Test2(integer v){
			return v + 1;
		}
				
		component Comp1(){
			store{
				enum a := 0;
				record b := {x := 1, y :=1}; 
			}
				
			behaviour{
				P = [b + 1] nothing*.P;
			}
			
			init{
				P;
			}
		}
		
		component Comp2(Z){
			store{
				enum a := 0;
				enum e := 0;
			}
				
			behaviour{
				P =  nothing*.P;
			}
			
			init{
				Z;
			}
		}
		
		Q = Q;
				
		system Bleh{
			collective{
				new Comp1();
				new Comp2(P);
			}
			environment{
				store{
					record loc := {x := 1, y :=1}; 
				}
			}
		}
		'''.parse.assertError(CarmaPackage::eINSTANCE.booleanExpression,
			CARMAValidator::ERROR_BooleanExpression_expression_boolean_type,
			CARMAValidator::ERROR_BooleanExpression_expression_boolean_type)
	}
	
	@Test
	def void test_WARN_ComponentBlockDefinition_unused(){
		'''
		component Producer(){
		    store{
		        enum product := 0;
		    }
		
		    behaviour{
		        Produce = produce*{product := product + 1}.Send;
		        Send = send<1>{product := product - 1}.Produce;
		    }
		
		    init{
		        Produce;
		    }
		
		}
		
		component Consumer(){
		    store{
		        enum product := 0;
		    }
		
		    behaviour{
		        Receive = send(z){product := product + z}.Consume;
		        Consume = consume*{product := product + 1}.Receive;
		    }
		
		    init{
		        Produce;
		    }
		
		}
		
		system Simple{
		
		    collective{
		        new Producer();
		    }
		
		    environment{
		        rate{
		        [True] produce* := 1;
		        }
		    }
		}'''.parse.assertWarning(CarmaPackage::eINSTANCE.componentBlockDefinition,
			CARMAValidator::WARN_ComponentBlockDefinition_unused,
			CARMAValidator::WARN_ComponentBlockDefinition_unused)
	}
	
	@Test
	def void test_ERROR_MacroExpressionReference_noAccess(){
	'''
	component Producer(){
	    store{
	        enum product := 0;
	    }
	
	    behaviour{
	        Produce = produce*{product := product + 1}.Produce;
	        Send = send<>{product := product - 1}.Send;
	    }
	
	    init{
	        Produce | Send;
	    }
	
	}
	
	component Consumer(){
	    store{
	        enum product := 0;
	    }
	
	    behaviour{
	        Receive = send(){product := product + 1}.Receive;
	        Consume = consume*{product := product - 1}.Consume;
	    }
	
	    init{
	        Produce | Consume;
	    }
	
	}
	
	system Simple{
	
	    collective{
	        new Producer();
	        new Consumer();
	    }
	
	    environment{
	        rate{
	        [True] produce* := 1;
	        }
	    }
	}
	'''.parse.assertError(CarmaPackage::eINSTANCE.macroExpressionReference,
			CARMAValidator::ERROR_MacroExpressionReference_noAccess,
			CARMAValidator::ERROR_MacroExpressionReference_noAccess)
	}
	
	@Test
	def void test_ERROR_ActionName_type_unique(){
		'''
		component Producer(){
		    store{
		        enum product := 0;
		    }
		
		    behaviour{
		       P = move*.Q;
		       Q = move<>.R;
		       R = move().P;
		    }
		
		    init{
		        P;
		    }
		
		}
		
		system SimpleMove{
		
		    collective{
		        new Producer();
		    }
		
		    environment{
		    }
		}
		'''.parse.assertError(CarmaPackage::eINSTANCE.actionName,
			CARMAValidator::ERROR_ActionName_type_unique,
			CARMAValidator::ERROR_ActionName_type_unique)
	}
	
	@Test
	def void test_ERROR_CBND_reference(){
		'''
		component Producer(){
		    store{
		        enum product := 0;
		    }
		
		    behaviour{
		        Produce = [my.product > 0] produce*{product := product + 1}.Produce + [my.product == 0] produceDouble*{product := product + 2}.Produce;
		        Send = [my.product > 0] send*{product := product - 1}.Send;
		    }
		
		    init{
		        Produce | Send;
		    }
		
		}
		
		
		system Simple{
		
		    collective{
		        new Producer();
		        //heh? validation rule missing... There can be no declaration without a definition
		        new Consumer();
		    }
		
		    environment{
		        rate{
		        [True] produce* := 1;
		        [True] produceDouble* := 1;
		        [True] send* := 1;
		        }
		    }
		}'''.parse.assertError(CarmaPackage::eINSTANCE.CBND,
			CARMAValidator::ERROR_CBND_reference,
			CARMAValidator::ERROR_CBND_reference)
	}
	
	@Test
	def void test_ERROR_CBND_matching(){
		'''
		component Producer(){
		    store{
		        enum product := 0;
		    }
		
		    behaviour{
		        Produce = [my.product > 0] produce*{product := product + 1}.Produce + [my.product == 0] produceDouble*{product := product + 2}.Produce;
		        Send = [my.product > 0] send*{product := product - 1}.Send;
		    }
		
		    init{
		        Produce | Send;
		    }
		
		}
		
		
		system Simple{
		
		    collective{
		        new Producer(1);
		        new Producer();
		    }
		
		    environment{
		        rate{
		        [True] produce* := 1;
		        [True] produceDouble* := 1;
		        [True] send* := 1;
		        }
		    }
		}'''.parse.assertError(CarmaPackage::eINSTANCE.CBND,
			CARMAValidator::ERROR_CBND_matching,
			CARMAValidator::ERROR_CBND_matching)
	}
	
	@Test
	def void test_ERROR_ProcessExpressionGuard_following_action(){
		'''
		component Producer(){
		    store{
		        enum product := 0;
		    }
		
		    behaviour{
		        P = [True] P;
		    }
		
		    init{
		        P;
		    }
		
		}
		
		
		system Simple{
		
		    collective{
		        new Producer();
		    }
		
		    environment{
		    }
		}
		'''.parse.assertError(CarmaPackage::eINSTANCE.processExpressionGuard,
			CARMAValidator::ERROR_ProcessExpressionGuard_following_action,
			CARMAValidator::ERROR_ProcessExpressionGuard_following_action)
		
	}
	
	@Test
	def void test_ERROR_Rate_Unique(){
			'''
	component Producer(enum a, Z){
	    store{
	        enum product := a;
	        record position := {x := 0, y := 1};
	    }
	
	    behaviour{
	        Produce = [my.product > 0] produce*{product := product + 1}.Produce + [my.product == 0] produceDouble*{product := product + 2}.Produce;
	        Send = [my.product > 0] send<1>{product := product - 1}.Send;
	    }
	
	    init{
	        Z;
	    }
	}
	
	component Consumer(enum a, Z){
	    
	    store{
	        enum product := a;
	    }
	
	    behaviour{
	        Consume = [my.product > 0] consume*{product := product - 1}.Consume + [my.product > 2] consumeDouble*{product := product - 2}.Consume;
	        Receive = [my.product > 0] send(z){product := product - z}.Receive;
	    }
	
	    init{
	        Z;
	    }
	}
	
	
	system Simple{
	
	    collective{
	        new Producer(1..6,Produce|Send);
	        new Consumer(1..6,Consume|Receive);
	    }
	
	    environment{
	    	
	        rate{
	        [True] produce* := 1;
	        [True] send := 1;
	        [True] send := 1;
	        [True] produceDouble* := 1;
	        }
	    }
	}
	'''.parse.assertError(CarmaPackage::eINSTANCE.rate,
			CARMAValidator::ERROR_Rate_Unique,
			CARMAValidator::ERROR_Rate_Unique)
	}
	
	
}