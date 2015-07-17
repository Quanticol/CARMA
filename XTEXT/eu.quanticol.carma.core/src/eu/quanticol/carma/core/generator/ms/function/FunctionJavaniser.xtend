package eu.quanticol.carma.core.generator.ms.function

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Addition
import eu.quanticol.carma.core.carma.And
import eu.quanticol.carma.core.carma.AtomicCalls
import eu.quanticol.carma.core.carma.AtomicMeasure
import eu.quanticol.carma.core.carma.AtomicNow
import eu.quanticol.carma.core.carma.AtomicOutcome
import eu.quanticol.carma.core.carma.AtomicPrimitive
import eu.quanticol.carma.core.carma.AtomicProcessComposition
import eu.quanticol.carma.core.carma.AtomicRecord
import eu.quanticol.carma.core.carma.AtomicVariable
import eu.quanticol.carma.core.carma.AttribParameter
import eu.quanticol.carma.core.carma.AttribType
import eu.quanticol.carma.core.carma.AttribVariableDeclaration
import eu.quanticol.carma.core.carma.BooleanExpression
import eu.quanticol.carma.core.carma.Calls
import eu.quanticol.carma.core.carma.CarmaBoolean
import eu.quanticol.carma.core.carma.CarmaDouble
import eu.quanticol.carma.core.carma.CarmaExponent
import eu.quanticol.carma.core.carma.CarmaInteger
import eu.quanticol.carma.core.carma.CeilingFunction
import eu.quanticol.carma.core.carma.Comparison
import eu.quanticol.carma.core.carma.Division
import eu.quanticol.carma.core.carma.DoubleParameter
import eu.quanticol.carma.core.carma.DoubleType
import eu.quanticol.carma.core.carma.DoubleVariableDeclaration
import eu.quanticol.carma.core.carma.Equality
import eu.quanticol.carma.core.carma.Expressions
import eu.quanticol.carma.core.carma.FloorFunction
import eu.quanticol.carma.core.carma.FunctionArgument
import eu.quanticol.carma.core.carma.FunctionAssignment
import eu.quanticol.carma.core.carma.FunctionCall
import eu.quanticol.carma.core.carma.FunctionCallArguments
import eu.quanticol.carma.core.carma.FunctionDeclaration
import eu.quanticol.carma.core.carma.FunctionExpression
import eu.quanticol.carma.core.carma.FunctionForStatement
import eu.quanticol.carma.core.carma.FunctionIfStatement
import eu.quanticol.carma.core.carma.FunctionReferenceMan
import eu.quanticol.carma.core.carma.FunctionReferencePre
import eu.quanticol.carma.core.carma.FunctionReturn
import eu.quanticol.carma.core.carma.FunctionStatement
import eu.quanticol.carma.core.carma.InstantiateRecord
import eu.quanticol.carma.core.carma.IntgerParameter
import eu.quanticol.carma.core.carma.IntgerType
import eu.quanticol.carma.core.carma.IntgerVariableDeclaration
import eu.quanticol.carma.core.carma.MaxFunction
import eu.quanticol.carma.core.carma.MinFunction
import eu.quanticol.carma.core.carma.Modulo
import eu.quanticol.carma.core.carma.Multiplication
import eu.quanticol.carma.core.carma.Name
import eu.quanticol.carma.core.carma.Not
import eu.quanticol.carma.core.carma.Or
import eu.quanticol.carma.core.carma.PDFunction
import eu.quanticol.carma.core.carma.Parameter
import eu.quanticol.carma.core.carma.PreArgument
import eu.quanticol.carma.core.carma.PreFunctionCall
import eu.quanticol.carma.core.carma.PredFunctionCallArguments
import eu.quanticol.carma.core.carma.PrimitiveTypes
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.RecordArgument
import eu.quanticol.carma.core.carma.RecordArguments
import eu.quanticol.carma.core.carma.RecordDeclaration
import eu.quanticol.carma.core.carma.RecordParameter
import eu.quanticol.carma.core.carma.RecordReferenceGlobal
import eu.quanticol.carma.core.carma.RecordReferenceMy
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.RecordType
import eu.quanticol.carma.core.carma.Subtraction
import eu.quanticol.carma.core.carma.Type
import eu.quanticol.carma.core.carma.Types
import eu.quanticol.carma.core.carma.UniformFunction
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.VariableReferenceGlobal
import eu.quanticol.carma.core.carma.VariableReferenceMy
import eu.quanticol.carma.core.carma.VariableReferencePure
import eu.quanticol.carma.core.carma.VariableReferenceReceiver
import eu.quanticol.carma.core.carma.VariableReferenceSender
import eu.quanticol.carma.core.generator.ms.SharedJavaniser
import java.util.ArrayList

import static extension org.eclipse.xtext.EcoreUtil2.*

class FunctionJavaniser {
	
	@Inject extension SharedJavaniser

}