/*******************************************************************************
 * Copyright (c) 2016-2019 Embedded Systems and Applications Group
 * Department of Computer Science, Technische Universitaet Darmstadt,
 * Hochschulstr. 10, 64289 Darmstadt, Germany.
 *
 * All rights reserved.
 *
 * This software is provided free for educational use only.
 * It may not be used for commercial purposes without the
 * prior written permission of the authors.
 ******************************************************************************/
package mavlc.context_analysis;

import com.sun.jdi.BooleanType;
import mavlc.errors.*;
import mavlc.syntax.AstNode;
import mavlc.syntax.AstNodeBaseVisitor;
import mavlc.syntax.expression.*;
import mavlc.syntax.function.FormalParameter;
import mavlc.syntax.function.Function;
import mavlc.syntax.module.Module;
import mavlc.syntax.record.RecordElementDeclaration;
import mavlc.syntax.record.RecordTypeDeclaration;
import mavlc.syntax.statement.*;
import mavlc.syntax.type.*;
import mavlc.type.*;

import java.lang.reflect.UndeclaredThrowableException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/* TODO enter group information
 *
 * EiCB group number: 090
 * Names and matriculation numbers of all group members:
 * Thien Nam Vu tv40nusa
 * Quoc Dang Huy Nguyen qn97cuhi
 * Dieu Linh Nguyen dn23qace
 */

/** A combined identification and type checking visitor. */
public class ContextualAnalysis extends AstNodeBaseVisitor<Type, Void> {
	
	protected final ModuleEnvironment env;
	
	protected final IdentificationTable table;
	
	protected Function currentFunction;
	
	/** @param moduleEnvironment an identification table containing the module's functions. */
	public ContextualAnalysis(ModuleEnvironment moduleEnvironment) {
		env = moduleEnvironment;
		table = new IdentificationTable();
	}
	
	private void checkType(AstNode node, Type t1, Type t2) {
		if(!t1.equals(t2)) throw new TypeError(node, t1, t2);
	}
	
	private int evalConstExpr(Expression expr) {
		expr.setType(expr.accept(this));
		return expr.accept(new ConstantExpressionEvaluator(), null);
	}
	
	@Override
	public Type visitTypeSpecifier(TypeSpecifier typeSpecifier, Void __) {
		// no need to set the type for simple type specifiers
		if(typeSpecifier instanceof IntTypeSpecifier) return IntType.instance;
		if(typeSpecifier instanceof VoidTypeSpecifier) return VoidType.instance;
		if(typeSpecifier instanceof BoolTypeSpecifier) return BoolType.instance;
		if(typeSpecifier instanceof FloatTypeSpecifier) return FloatType.instance;
		if(typeSpecifier instanceof StringTypeSpecifier) return StringType.instance;
		throw new InternalCompilerError("visitTypeSpecifier should only be called for simple types");
	}
	
	@Override
	public Type visitRecordTypeSpecifier(RecordTypeSpecifier recordTypeSpecifier, Void __) {
		RecordType type = new RecordType(recordTypeSpecifier.recordTypeName, env.getRecordTypeDeclaration(recordTypeSpecifier.recordTypeName));
		recordTypeSpecifier.setType(type);
		return type;
	}
	
	@Override
	public Type visitVectorTypeSpecifier(VectorTypeSpecifier vectorTypeSpecifier, Void __) {
		Type elementType = vectorTypeSpecifier.elementTypeSpecifier.accept(this);
		if(!elementType.isNumericType()) {
			throw new InapplicableOperationError(vectorTypeSpecifier, elementType, NumericType.class);
		}
		int dim = evalConstExpr(vectorTypeSpecifier.dimensionExpression);
		if(dim <= 0)
			throw new StructureDimensionError(vectorTypeSpecifier, "Vector dimension must be strictly positive");
		
		VectorType type = new VectorType((NumericType) elementType, dim);
		vectorTypeSpecifier.setType(type);
		return type;
	}
	
	@Override
	public Type visitMatrixTypeSpecifier(MatrixTypeSpecifier matrixTypeSpecifier, Void __) {
		Type elementType = matrixTypeSpecifier.elementTypeSpecifier.accept(this);
		if(!elementType.isNumericType()) {
			throw new InapplicableOperationError(matrixTypeSpecifier, elementType, NumericType.class);
		}
		int rows = evalConstExpr(matrixTypeSpecifier.rowsExpression);
		int cols = evalConstExpr(matrixTypeSpecifier.colsExpression);
		if(rows <= 0 || cols <= 0)
			throw new StructureDimensionError(matrixTypeSpecifier, "Matrix dimensions must be strictly positive");
		
		MatrixType type = new MatrixType((NumericType) elementType, rows, cols);
		matrixTypeSpecifier.setType(type);
		return type;
	}
	
	@Override
	public Type visitModule(Module module, Void __) {
		boolean hasMain = false;
		for(RecordTypeDeclaration record : module.records) {
			env.addRecordTypeDeclaration(record);
			record.accept(this);
		}
		for(Function function : module.functions) {
			env.addFunction(function);
		}
		for(Function function : module.functions) {
			currentFunction = function;
			function.accept(this);
			if(isMainFunction(function)) hasMain = true;
		}
		if(!hasMain) {
			throw new MissingMainFunctionError();
		}
		
		return null;
	}
	
	private boolean isMainFunction(Function func) {
		// signature of the main method must be "void main()"
		return func.name.equals("main")
				&& func.parameters.isEmpty()
				&& func.getReturnType() == VoidType.instance;
	}
	
	@Override
	public Type visitFunction(Function function, Void __) {
		table.openNewScope();
		
		if(!function.isReturnTypeSet()) {
			function.setReturnType(function.returnTypeSpecifier.accept(this));
		}
		
		for(FormalParameter param : function.parameters) {
			param.accept(this);
		}
		
		if(function.body.isEmpty() && function.getReturnType().isValueType()) {
			throw new MissingReturnError(function);
		}
		
		for(int i = 0; i < function.body.size(); i++) {
			Statement statement = function.body.get(i);
			if(i == function.body.size() - 1 && function.getReturnType().isValueType()) {
				if(!(statement instanceof ReturnStatement)) {
					throw new MissingReturnError(function);
				}
				ReturnStatement returnStatement = (ReturnStatement) statement;
				Type retVal = returnStatement.returnValue.accept(this);
				checkType(returnStatement, retVal, currentFunction.getReturnType());
				table.closeCurrentScope();
				return retVal;
			} else {
				statement.accept(this);
			}
		}
		
		table.closeCurrentScope();
		return null;
	}
	
	@Override
	public Type visitRecordTypeDeclaration(RecordTypeDeclaration recordTypeDeclaration, Void __) {
		Set<String> elementNames = new HashSet<>();
		for(RecordElementDeclaration element : recordTypeDeclaration.elements) {
			element.accept(this);
			// two elements with the same name
			if(!elementNames.add(element.name))
				throw new RecordElementError(recordTypeDeclaration, recordTypeDeclaration.name, element.name);
			// records cannot contain records
			if(!element.getType().isMemberType())
				throw new RecordElementError(recordTypeDeclaration, recordTypeDeclaration.name, element.name);
		}
		return new RecordType(recordTypeDeclaration.name, recordTypeDeclaration);
	}
	
	@Override
	public Type visitRecordElementDeclaration(RecordElementDeclaration recordElementDeclaration, Void __) {
		Type type = recordElementDeclaration.typeSpecifier.accept(this);
		recordElementDeclaration.setType(type);
		return type;
	}
	
	@Override
	public Type visitDeclaration(Declaration declaration, Void __) {
		declaration.setType(declaration.typeSpecifier.accept(this));
		table.addIdentifier(declaration.name, declaration);
		return declaration.getType();
	}
	
	@Override
	public Type visitValueDefinition(ValueDefinition valueDefinition, Void __) {
		Type rhs = valueDefinition.value.accept(this);
		visitDeclaration(valueDefinition, null);
		Type lhs = valueDefinition.getType();
		checkType(valueDefinition, lhs, rhs);
		return null;
	}
	
	@Override
	public Type visitVariableAssignment(VariableAssignment variableAssignment, Void __) {
		// TODO implement (task 2.2)

		String name = variableAssignment.identifier.name;
		Declaration declaration = table.getDeclaration(name);
		if(declaration==null){
			throw new UndeclaredReferenceError(name);
		}
		Type type =variableAssignment.value.accept(this);
		Type lhsType;
		if(variableAssignment.identifier instanceof VectorLhsIdentifier){
			lhsType = visitVectorLhsIdentifier((VectorLhsIdentifier) variableAssignment.identifier,__);
		}else if(variableAssignment.identifier instanceof MatrixLhsIdentifier){
			lhsType = visitMatrixLhsIdentifier((MatrixLhsIdentifier) variableAssignment.identifier,__);
		}else if(variableAssignment.identifier instanceof RecordLhsIdentifier){
			lhsType = visitRecordLhsIdentifier((RecordLhsIdentifier) variableAssignment.identifier,__);
		}else{
			lhsType = visitLeftHandIdentifier(variableAssignment.identifier,__);
		}

		checkType(variableAssignment,lhsType,type);
		variableAssignment.identifier.setDeclaration(declaration);
		return type;
	}
	
	@Override
	public Type visitLeftHandIdentifier(LeftHandIdentifier leftHandIdentifier, Void __) {
		// TODO implement (task 2.2)
		String name = leftHandIdentifier.name;
		Declaration declaration = table.getDeclaration(name);
		if(declaration==null){
			throw new UndeclaredReferenceError(name);
		}
		if(!declaration.isVariable()){
			throw new ConstantAssignmentError(leftHandIdentifier,declaration);
		}
		Type leftType = declaration.getType();
		return leftType;
	}
	
	@Override
	public Type visitMatrixLhsIdentifier(MatrixLhsIdentifier matrixLhsIdentifier, Void __) {
		// TODO implement (task 2.2)
		String name = matrixLhsIdentifier.name;
		Declaration declaration=table.getDeclaration(name);
		if(declaration==null){
			throw new UndeclaredReferenceError(name);
		}
		if(!declaration.isVariable()){
			throw new ConstantAssignmentError(matrixLhsIdentifier,declaration);
		}
		Type leftType = declaration.getType();
		if(!(leftType instanceof MatrixType)){
			throw new InapplicableOperationError(matrixLhsIdentifier,leftType);
		}
		MatrixType matrixType = (MatrixType) leftType;
		int rowActual = evalConstExpr(matrixLhsIdentifier.rowIndexExpression);
		if(rowActual>=matrixType.rows){
			throw new StructureDimensionError(matrixLhsIdentifier,rowActual,matrixType.rows);
		}
		int colActual = evalConstExpr(matrixLhsIdentifier.colIndexExpression);
		if(colActual>=matrixType.cols){
			throw new StructureDimensionError(matrixLhsIdentifier,colActual,matrixType.cols);
		}

		return matrixType.elementType;
	}
	
	@Override
	public Type visitVectorLhsIdentifier(VectorLhsIdentifier vectorLhsIdentifier, Void __) {
		// TODO implement (task 2.2)
		String name = vectorLhsIdentifier.name;
		Declaration declaration=table.getDeclaration(name);
		if(declaration==null){
			throw new UndeclaredReferenceError(name);
		}
		if(!declaration.isVariable()){
			throw new ConstantAssignmentError(vectorLhsIdentifier,declaration);
		}
		Type leftType = declaration.getType();
		if(!(leftType instanceof VectorType)){
			throw new InapplicableOperationError(vectorLhsIdentifier,leftType);
		}
		VectorType vectorType = (VectorType) leftType;
		int dimActual = evalConstExpr(vectorLhsIdentifier.indexExpression);
		if(dimActual>=vectorType.dimension){
			throw new StructureDimensionError(vectorLhsIdentifier,dimActual,vectorType.dimension);
		}
		return vectorType.elementType;
	}
	
	@Override
	public Type visitRecordLhsIdentifier(RecordLhsIdentifier recordLhsIdentifier, Void __) {
		// TODO implement (task 2.2)
		String name = recordLhsIdentifier.name;
		Declaration declaration=table.getDeclaration(name);
		if(declaration==null){
			throw new UndeclaredReferenceError(name);
		}
		if(!declaration.isVariable()){
			throw new ConstantAssignmentError(recordLhsIdentifier,declaration);
		}
		Type leftType = declaration.getType();
		if(!(leftType instanceof RecordType)){
			throw new InapplicableOperationError(recordLhsIdentifier,leftType);
		}
		RecordType recordType = (RecordType) leftType;

		String elementName = recordLhsIdentifier.elementName;
		RecordElementDeclaration elementDeclaration = recordType.typeDeclaration.getElement(elementName);
		if(elementDeclaration==null){
			throw new RecordElementError(recordLhsIdentifier,recordType.name,elementName);
		}
		if(!elementDeclaration.isVariable()){
			throw new ConstantAssignmentError(recordLhsIdentifier,elementDeclaration);
		}
		Type elementType = elementDeclaration.getType();
		return elementType;
	}
	
	@Override
	public Type visitForLoop(ForLoop forLoop, Void __) {
		// TODO implement (task 2.5)
		table.openNewScope();
		String varInitName = forLoop.initVarName;
		Declaration varInitDeclaration = table.getDeclaration(varInitName);
		if(varInitDeclaration==null){
			throw new UndeclaredReferenceError(varInitName);
		}
		if(!varInitDeclaration.isVariable()){
			throw new ConstantAssignmentError(forLoop,varInitDeclaration);
		}
		forLoop.setInitVarDeclaration(varInitDeclaration);
		Type varInitType = varInitDeclaration.getType();
		Type exprInitType = forLoop.initExpression.accept(this);
		checkType(forLoop,varInitType,exprInitType);

		Type loopConditionType = forLoop.loopCondition.accept(this);
		checkType(forLoop, loopConditionType, BoolType.instance);

		String varIncrName = forLoop.incrVarName;
		Declaration varIncrDeclaration = table.getDeclaration(varIncrName);
		if(varIncrDeclaration==null){
			throw new UndeclaredReferenceError(varIncrName);
		}
		if(!varIncrDeclaration.isVariable()){
			throw new ConstantAssignmentError(forLoop,varIncrDeclaration);
		}
		forLoop.setIncrVarDeclaration(varIncrDeclaration);
		Type varIncrType = varIncrDeclaration.getType();
		Type exprIncrType = forLoop.incrExpression.accept(this);
		checkType(forLoop,varIncrType,exprIncrType);



		table.openNewScope();
		forLoop.body.accept(this);
		table.closeCurrentScope();
		table.closeCurrentScope();
		return null;
	}
	
	@Override
	public Type visitForEachLoop(ForEachLoop forEachLoop, Void __) {
		// check for equal type on both sides of the initializer
		IteratorDeclaration iterator = forEachLoop.iteratorDeclaration;
		table.openNewScope();
		// iterator needs to be in an extra scope
		iterator.accept(this);
		
		// check for correct type on both sides of the colon
		Expression struct = forEachLoop.structExpression;
		Type structType = struct.accept(this);
		if(iterator.isVariable()) {
			// struct must be a variable as well
			if(!(struct instanceof IdentifierReference)) {
				// no declaration to pass here
				throw new ConstantAssignmentError(forEachLoop, null);
			} else if(!((IdentifierReference) struct).getDeclaration().isVariable()) {
				throw new ConstantAssignmentError(forEachLoop, ((IdentifierReference) struct).getDeclaration());
			}
		}
		
		if(structType instanceof StructType) {
			checkType(forEachLoop, ((StructType) structType).elementType, iterator.getType());
		} else {
			throw new InapplicableOperationError(forEachLoop, structType, MatrixType.class, VectorType.class);
		}
		
		// process loop body
		table.openNewScope();
		forEachLoop.body.accept(this);
		table.closeCurrentScope();
		table.closeCurrentScope();
		return null;
	}
	
	@Override
	public Type visitIfStatement(IfStatement ifStatement, Void __) {
		Type testType = ifStatement.condition.accept(this);
		checkType(ifStatement, testType, BoolType.instance);
		
		table.openNewScope();
		ifStatement.thenStatement.accept(this);
		table.closeCurrentScope();
		
		if(ifStatement.hasElseStatement()) {
			assert ifStatement.elseStatement != null;
			table.openNewScope();
			ifStatement.elseStatement.accept(this);
			table.closeCurrentScope();
		}
		return null;
	}
	
	@Override
	public Type visitCallStatement(CallStatement callStatement, Void __) {
		// TODO implement (task 2.6)
		CallExpression callExpression = callStatement.callExpression;
		String functionName = callExpression.functionName;
		Function callee = env.functions.get(functionName);
		if(callee==null){
			throw new UndeclaredReferenceError(functionName);
		}
		callExpression.setCalleeDefinition(callee);
		callee = callExpression.getCalleeDefinition();
		if(callExpression.actualParameters.size()!= callee.parameters.size()){
			throw new ArgumentCountError(callExpression,callee,callee.parameters.size(),callExpression.actualParameters.size());
		}
		for(int i=0;i<callee.parameters.size();i++){
			callee.parameters.get(i).setType(callee.parameters.get(i).typeSpecifier.accept(this));
			Type formalType = callee.parameters.get(i).getType();
			Type actualType = callExpression.actualParameters.get(i).accept(this);
			checkType(callExpression,formalType,actualType);
		}
		table.openNewScope();
		for(int i = 0; i < callee.body.size(); i++) {
			Statement statement = callee.body.get(i);
			statement.accept(this);
		}
		table.closeCurrentScope();
		callee.setReturnType(callee.returnTypeSpecifier.accept(this));
		callStatement.callExpression.setType(VoidType.instance);
		return null;
	}
	
	@Override
	public Type visitReturnStatement(ReturnStatement returnStatement, Void __) {
		throw new MisplacedReturnError(returnStatement);
	}
	
	@Override
	public Type visitCompoundStatement(CompoundStatement compoundStatement, Void __) {
		// TODO implement (task 2.1)
		table.openNewScope();
		for(Statement statement:compoundStatement.statements){
			statement.accept(this);
		}
		table.closeCurrentScope();
		return null;
	}
	
	@Override
	public Type visitSwitchStatement(SwitchStatement switchStatement, Void __) {
		Type testType = switchStatement.condition.accept(this);
		checkType(switchStatement, testType, IntType.instance);
		
		for(Case theCase : switchStatement.cases) {
			theCase.setCondition(evalConstExpr(theCase.conditionExpression));
		}
		
		List<Case> lSC = switchStatement.cases;
		for(int i = 0; i < lSC.size() - 1; i++) {
			for(int j = i + 1; j < lSC.size(); j++) {
				if(lSC.get(i).getCondition() == lSC.get(j).getCondition()) {
					throw new DuplicateCaseError(switchStatement, false, lSC.get(i), lSC.get(j));
				}
			}
		}
		
		List<Default> defaults = switchStatement.defaults;
		
		if(defaults.size() > 1) {
			throw new DuplicateCaseError(switchStatement, true, defaults.get(0), defaults.get(1));
		}
		
		for(SwitchSection curCase : switchStatement.cases) {
			table.openNewScope();
			curCase.accept(this);
			table.closeCurrentScope();
		}
		
		if(defaults.size() == 1) {
			table.openNewScope();
			switchStatement.defaults.get(0).accept(this);
			table.closeCurrentScope();
		}
		
		return null;
	}
	
	@Override
	public Type visitSwitchSection(SwitchSection switchSection, Void __) {
		switchSection.body.accept(this);
		return null;
	}
	
	@Override
	public Type visitMatrixMultiplication(MatrixMultiplication matrixMultiplication, Void __) {
		// TODO implement (task 2.4)
		Type leftOp = matrixMultiplication.leftOperand.accept(this);
		Type rightOp = matrixMultiplication.rightOperand.accept(this);
		if(!(leftOp instanceof MatrixType)){
			throw new InapplicableOperationError(matrixMultiplication,leftOp,MatrixType.class);
		}
		if(!(rightOp instanceof MatrixType)){
			throw new InapplicableOperationError(matrixMultiplication,rightOp,MatrixType.class);
		}
		MatrixType lMat = (MatrixType) leftOp;
		MatrixType rMat = (MatrixType) rightOp;
		checkType(matrixMultiplication,lMat.elementType,rMat.elementType);
		NumericType eType = lMat.elementType;
		if(lMat.cols != rMat.rows){
			throw new StructureDimensionError(matrixMultiplication,lMat.cols,rMat.rows);
		}
		Type res = new MatrixType(eType, lMat.rows, rMat.cols);
		matrixMultiplication.setType(res);
		return res;

	}
	
	@Override
	public Type visitDotProduct(DotProduct dotProduct, Void __) {
		Type leftOp = dotProduct.leftOperand.accept(this);
		Type rightOp = dotProduct.rightOperand.accept(this);
		
		if(!(leftOp instanceof VectorType))
			throw new InapplicableOperationError(dotProduct, leftOp, VectorType.class);
		if(!(rightOp instanceof VectorType))
			throw new InapplicableOperationError(dotProduct, rightOp, VectorType.class);
		
		VectorType lVec = (VectorType) leftOp;
		VectorType rVec = (VectorType) rightOp;
		
		// make sure element types match
		checkType(dotProduct, lVec.elementType, rVec.elementType);
		NumericType eType = lVec.elementType;
		
		// make sure dimensions are compatible
		if(lVec.dimension != rVec.dimension)
			throw new StructureDimensionError(dotProduct, lVec.dimension, rVec.dimension);
		
		dotProduct.setType(eType);
		return eType;
	}
	
	private Type visitArithmeticOperator(BinaryExpression node, boolean allowLeftStruct, boolean allowRightStruct, boolean allowBothStruct) {
		Type lType = node.leftOperand.accept(this);
		Type rType = node.rightOperand.accept(this);
		
		if(lType.isNumericType() && rType.isNumericType()) {
			checkType(node, lType, rType);
			node.setType(lType);
			return lType;
		}
		
		if(lType.isStructType() && rType.isNumericType()) {
			if(!allowLeftStruct)
				throw new InapplicableOperationError(node, lType, IntType.class, FloatType.class);
			checkType(node, ((StructType) lType).elementType, rType);
			node.setType(lType);
			return lType;
		}
		
		if(lType.isNumericType() && rType.isStructType()) {
			if(!allowRightStruct)
				throw new InapplicableOperationError(node, lType, IntType.class, FloatType.class);
			checkType(node, lType, ((StructType) rType).elementType);
			node.setType(rType);
			return rType;
		}
		
		if(lType.isStructType() && rType.isStructType()) {
			if(!allowBothStruct)
				throw new InapplicableOperationError(node, allowLeftStruct ? rType : lType, IntType.class, FloatType.class);
			checkType(node, lType, rType);
			node.setType(lType);
			return lType;
		}
		
		// if we got here, at least one operand is neither a number nor a structure
		if(!lType.isNumericType() && !lType.isStructType()) {
			//noinspection unchecked
			throw new InapplicableOperationError(node, lType, allowLeftStruct
					? new Class[]{IntType.class, FloatType.class, VectorType.class, MatrixType.class}
					: new Class[]{IntType.class, FloatType.class});
		} else {
			//noinspection unchecked
			throw new InapplicableOperationError(node, rType, allowRightStruct
					? new Class[]{IntType.class, FloatType.class, VectorType.class, MatrixType.class}
					: new Class[]{IntType.class, FloatType.class});
		}
	}
	
	@Override
	public Type visitAddition(Addition addition, Void __) {
		return visitArithmeticOperator(addition, false, false, true);
	}
	
	@Override
	public Type visitSubtraction(Subtraction subtraction, Void __) {
		return visitArithmeticOperator(subtraction, false, false, true);
	}
	
	@Override
	public Type visitMultiplication(Multiplication multiplication, Void __) {
		return visitArithmeticOperator(multiplication, true, true, true);
	}
	
	@Override
	public Type visitDivision(Division division, Void __) {
		return visitArithmeticOperator(division, false, false, false);
	}
	
	@Override
	public Type visitExponentiation(Exponentiation exponentiation, Void __) {
		return visitArithmeticOperator(exponentiation, false, false, false);
	}
	
	@Override
	public Type visitCompare(Compare compare, Void __) {
		// TODO implement (task 2.4)
		Type leftOp = compare.leftOperand.accept(this);
		Type rightOp = compare.rightOperand.accept(this);

		if(!(leftOp instanceof NumericType))
			throw new InapplicableOperationError(compare, leftOp, NumericType.class);
		if(!(rightOp instanceof NumericType))
			throw new InapplicableOperationError(compare, rightOp, NumericType.class);

		compare.setType(BoolType.instance);
		return BoolType.instance;	}
	
	@Override
	public Type visitAnd(And and, Void __) {
		return visitBooleanExpression(and);
	}
	
	@Override
	public Type visitOr(Or or, Void __) {
		return visitBooleanExpression(or);
	}
	
	private Type visitBooleanExpression(BinaryExpression exp) {
		Type leftOp = exp.leftOperand.accept(this);
		Type rightOp = exp.rightOperand.accept(this);
		
		if(!(leftOp instanceof BoolType))
			throw new InapplicableOperationError(exp, leftOp, BoolType.class);
		if(!(rightOp instanceof BoolType))
			throw new InapplicableOperationError(exp, rightOp, BoolType.class);
		
		exp.setType(BoolType.instance);
		return BoolType.instance;
	}
	
	@Override
	public Type visitMatrixTranspose(MatrixTranspose matrixTranspose, Void __) {
		// TODO implement (task 2.4)
		Type operand = matrixTranspose.operand.accept(this);
		if(!(operand instanceof MatrixType)){
			throw new InapplicableOperationError(matrixTranspose,operand,MatrixType.class);
		}
		MatrixType mat = (MatrixType) operand;
		NumericType eType = mat.elementType;
		Type res = new MatrixType(eType, mat.cols, mat.rows);
		matrixTranspose.setType(res);
		return res;
	}
	
	@Override
	public Type visitMatrixRows(MatrixRows rows, Void __) {
		Type opType = rows.operand.accept(this);
		if(!(opType instanceof MatrixType))
			throw new InapplicableOperationError(rows, opType, MatrixType.class);
		rows.setType(IntType.instance);
		return IntType.instance;
	}
	
	@Override
	public Type visitMatrixCols(MatrixCols cols, Void __) {
		Type opType = cols.operand.accept(this);
		if(!(opType instanceof MatrixType))
			throw new InapplicableOperationError(cols, opType, MatrixType.class);
		cols.setType(IntType.instance);
		return IntType.instance;
	}
	
	@Override
	public Type visitVectorDimension(VectorDimension vectorDimension, Void __) {
		Type opType = vectorDimension.operand.accept(this);
		if(!(opType instanceof VectorType))
			throw new InapplicableOperationError(vectorDimension, opType, VectorType.class);
		vectorDimension.setType(IntType.instance);
		return IntType.instance;
	}
	
	@Override
	public Type visitUnaryMinus(UnaryMinus unaryMinus, Void __) {
		Type opType = unaryMinus.operand.accept(this);
		if(!opType.isNumericType())
			throw new InapplicableOperationError(unaryMinus, opType, IntType.class, FloatType.class);
		unaryMinus.setType(opType);
		return opType;
	}
	
	@Override
	public Type visitNot(Not not, Void __) {
		Type opType = not.operand.accept(this);
		checkType(not, opType, BoolType.instance);
		not.setType(BoolType.instance);
		return BoolType.instance;
	}
	
	@Override
	public Type visitCallExpression(CallExpression callExpression, Void __) {
		// TODO implement (task 2.6)
		String functionName = callExpression.functionName;
//		if(!callExpression.isCalleeDefinitionSet()){
//			throw new UndeclaredReferenceError(functionName);
//		}
		Function callee = env.functions.get(functionName);
		if(callee==null){
			throw new UndeclaredReferenceError(functionName);
		}
		callExpression.setCalleeDefinition(callee);
		callee = callExpression.getCalleeDefinition();
		if(callExpression.actualParameters.size()!= callee.parameters.size()){
			throw new ArgumentCountError(callExpression,callee,callee.parameters.size(),callExpression.actualParameters.size());
		}
		for(int i=0;i<callee.parameters.size();i++){
			callee.parameters.get(i).setType(callee.parameters.get(i).typeSpecifier.accept(this));
			Type formalType = callee.parameters.get(i).getType();
			Type actualType = callExpression.actualParameters.get(i).accept(this);
			checkType(callExpression,formalType,actualType);
		}
		table.openNewScope();
		for(int i = 0; i < callee.body.size(); i++) {
			Statement statement = callee.body.get(i);
			if(i == callee.body.size() - 1) {
				if(!(statement instanceof ReturnStatement)) {
					throw new MissingReturnError(callee);
				}
				ReturnStatement returnStatement = (ReturnStatement) statement;
				Type retVal = returnStatement.returnValue.getType();
				checkType(returnStatement, retVal, callee.getReturnType());
			} else {
				statement.accept(this);
			}
		}
		table.closeCurrentScope();
		callee.setReturnType(callee.returnTypeSpecifier.accept(this));
		callExpression.setType(callee.getReturnType());
		return callee.getReturnType();
	}
	
	@Override
	public Type visitElementSelect(ElementSelect elementSelect, Void __) {
		Type baseType = elementSelect.structExpression.accept(this);
		if(!(baseType instanceof StructType))
			throw new InapplicableOperationError(elementSelect, baseType, MatrixType.class, VectorType.class);
		
		Type indexType = elementSelect.indexExpression.accept(this);
		if(!indexType.equals(IntType.instance))
			throw new TypeError(elementSelect, indexType, IntType.instance);
		
		if(baseType instanceof VectorType) {
			Type resultType = ((VectorType) baseType).elementType;
			elementSelect.setType(resultType);
			return resultType;
		} else if(baseType instanceof MatrixType) {
			NumericType elementType = ((MatrixType) baseType).elementType;
			int size = ((MatrixType) baseType).cols;
			Type resultType = new VectorType(elementType, size);
			elementSelect.setType(resultType);
			return resultType;
		}
		return null;
	}
	
	@Override
	public Type visitRecordElementSelect(RecordElementSelect recordElementSelect, Void __) {
		Type baseType = recordElementSelect.recordExpression.accept(this);
		if(!(baseType instanceof RecordType)) {
			throw new InapplicableOperationError(recordElementSelect, baseType, RecordType.class);
		}
		String elementName = recordElementSelect.elementName;
		RecordElementDeclaration element =
				(((RecordType) baseType).typeDeclaration.getElement(elementName));
		if(element == null) {
			throw new RecordElementError(recordElementSelect, ((RecordType) baseType).name, elementName);
		}
		recordElementSelect.setType(element.getType());
		return element.getType();
	}
	
	@Override
	public Type visitSubMatrix(SubMatrix subMatrix, Void __) {
		int rso = evalConstExpr(subMatrix.rowStartOffsetExpression);
		int reo = evalConstExpr(subMatrix.rowEndOffsetExpression);
		int cso = evalConstExpr(subMatrix.colStartOffsetExpression);
		int ceo = evalConstExpr(subMatrix.colEndOffsetExpression);
		int rows = reo - rso + 1;
		int cols = ceo - cso + 1;
		
		subMatrix.setRowStartOffset(rso);
		subMatrix.setRowEndOffset(reo);
		subMatrix.setColStartOffset(cso);
		subMatrix.setColEndOffset(ceo);
		
		Type rowBaseType = subMatrix.rowBaseIndexExpression.accept(this);
		checkType(subMatrix, rowBaseType, IntType.instance);
		Type colBaseType = subMatrix.colBaseIndexExpression.accept(this);
		checkType(subMatrix, colBaseType, IntType.instance);
		
		Type baseType = subMatrix.structExpression.accept(this);
		if(!(baseType instanceof MatrixType))
			throw new InapplicableOperationError(subMatrix, baseType, MatrixType.class);
		MatrixType matrix = (MatrixType) baseType;
		
		if(reo < rso) throw new StructureDimensionError(subMatrix, reo, rso);
		if(ceo < cso) throw new StructureDimensionError(subMatrix, ceo, cso);
		if(matrix.rows < rows) throw new StructureDimensionError(subMatrix, matrix.rows, rows);
		if(matrix.cols < cols) throw new StructureDimensionError(subMatrix, matrix.cols, cols);
		
		Type resultType = new MatrixType(((MatrixType) baseType).elementType, rows, cols);
		subMatrix.setType(resultType);
		return resultType;
	}
	
	@Override
	public Type visitSubVector(SubVector subVector, Void __) {
		int so = evalConstExpr(subVector.startOffsetExpression);
		int eo = evalConstExpr(subVector.endOffsetExpression);
		int size = eo - so + 1;
		
		subVector.setStartOffset(so);
		subVector.setEndOffset(eo);
		
		Type indexType = subVector.baseIndexExpression.accept(this);
		checkType(subVector, indexType, IntType.instance);
		Type baseType = subVector.structExpression.accept(this);
		if(!(baseType instanceof VectorType)) {
			throw new InapplicableOperationError(subVector, baseType, VectorType.class);
		}
		VectorType vector = (VectorType) baseType;
		if(eo < so) {
			throw new StructureDimensionError(subVector, eo, so);
		}
		if(vector.dimension < size) {
			throw new StructureDimensionError(subVector, vector.dimension, size);
		}
		
		Type resultType = new VectorType(((VectorType) baseType).elementType, size);
		subVector.setType(resultType);
		return resultType;
	}
	
	@Override
	public Type visitStructureInit(StructureInit structureInit, Void __) {
		// The type of the first element determines the structure
		Type firstElem = structureInit.elements.get(0).accept(this);
		if(firstElem instanceof VectorType) {
			// Matrix init
			NumericType elemType = ((VectorType) firstElem).elementType;
			int size = ((VectorType) firstElem).dimension;
			int x = 0;
			for(Expression element : structureInit.elements) {
				Type t = element.accept(this);
				checkType(structureInit, firstElem, t);
				++x;
			}
			MatrixType resultType = new MatrixType(elemType, x, size);
			structureInit.setType(resultType);
			return resultType;
		} else {
			// Vector init
			if(!firstElem.isNumericType()) {
				throw new InapplicableOperationError(structureInit, firstElem, IntType.class, FloatType.class);
			}
			NumericType elemType = (NumericType) firstElem;
			int size = 0;
			for(Expression element : structureInit.elements) {
				Type t = element.accept(this);
				checkType(structureInit, elemType, t);
				++size;
			}
			VectorType resultType = new VectorType(elemType, size);
			structureInit.setType(resultType);
			return resultType;
		}
	}
	
	@Override
	public Type visitRecordInit(RecordInit recordInit, Void __) {
		// TODO implement (task 2.4)
		String name = recordInit.typeName;
		RecordTypeDeclaration recordTypeDeclaration = env.getRecordTypeDeclaration(name);

		if (recordTypeDeclaration == null) {
			throw new UndeclaredReferenceError(name);
		}
		List<RecordElementDeclaration> elements = recordTypeDeclaration.elements;
		for (RecordElementDeclaration element : elements) {
			Type elementType = element.accept(this);
			checkType(element, element.getType(), elementType);
		}
		Type recordType = new RecordType(recordTypeDeclaration.name, recordTypeDeclaration);
		recordInit.setType(recordType);
		return recordType;
	}
	
	@Override
	public Type visitStringValue(StringValue stringValue, Void __) {
		return StringType.instance;
	}
	
	@Override
	public Type visitBoolValue(BoolValue boolValue, Void __) {
		return BoolType.instance;
	}
	
	@Override
	public Type visitIntValue(IntValue intValue, Void __) {
		return IntType.instance;
	}
	
	@Override
	public Type visitFloatValue(FloatValue floatValue, Void __) {
		return FloatType.instance;
	}
	
	@Override
	public Type visitIdentifierReference(IdentifierReference identifierReference, Void __) {
		Declaration decl = table.getDeclaration(identifierReference.name);
		identifierReference.setDeclaration(decl);
		identifierReference.setType(decl.getType());
		return decl.getType();
	}
	
	@Override
	public Type visitSelectExpression(SelectExpression exp, Void __) {
		Type testType = exp.condition.accept(this);
		checkType(exp, testType, BoolType.instance);
		
		Type trueType = exp.trueCase.accept(this);
		Type falseType = exp.falseCase.accept(this);
		checkType(exp, trueType, falseType);
		exp.setType(trueType);
		return trueType;
	}
}
