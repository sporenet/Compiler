//------------------------------------------------------------------------------
/// @brief SnuPL abstract syntax tree
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/09/28 Bernhard Egger assignment 2: AST for SnuPL/-1
///
/// @section license_section License
/// Copyright (c) 2012-2014, Bernhard Egger
/// All rights reserved.
///
/// Redistribution and use in source and binary forms,  with or without modifi-
/// cation, are permitted provided that the following conditions are met:
///
/// - Redistributions of source code must retain the above copyright notice,
///   this list of conditions and the following disclaimer.
/// - Redistributions in binary form must reproduce the above copyright notice,
///   this list of conditions and the following disclaimer in the documentation
///   and/or other materials provided with the distribution.
///
/// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
/// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,  BUT NOT LIMITED TO,  THE
/// IMPLIED WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE
/// ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDER  OR CONTRIBUTORS BE
/// LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSE-
/// QUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF  SUBSTITUTE
/// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
/// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN  CONTRACT, STRICT
/// LIABILITY, OR TORT  (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY WAY
/// OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
/// DAMAGE.
//------------------------------------------------------------------------------

#include <iostream>
#include <cassert>

#include <typeinfo>

#include "ast.h"
using namespace std;


//------------------------------------------------------------------------------
// CAstNode
//
int CAstNode::_global_id = 0;

CAstNode::CAstNode(CToken token)
  : _token(token), _addr(NULL)
{
  _id = _global_id++;
}

CAstNode::~CAstNode(void)
{
  if (_addr != NULL) delete _addr;
}

int CAstNode::GetID(void) const
{
  return _id;
}

CToken CAstNode::GetToken(void) const
{
  return _token;
}

const CType* CAstNode::GetType(void) const
{
  return CTypeManager::Get()->GetNull();
}

string CAstNode::dotID(void) const
{
  ostringstream out;
  out << "node" << dec << _id;
  return out.str();
}

string CAstNode::dotAttr(void) const
{
  return " [label=\"" + dotID() + "\"]";
}

void CAstNode::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << dotID() << dotAttr() << ";" << endl;
}

CTacAddr* CAstNode::ToTac(CCodeBlock *cb)
{
  return NULL;
}

CTacAddr* CAstNode::GetTacAddr(void) const
{
  return NULL;
}

ostream& operator<<(ostream &out, const CAstNode &t)
{
  return t.print(out);
}

ostream& operator<<(ostream &out, const CAstNode *t)
{
  return t->print(out);
}

//------------------------------------------------------------------------------
// CAstScope
//
CAstScope::CAstScope(CToken t, const string name, CAstScope *parent)
  : CAstNode(t), _name(name), _symtab(NULL), _parent(parent), _statseq(NULL),
    _cb(NULL)
{
  if (_parent != NULL) _parent->AddChild(this);
}

CAstScope::~CAstScope(void)
{
  delete _symtab;
  delete _statseq;
  delete _cb;
}

const string CAstScope::GetName(void) const
{
  return _name;
}

CAstScope* CAstScope::GetParent(void) const
{
  return _parent;
}

size_t CAstScope::GetNumChildren(void) const
{
  return _children.size();
}

CAstScope* CAstScope::GetChild(size_t i) const
{
  assert(i < _children.size());
  return _children[i];
}

CSymtab* CAstScope::GetSymbolTable(void) const
{
  assert(_symtab != NULL);
  return _symtab;
}

void CAstScope::SetStatementSequence(CAstStatement *statseq)
{
  _statseq = statseq;
}

CAstStatement* CAstScope::GetStatementSequence(void) const
{
  return _statseq;
}

bool CAstScope::TypeCheck(CToken *t, string *msg) const
{
  bool result = true;
	try {
		CAstStatement *s = _statseq;
		while (result && (s != NULL)) {
			result = s->TypeCheck(t, msg);
			s = s->GetNext();
		}
		vector<CAstScope*>::const_iterator it = _children.begin();
		while (result && (it != _children.end())) {
			result = (*it)->TypeCheck(t, msg);
			it++;
		}
	} catch (...) {
		result = false;
	}
	return result;
}

ostream& CAstScope::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "CAstScope: '" << _name << "'" << endl;
  out << ind << "  symbol table:" << endl;
  _symtab->print(out, indent+4);
  out << ind << "  statement list:" << endl;
  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    do {
      s->print(out, indent+4);
      s = s->GetNext();
    } while (s != NULL);
  } else {
    out << ind << "    empty." << endl;
  }

  out << ind << "  nested scopes:" << endl;
  if (_children.size() > 0) {
    for (size_t i=0; i<_children.size(); i++) {
      _children[i]->print(out, indent+4);
    }
  } else {
    out << ind << "    empty." << endl;
  }
  out << ind << endl;

  return out;
}

void CAstScope::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    string prev = dotID();
    do {
      s->toDot(out, indent);
      out << ind << prev << " -> " << s->dotID() << " [style=dotted];" << endl;
      prev = s->dotID();
      s = s->GetNext();
    } while (s != NULL);
  }

  vector<CAstScope*>::const_iterator it = _children.begin();
  while (it != _children.end()) {
    CAstScope *s = *it++;
    s->toDot(out, indent);
    out << ind << dotID() << " -> " << s->dotID() << ";" << endl;
  }

}

CTacAddr* CAstScope::ToTac(CCodeBlock *cb)
{
  assert(cb != NULL);
	CAstStatement *s = GetStatementSequence();
	while (s != NULL) {
		CTacLabel *next = cb->CreateLabel();
		s->ToTac(cb, next);
		cb->AddInstr(next);
		s = s->GetNext();
	}
	cb->CleanupControlFlow();
	return NULL;
}

CCodeBlock* CAstScope::GetCodeBlock(void) const
{
  return _cb;
}

void CAstScope::SetSymbolTable(CSymtab *st)
{
  if (_symtab != NULL) delete _symtab;
  _symtab = st;
}

void CAstScope::AddChild(CAstScope *child)
{
  _children.push_back(child);
}


//------------------------------------------------------------------------------
// CAstModule
//
CAstModule::CAstModule(CToken t, const string name)
  : CAstScope(t, name, NULL)
{
  SetSymbolTable(new CSymtab());
}

CSymbol* CAstModule::CreateVar(const string ident, const CType *type)
{
  return new CSymGlobal(ident, type);
}

string CAstModule::dotAttr(void) const
{
  return " [label=\"m " + GetName() + "\",shape=box]";
}



//------------------------------------------------------------------------------
// CAstProcedure
//
CAstProcedure::CAstProcedure(CToken t, const string name,
                             CAstScope *parent, CSymProc *symbol)
  : CAstScope(t, name, parent), _symbol(symbol)
{
  assert(GetParent() != NULL);
  SetSymbolTable(new CSymtab(GetParent()->GetSymbolTable()));
  assert(_symbol != NULL);
}

CSymProc* CAstProcedure::GetSymbol(void) const
{
  return _symbol;
}

CSymbol* CAstProcedure::CreateVar(const string ident, const CType *type)
{
  return new CSymLocal(ident, type);
}

const CType* CAstProcedure::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

string CAstProcedure::dotAttr(void) const
{
  return " [label=\"p/f " + GetName() + "\",shape=box]";
}


//------------------------------------------------------------------------------
// CAstType
//
CAstType::CAstType(CToken t, const CType *type)
  : CAstNode(t), _type(type)
{
  assert(type != NULL);
}

const CType* CAstType::GetType(void) const
{
  return _type;
}

ostream& CAstType::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "CAstType (" << _type << ")" << endl;
  return out;
}


//------------------------------------------------------------------------------
// CAstStatement
//
CAstStatement::CAstStatement(CToken token)
  : CAstNode(token), _next(NULL)
{
}

CAstStatement::~CAstStatement(void)
{
  delete _next;
}

void CAstStatement::SetNext(CAstStatement *next)
{
  _next = next;
}

CAstStatement* CAstStatement::GetNext(void) const
{
  return _next;
}

CTacAddr* CAstStatement::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  cb->AddInstr(new CTacInstr(opGoto, next));
	return NULL;
}


//------------------------------------------------------------------------------
// CAstStatAssign
//
CAstStatAssign::CAstStatAssign(CToken t,
                               CAstDesignator *lhs, CAstExpression *rhs)
  : CAstStatement(t), _lhs(lhs), _rhs(rhs)
{
  assert(lhs != NULL);
  assert(rhs != NULL);
}

CAstDesignator* CAstStatAssign::GetLHS(void) const
{
  return _lhs;
}

CAstExpression* CAstStatAssign::GetRHS(void) const
{
  return _rhs;
}

bool CAstStatAssign::TypeCheck(CToken *t, string *msg) const
{
  CAstDesignator *lhs = GetLHS();
  CAstExpression *rhs = GetRHS();
  const CType *lt = GetType();
  const CType *rt;
  if (!rhs->TypeCheck(t, msg)) return false;
  if (!lt->Match(rhs->GetType())) {
    if (t != NULL) *t = GetToken();
    if (msg != NULL) *msg = "incompatible types in assignment.";
  return false;
  }
  return true;
}

const CType* CAstStatAssign::GetType(void) const
{
  return _lhs->GetType();
}

ostream& CAstStatAssign::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << ":=" << " ";

  out << endl;

  _lhs->print(out, indent+2);
  _rhs->print(out, indent+2);

  return out;
}

string CAstStatAssign::dotAttr(void) const
{
  return " [label=\":=\",shape=box]";
}

void CAstStatAssign::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _lhs->toDot(out, indent);
  out << ind << dotID() << "->" << _lhs->dotID() << ";" << endl;
  _rhs->toDot(out, indent);
  out << ind << dotID() << "->" << _rhs->dotID() << ";" << endl;
}

CTacAddr* CAstStatAssign::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  cb->AddInstr(new CTacInstr(opAssign, GetLHS()->ToTac(cb, NULL, NULL), GetRHS()->ToTac(cb, NULL, NULL)));
	cb->AddInstr(new CTacInstr(opGoto, next));
	return NULL;
}


//------------------------------------------------------------------------------
// CAstStatCall
//
CAstStatCall::CAstStatCall(CToken t, CAstFunctionCall *call)
  : CAstStatement(t), _call(call)
{
  assert(call != NULL);
}

CAstFunctionCall* CAstStatCall::GetCall(void) const
{
  return _call;
}

bool CAstStatCall::TypeCheck(CToken *t, string *msg) const
{
  return GetCall()->TypeCheck(t, msg);
}

ostream& CAstStatCall::print(ostream &out, int indent) const
{
  _call->print(out, indent);

  return out;
}

string CAstStatCall::dotID(void) const
{
  return _call->dotID();
}

string CAstStatCall::dotAttr(void) const
{
  return _call->dotAttr();
}

void CAstStatCall::toDot(ostream &out, int indent) const
{
  _call->toDot(out, indent);
}

CTacAddr* CAstStatCall::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  GetCall()->ToTac(cb, NULL, NULL);
	cb->AddInstr(new CTacInstr(opGoto, next));
	return NULL;
}


//------------------------------------------------------------------------------
// CAstStatReturn
//
CAstStatReturn::CAstStatReturn(CToken t, CAstScope *scope, CAstExpression *expr)
  : CAstStatement(t), _scope(scope), _expr(expr)
{
  assert(scope != NULL);
}

CAstScope* CAstStatReturn::GetScope(void) const
{
  return _scope;
}

CAstExpression* CAstStatReturn::GetExpression(void) const
{
  return _expr;
}

bool CAstStatReturn::TypeCheck(CToken *t, string *msg) const
{
	const CType *st = GetScope()->GetType();
	CAstExpression *e = GetExpression();
	if (st->Match(CTypeManager::Get()->GetNull())) {
		if (e != NULL) {
			if (t != NULL) *t = e->GetToken();
			if (msg != NULL) *msg = "superfluous expression after return.";
			return false;
		}
	} else {
		if (e == NULL) {
			if (t != NULL) *t = GetToken();
			if (msg != NULL) *msg = "expression expected after return.";
			return false;
		}
		if (!e->TypeCheck(t, msg)) return false;
		if (!st->Match(e->GetType())) {
			if (t != NULL) *t = e->GetToken();
			if (msg != NULL) *msg = "return type mismatch.";
			return false;
		}
	}
	return true;
}

const CType* CAstStatReturn::GetType(void) const
{
  const CType *t = NULL;

  if (GetExpression() != NULL) {
    t = GetExpression()->GetType();
  } else {
    t = CTypeManager::Get()->GetNull();
  }

  return t;
}

ostream& CAstStatReturn::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "return" << " ";

  out << endl;

  if (_expr != NULL) _expr->print(out, indent+2);

  return out;
}

string CAstStatReturn::dotAttr(void) const
{
  return " [label=\"return\",shape=box]";
}

void CAstStatReturn::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  if (_expr != NULL) {
    _expr->toDot(out, indent);
    out << ind << dotID() << "->" << _expr->dotID() << ";" << endl;
  }
}

CTacAddr* CAstStatReturn::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  if (GetExpression() == NULL) {
		cb->AddInstr(new CTacInstr(opReturn, NULL, NULL, NULL));
	} else {
		cb->AddInstr(new CTacInstr(opReturn, NULL, GetExpression()->ToTac(cb, NULL, NULL), NULL));
	}
	cb->AddInstr(new CTacInstr(opGoto, next));
	return NULL;
}


//------------------------------------------------------------------------------
// CAstStatIf
//
CAstStatIf::CAstStatIf(CToken t, CAstExpression *cond,
                       CAstStatement *ifBody, CAstStatement *elseBody)
  : CAstStatement(t), _cond(cond), _ifBody(ifBody), _elseBody(elseBody)
{
  assert(cond != NULL);
}

CAstExpression* CAstStatIf::GetCondition(void) const
{
  return _cond;
}

CAstStatement* CAstStatIf::GetIfBody(void) const
{
  return _ifBody;
}

CAstStatement* CAstStatIf::GetElseBody(void) const
{
  return _elseBody;
}

bool CAstStatIf::TypeCheck(CToken *t, string *msg) const
{
  CAstExpression *cond = GetCondition();
	CAstStatement *ifBody = GetIfBody();
	CAstStatement *elseBody = GetElseBody();
	if (!cond->TypeCheck(t, msg)) return false;
	const CType *ct = cond->GetType();
	if (!ct->Match(CTypeManager::Get()->GetBool())) {
		if (t != NULL) *t = cond->GetToken();
		if (msg != NULL) *msg = "boolean expression expected.";
		return false;
	}
	if (ifBody == NULL) {
		if (elseBody == NULL) return true;
		if (elseBody->TypeCheck(t, msg)) return true;
		return false;
	}
	if (ifBody->TypeCheck(t, msg)) {
		if (elseBody == NULL) return true;
		if (elseBody->TypeCheck(t, msg)) return true;
	}
	return false;
}

ostream& CAstStatIf::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "if cond" << endl;
  _cond->print(out, indent+2);
  out << ind << "if-body" << endl;
  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  } else out << ind << "  empty." << endl;
  out << ind << "else-body" << endl;
  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  } else out << ind << "  empty." << endl;

  return out;
}

string CAstStatIf::dotAttr(void) const
{
  return " [label=\"if\",shape=box]";
}

void CAstStatIf::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }

  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];" 
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr* CAstStatIf::ToTac(CCodeBlock *cb, CTacLabel *next)
{
  CTacLabel *lbl_true = cb->CreateLabel("if_true");
	CTacLabel *lbl_false = cb->CreateLabel("if_false");

	CAstConstant *c = dynamic_cast<CAstConstant*>(GetCondition());

	if (c != NULL) {
		if (c->GetValue() == true) {
			cb->AddInstr(new CTacInstr(opGoto, lbl_true));
		} else {
			cb->AddInstr(new CTacInstr(opGoto, lbl_false));
		}
	} else {
		CTacAddr *eval_cond = GetCondition()->ToTac(cb, lbl_true, lbl_false);

		if (eval_cond != NULL) {
			cb->AddInstr(new CTacInstr(opEqual, lbl_true, eval_cond, new CTacConst(true)));
			cb->AddInstr(new CTacInstr(opGoto, lbl_false));
		}
	}

	cb->AddInstr(lbl_true);

  CAstStatement *if_stmt = GetIfBody();
	while (if_stmt != NULL) {
		CTacLabel *lbl_if = cb->CreateLabel();
		if_stmt->ToTac(cb, lbl_if);
		cb->AddInstr(lbl_if);
		if_stmt = if_stmt->GetNext();
	}
	cb->AddInstr(new CTacInstr(opGoto, next));

	cb->AddInstr(lbl_false);

	CAstStatement *else_stmt = GetElseBody();
	while (else_stmt != NULL) {
		CTacLabel *lbl_else = cb->CreateLabel();
		else_stmt->ToTac(cb, lbl_else);
		cb->AddInstr(lbl_else);
		else_stmt = else_stmt->GetNext();
	}
	cb->AddInstr(new CTacInstr(opGoto, next));

	return NULL;
}


//------------------------------------------------------------------------------
// CAstStatWhile
//
CAstStatWhile::CAstStatWhile(CToken t,
                             CAstExpression *cond, CAstStatement *body)
  : CAstStatement(t), _cond(cond), _body(body)
{
  assert(cond != NULL);
}

CAstExpression* CAstStatWhile::GetCondition(void) const
{
  return _cond;
}

CAstStatement* CAstStatWhile::GetBody(void) const
{
  return _body;
}

bool CAstStatWhile::TypeCheck(CToken *t, string *msg) const
{
  CAstExpression *cond = GetCondition();
	CAstStatement *body = GetBody();
	if (!cond->TypeCheck(t, msg)) return false;
	const CType *ct = cond->GetType();
	if (!ct->Match(CTypeManager::Get()->GetBool())) {
		if (t != NULL) *t = cond->GetToken();
		if (msg != NULL) *msg = "boolean expression expected.";
		return false;
	}
	if (body == NULL) return true;
	if (body->TypeCheck(t, msg)) return true;
	return false;
}

ostream& CAstStatWhile::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "while cond" << endl;
  _cond->print(out, indent+2);
  out << ind << "while-body" << endl;
  if (_body != NULL) {
    CAstStatement *s = _body;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  }
  else out << ind << "  empty." << endl;

  return out;
}

string CAstStatWhile::dotAttr(void) const
{
  return " [label=\"while\",shape=box]";
}

void CAstStatWhile::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_body != NULL) {
    CAstStatement *s = _body;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr* CAstStatWhile::ToTac(CCodeBlock *cb, CTacLabel *next)
{
	CTacLabel *lbl_cond = cb->CreateLabel("while_cond");
	CTacLabel *lbl_body = cb->CreateLabel("while_body");
	
	cb->AddInstr(lbl_cond);

	CAstConstant *c = dynamic_cast<CAstConstant*>(GetCondition());
	if (c != NULL) {
		if (c->GetValue() == false) {
			cb->AddInstr(new CTacInstr(opGoto, next));
		}
	} else {
		GetCondition()->ToTac(cb, lbl_body, next);
		cb->AddInstr(lbl_body);
	}
	
	CAstStatement *body_stmt = GetBody();
	
	while (body_stmt != NULL) {
		CTacLabel *lbl = cb->CreateLabel();
		body_stmt->ToTac(cb, lbl);
		cb->AddInstr(lbl);
		body_stmt = body_stmt->GetNext();
	}
	cb->AddInstr(new CTacInstr(opGoto, lbl_cond));

	return NULL;
}


//------------------------------------------------------------------------------
// CAstExpression
//
CAstExpression::CAstExpression(CToken t)
  : CAstNode(t)
{
}

CTacAddr* CAstExpression::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
	return NULL;
}


//------------------------------------------------------------------------------
// CAstOperation
//
CAstOperation::CAstOperation(CToken t, EOperation oper)
  : CAstExpression(t), _oper(oper)
{
}

EOperation CAstOperation::GetOperation(void) const
{
  return _oper;
}


//------------------------------------------------------------------------------
// CAstBinaryOp
//
CAstBinaryOp::CAstBinaryOp(CToken t, EOperation oper,
                           CAstExpression *l,CAstExpression *r)
  : CAstOperation(t, oper), _left(l), _right(r)
{
  // these are the only binary operation we support for now
  assert((oper == opAdd)        || (oper == opSub)         ||
         (oper == opMul)        || (oper == opDiv)         ||
         (oper == opAnd)        || (oper == opOr)          ||
         (oper == opEqual)      || (oper == opNotEqual)    ||
         (oper == opLessThan)   || (oper == opLessEqual)   ||
         (oper == opBiggerThan) || (oper == opBiggerEqual)
        );
  assert(l != NULL);
  assert(r != NULL);
}

CAstExpression* CAstBinaryOp::GetLeft(void) const
{
  return _left;
}

CAstExpression* CAstBinaryOp::GetRight(void) const
{
  return _right;
}

bool CAstBinaryOp::TypeCheck(CToken *t, string *msg) const
{
  CAstExpression *left = GetLeft();
	CAstExpression *right = GetRight();
	if (!left->TypeCheck(t, msg) || !right->TypeCheck(t, msg)) return false;
	const CType *lt = left->GetType();
	const CType *rt = right->GetType();
	EOperation oper = GetOperation();
	switch (oper) {
		case opEqual:
		case opNotEqual:
			if (!lt->Match(rt)) {
				if (t != NULL) *t = GetToken();
				if (msg != NULL) *msg = "binop: type mismatch.";
				return false;
			}
			break;
		case opAnd:
		case opOr:
			if (!lt->Match(CTypeManager::Get()->GetBool()) || !rt->Match(CTypeManager::Get()->GetBool())) {
				if (t != NULL) *t = GetToken();
				if (msg != NULL) *msg = "binop: type mismatch.";
				return false;
			}
			break;
		default:
			if (!lt->Match(CTypeManager::Get()->GetInt()) || !rt->Match(CTypeManager::Get()->GetInt())) {
				if (t != NULL) *t = GetToken();
				if (msg != NULL) *msg = "binop: type mismatch.";
				return false;
			}
			break;
	}
	return true;
}

const CType* CAstBinaryOp::GetType(void) const
{
  EOperation op = GetOperation();
	if ((op == opAdd) || (op == opSub) || (op == opMul) || (op == opDiv)) {
		return CTypeManager::Get()->GetInt();
	}
	else return CTypeManager::Get()->GetBool();
}

ostream& CAstBinaryOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  out << endl;

  _left->print(out, indent+2);
  _right->print(out, indent+2);

  return out;
}

string CAstBinaryOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstBinaryOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _left->toDot(out, indent);
  out << ind << dotID() << "->" << _left->dotID() << ";" << endl;
  _right->toDot(out, indent);
  out << ind << dotID() << "->" << _right->dotID() << ";" << endl;
}

CTacAddr* CAstBinaryOp::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
  EOperation op = GetOperation();
	CTacAddr *eval_left = NULL, *eval_right = NULL;
	CTacTemp *tmp_val = NULL;

	if (op == opAdd || op == opSub || op == opMul || op == opDiv) {
		eval_left = GetLeft()->ToTac(cb, NULL, NULL);
		eval_right = GetRight()->ToTac(cb, NULL, NULL);
		
		tmp_val = cb->CreateTemp(CTypeManager::Get()->GetInt());
		
		cb->AddInstr(new CTacInstr(op, tmp_val, eval_left, eval_right));
	} else if (op == opAnd) {
		CTacLabel *lbl_true = ltrue;
		CTacLabel *lbl_false = lfalse;
		CTacLabel *lbl_test = cb->CreateLabel();
		CTacLabel *lbl_end;

		if (ltrue == NULL && lfalse == NULL) {
			lbl_true = cb->CreateLabel();
			lbl_false = cb->CreateLabel();
			lbl_end = cb->CreateLabel();
		}

		CAstConstant *l = dynamic_cast<CAstConstant*>(GetLeft());
		CAstConstant *r = dynamic_cast<CAstConstant*>(GetRight());
		// if l and r are constants
		if (l && r) {
			if (l->GetValue() == true && r->GetValue() == true) {
				cb->AddInstr(new CTacInstr(opGoto, lbl_true));
			} else {
				cb->AddInstr(new CTacInstr(opGoto, lbl_false));
			}
		}
		// if l is constant and r isn't constant
		else if (l) {
			if (l->GetValue() == true) {
				eval_right = GetRight()->ToTac(cb, lbl_true, lbl_false);
				if (eval_right != NULL) {
					cb->AddInstr(new CTacInstr(opEqual, lbl_true, eval_right, new CTacConst(true)));
					cb->AddInstr(new CTacInstr(opGoto, lbl_false));
				}
			}
			else {
				cb->AddInstr(new CTacInstr(opGoto, lbl_false));
			}
		}
		// if r is constant and l isn't constant
		else if (r) {
			if (r->GetValue() == true) {
				eval_left = GetLeft()->ToTac(cb, lbl_true, lbl_false);
				if (eval_left != NULL) {
					cb->AddInstr(new CTacInstr(opEqual, lbl_true, eval_left, new CTacConst(true)));
					cb->AddInstr(new CTacInstr(opGoto, lbl_false));
				}
			}
			else {
				cb->AddInstr(new CTacInstr(opGoto, lbl_false));
			}
		}
		// if l and r are not constants
		else {
			eval_left = GetLeft()->ToTac(cb, lbl_test, lbl_false);
			if (eval_left != NULL) {
				cb->AddInstr(new CTacInstr(opEqual, lbl_test, eval_left, new CTacConst(true)));
				cb->AddInstr(new CTacInstr(opGoto, lbl_false));
			}

			cb->AddInstr(lbl_test);
		
			eval_right = GetRight()->ToTac(cb, lbl_true, lbl_false);
			if (eval_right != NULL) {
				cb->AddInstr(new CTacInstr(opEqual, lbl_true, eval_right, new CTacConst(true)));
				cb->AddInstr(new CTacInstr(opGoto, lbl_false));
			}
		}

		if (ltrue == NULL && lfalse == NULL) {	
			tmp_val = cb->CreateTemp(CTypeManager::Get()->GetBool());
			
			cb->AddInstr(lbl_true);
			cb->AddInstr(new CTacInstr(opAssign, tmp_val, new CTacConst(true)));
			cb->AddInstr(new CTacInstr(opGoto, lbl_end));
			
			cb->AddInstr(lbl_false);
			cb->AddInstr(new CTacInstr(opAssign, tmp_val, new CTacConst(false)));
			cb->AddInstr(new CTacInstr(opGoto, lbl_end));
		
			cb->AddInstr(lbl_end);
		}
	} else if (op == opOr) {
		CTacLabel *lbl_true = ltrue;
		CTacLabel *lbl_false = lfalse;
		CTacLabel *lbl_test = cb->CreateLabel();
		CTacLabel *lbl_end;

		if (ltrue == NULL && lfalse == NULL) {
			lbl_true = cb->CreateLabel();
			lbl_false = cb->CreateLabel();
			lbl_end = cb->CreateLabel();
		}

		CAstConstant *l = dynamic_cast<CAstConstant*>(GetLeft());
		CAstConstant *r = dynamic_cast<CAstConstant*>(GetRight());
		
		// if l and r are constants
		if (l && r) {
			if (l->GetValue() == false && r->GetValue() == false) {
				cb->AddInstr(new CTacInstr(opGoto, lbl_false));
			} else {
				cb->AddInstr(new CTacInstr(opGoto, lbl_true));
			}
		}
		// if l is constant and r isn't constant
		else if (l) {
			if (l->GetValue() == false) {
				eval_right = GetRight()->ToTac(cb, lbl_true, lbl_false);
				if (eval_right != NULL) {
					cb->AddInstr(new CTacInstr(opEqual, lbl_true, eval_right, new CTacConst(true)));
					cb->AddInstr(new CTacInstr(opGoto, lbl_false));
				}
			}
			else {
				cb->AddInstr(new CTacInstr(opGoto, lbl_true));
			}
		}
		// if r is constant and l isn't constant
		else if (r) {
			if (r->GetValue() == false) {
				eval_left = GetLeft()->ToTac(cb, lbl_true, lbl_false);
				if (eval_left != NULL) {
					cb->AddInstr(new CTacInstr(opEqual, lbl_true, eval_left, new CTacConst(true)));
					cb->AddInstr(new CTacInstr(opGoto, lbl_false));
				}
			}
			else {
				cb->AddInstr(new CTacInstr(opGoto, lbl_true));
			}
		}
		// if l and r are not constants
		else {
			eval_left = GetLeft()->ToTac(cb, lbl_true, lbl_test);
			if (eval_left != NULL) {
				cb->AddInstr(new CTacInstr(opEqual, lbl_true, eval_left, new CTacConst(true)));
				cb->AddInstr(new CTacInstr(opGoto, lbl_test));
			}

			cb->AddInstr(lbl_test);
			
			eval_right = GetRight()->ToTac(cb, lbl_true, lbl_false);
			if (eval_right != NULL) {
				cb->AddInstr(new CTacInstr(opEqual, lbl_true, eval_right, new CTacConst(true)));
				cb->AddInstr(new CTacInstr(opGoto, lbl_false));
			}
		}

		if (ltrue == NULL && lfalse == NULL) {
			
			tmp_val = cb->CreateTemp(CTypeManager::Get()->GetBool());
		
			cb->AddInstr(lbl_true);
			cb->AddInstr(new CTacInstr(opAssign, tmp_val, new CTacConst(true)));
			cb->AddInstr(new CTacInstr(opGoto, lbl_end));
		
			cb->AddInstr(lbl_false);
			cb->AddInstr(new CTacInstr(opAssign, tmp_val, new CTacConst(false)));
			cb->AddInstr(new CTacInstr(opGoto, lbl_end));
		
			cb->AddInstr(lbl_end);
		}
	} else {
		CTacLabel *lbl_true = ltrue;
		CTacLabel *lbl_false = lfalse;
		CTacLabel *lbl_end;
		
		if (ltrue == NULL && lfalse == NULL) {
			lbl_true = cb->CreateLabel();
			lbl_false = cb->CreateLabel();
			lbl_end = cb->CreateLabel();
		}
		
		eval_left = GetLeft()->ToTac(cb, NULL, NULL);
		eval_right = GetRight()->ToTac(cb, NULL, NULL);

		cb->AddInstr(new CTacInstr(op, lbl_true, eval_left, eval_right));
		cb->AddInstr(new CTacInstr(opGoto, lbl_false));

		if (ltrue == NULL && lfalse == NULL) {
			tmp_val = cb->CreateTemp(CTypeManager::Get()->GetBool());

			cb->AddInstr(lbl_true);
			cb->AddInstr(new CTacInstr(opAssign, tmp_val, new CTacConst(true)));
			cb->AddInstr(new CTacInstr(opGoto, lbl_end));

			cb->AddInstr(lbl_false);
			cb->AddInstr(new CTacInstr(opAssign, tmp_val, new CTacConst(false)));
			cb->AddInstr(new CTacInstr(opGoto, lbl_end));

			cb->AddInstr(lbl_end);
		}
	}

	return tmp_val;
}


//------------------------------------------------------------------------------
// CAstUnaryOp
//
CAstUnaryOp::CAstUnaryOp(CToken t, EOperation oper, CAstExpression *e)
  : CAstOperation(t, oper), _operand(e)
{
  assert((oper == opNeg) || (oper == opNot) || (oper == opPos));
  assert(e != NULL);
}

CAstExpression* CAstUnaryOp::GetOperand(void) const
{
  return _operand;
}

bool CAstUnaryOp::TypeCheck(CToken *t, string *msg) const
{
  CAstExpression *operand = GetOperand();
	if (!operand->TypeCheck(t, msg)) return false;
	const CType *ty = operand->GetType();
	EOperation oper = GetOperation();
	switch (oper) {
		case opNot:
			if (!ty->Match(CTypeManager::Get()->GetBool())) {
				if (t != NULL) *t = GetToken();
				if (msg != NULL) *msg = "unaryop: type mismatch.";
				return false;
			}
			break;
		case opNeg:
		case opPos:
			if (!ty->Match(CTypeManager::Get()->GetInt())) {
				if (t != NULL) *t = GetToken();
				if (msg != NULL) *msg = "unaryop: type mismatch.";
				return false;
			}
			break;
	}
	return true;
}

const CType* CAstUnaryOp::GetType(void) const
{
  EOperation op = GetOperation();
	if (op == opNot) return CTypeManager::Get()->GetBool();
	else return CTypeManager::Get()->GetInt();
}

ostream& CAstUnaryOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  out << endl;

  _operand->print(out, indent+2);

  return out;
}

string CAstUnaryOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstUnaryOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _operand->toDot(out, indent);
  out << ind << dotID() << "->" << _operand->dotID() << ";" << endl;
}

CTacAddr* CAstUnaryOp::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
	EOperation op = GetOperation();
	CTacAddr *eval = NULL;
	CTacTemp *tmp_val = NULL;

	if (op == opPos || op == opNeg) {
		CAstConstant *operand = dynamic_cast<CAstConstant*>(GetOperand());
		
		if (operand) {
			if (op == opPos) {
				return new CTacConst(operand->GetValue());
			} else {
				return new CTacConst(-1 * operand->GetValue());
			}
		}

		eval = GetOperand()->ToTac(cb, NULL, NULL);

		tmp_val = cb->CreateTemp(CTypeManager::Get()->GetInt());

		cb->AddInstr(new CTacInstr(op, tmp_val, eval, NULL));
	} else {
		CTacLabel *lbl_true = ltrue;
		CTacLabel *lbl_false = lfalse;
		CTacLabel *lbl_end;

		if (ltrue == NULL && lfalse == NULL) {
			lbl_true = cb->CreateLabel();
			lbl_false = cb->CreateLabel();
			lbl_end = cb->CreateLabel();
		}

		CAstConstant *operand = dynamic_cast<CAstConstant*>(GetOperand());

		if (operand) {
			if (operand->GetValue() == true) {
				cb->AddInstr(new CTacInstr(opGoto, lbl_false));
			} else {
				cb->AddInstr(new CTacInstr(opGoto, lbl_true));
			}
		} else {
			eval = GetOperand()->ToTac(cb, lbl_false, lbl_true);
			if (eval != NULL) {
				cb->AddInstr(new CTacInstr(opEqual, lbl_false, eval, new CTacConst(true)));
				cb->AddInstr(new CTacInstr(opGoto, lbl_true));
			}
		}

		if (ltrue == NULL && lfalse == NULL) {
			tmp_val = cb->CreateTemp(CTypeManager::Get()->GetBool());

			cb->AddInstr(lbl_true);
			cb->AddInstr(new CTacInstr(opAssign, tmp_val, new CTacConst(true)));
			cb->AddInstr(new CTacInstr(opGoto, lbl_end));

			cb->AddInstr(lbl_false);
			cb->AddInstr(new CTacInstr(opAssign, tmp_val, new CTacConst(false)));
			
			cb->AddInstr(lbl_end);
		}
	}

	return tmp_val;
}


//------------------------------------------------------------------------------
// CAstFunctionCall
//
CAstFunctionCall::CAstFunctionCall(CToken t, const CSymProc *symbol)
  : CAstExpression(t), _symbol(symbol)
{
  assert(symbol != NULL);
}

const CSymProc* CAstFunctionCall::GetSymbol(void) const
{
  return _symbol;
}

void CAstFunctionCall::AddArg(CAstExpression *arg)
{
  _arg.push_back(arg);
}

int CAstFunctionCall::GetNArgs(void) const
{
  return (int)_arg.size();
}

CAstExpression* CAstFunctionCall::GetArg(int index) const
{
  assert((index >= 0) && (index < _arg.size()));
  return _arg[index];
}

bool CAstFunctionCall::TypeCheck(CToken *t, string *msg) const
{
  const CSymProc *sb = GetSymbol();
  int nargs = GetNArgs();
	if (nargs > sb->GetNParams()) {
		if (t != NULL) *t = GetToken();
		if (msg != NULL) *msg = "too many arguments.";
		return false;
	}
	if (nargs < sb->GetNParams()) {
		if (t != NULL) *t = GetToken();
		if (msg != NULL) *msg = "not enough arguments.";
		return false;
	}
	for (int i = 0; i < nargs; i++) {
		CAstExpression *argi = GetArg(i);
		const CSymParam *parami = sb->GetParam(i);
		if (!argi->TypeCheck(t, msg)) return false;
		if (!argi->GetType()->Match(parami->GetDataType())) {
			if (t != NULL) *t = argi->GetToken();
			if (msg != NULL) *msg = "argument type mismatch.";
			return false;
		}
	}
	return true;
}

const CType* CAstFunctionCall::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

ostream& CAstFunctionCall::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "call " << _symbol << " ";
  
  out << endl;

  for (size_t i=0; i<_arg.size(); i++) {
    _arg[i]->print(out, indent+2);
  }

  return out;
}

string CAstFunctionCall::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"call " << _symbol->GetName() << "\",shape=box]";
  return out.str();
}

void CAstFunctionCall::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  for (size_t i=0; i<_arg.size(); i++) {
    _arg[i]->toDot(out, indent);
    out << ind << dotID() << "->" << _arg[i]->dotID() << ";" << endl;
  }
}

CTacAddr* CAstFunctionCall::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
	CTacTemp *tmp_val = NULL;
	int nargs = GetNArgs();

	for (int i = nargs - 1; i >= 0; i--) {
		cb->AddInstr(new CTacInstr(opParam, new CTacConst(i), GetArg(i)->ToTac(cb, NULL, NULL)));
	}

	if (GetType()->IsBoolean() || GetType()->IsScalar()) {
		tmp_val = cb->CreateTemp(GetType());
	}

	cb->AddInstr(new CTacInstr(opCall, tmp_val, new CTacName(GetSymbol())));

	return tmp_val;
}


//------------------------------------------------------------------------------
// CAstOperand
//
CAstOperand::CAstOperand(CToken t)
  : CAstExpression(t)
{
}


//------------------------------------------------------------------------------
// CAstDesignator
//
CAstDesignator::CAstDesignator(CToken t, const CSymbol *symbol,
                               CAstExpression *offset)
  : CAstOperand(t), _symbol(symbol), _offset(offset)
{
  assert(symbol != NULL);
}

const CSymbol* CAstDesignator::GetSymbol(void) const
{
  return _symbol;
}

bool CAstDesignator::TypeCheck(CToken *t, string *msg) const
{
  return true; 
}

const CType* CAstDesignator::GetType(void) const
{
  const CType *t = GetSymbol()->GetDataType();

  if (_offset != NULL) {
    if (t->IsArray() && (_offset->GetType()->IsScalar())) {
      const CArrayType *at = dynamic_cast<const CArrayType*>(t);
      assert(at != NULL);
      t = at->GetBaseType();
    } else {
      t = NULL;
    }
  }

  return t;
}

ostream& CAstDesignator::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << _symbol << " ";

  out << endl;

  if (_offset != NULL) _offset->print(out, indent+2);

  return out;
}

string CAstDesignator::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << _symbol->GetName();
  if (_offset != NULL) out << "[]";
  out << "\",shape=ellipse]";
  return out.str();
}

void CAstDesignator::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  if (_offset != NULL) {
    _offset->toDot(out, indent);
    out << ind << dotID() << "->" << _offset->dotID() << ";" << endl;
  }
}

CTacAddr* CAstDesignator::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
  return new CTacName(GetSymbol());
}


//------------------------------------------------------------------------------
// CAstConstant
//
CAstConstant::CAstConstant(CToken t, const CType *type, long long value)
  : CAstOperand(t), _type(type), _value(value)
{
}

void CAstConstant::SetValue(long long value)
{
  _value = value;
}

long long CAstConstant::GetValue(void) const
{
  return _value;
}

string CAstConstant::GetValueStr(void) const
{
  ostringstream out;

  if (GetType() == CTypeManager::Get()->GetBool()) {
    out << (_value == 0 ? "false" : "true");
  } else {
    out << dec << _value;
  }

  return out.str();
}

bool CAstConstant::TypeCheck(CToken *t, string *msg) const
{
  const CType *ty = GetType();
	if (ty->Match(CTypeManager::Get()->GetBool())) return true;
	else {
		if (GetValue() > 2147483648) {
			if (t != NULL) *t = GetToken();
			if (msg != NULL) *msg = "integer constant outside valid range.";
			return false;
		}
	}
	return true;
}

const CType* CAstConstant::GetType(void) const
{
  return _type;
}

ostream& CAstConstant::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetValueStr() << " ";

  out << endl;

  return out;
}

string CAstConstant::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetValueStr() << "\",shape=ellipse]";
  return out.str();
}

CTacAddr* CAstConstant::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
  return new CTacConst(GetValue());
}

