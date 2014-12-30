//------------------------------------------------------------------------------
/// @brief SnuPL/0 parser
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/09/28 Bernhard Egger assignment 2: parser for SnuPL/-1
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

#include <limits.h>
#include <cassert>
#include <vector>
#include <iostream>
#include <exception>

#include "parser.h"
using namespace std;


//------------------------------------------------------------------------------
// CParser
//
CParser::CParser(CScanner *scanner)
{
  _scanner = scanner;
  _module = NULL;
}

CAstNode* CParser::Parse(void)
{
  _abort = false;

  if (_module != NULL) { delete _module; _module = NULL; }

  try {
    if (_scanner != NULL) _module = module();

    if (_module != NULL) {
      CToken t;
      string msg;
      if (!_module->TypeCheck(&t, &msg)) SetError(t, msg);
    }
  } catch (...) {
    _module = NULL;
  }

  return _module;
}

const CToken* CParser::GetErrorToken(void) const
{
  if (_abort) return &_error_token;
  else return NULL;
}

string CParser::GetErrorMessage(void) const
{
  if (_abort) return _message;
  else return "";
}

void CParser::SetError(CToken t, const string message)
{
  _error_token = t;
  _message = message;
  _abort = true;
  throw message;
}

bool CParser::Consume(EToken type, CToken *token)
{
  if (_abort) return false;

  CToken t = _scanner->Get();

  if (t.GetType() != type) {
    SetError(t, "expected '" + CToken::Name(type) + "', got '" +
             t.GetName() + "'");
  }

  if (token != NULL) *token = t;

  return t.GetType() == type;
}

void CParser::InitSymbolTable(CSymtab *s)
{
  CTypeManager *tm = CTypeManager::Get();

  CSymProc *input = new CSymProc("Input", tm->GetInt());

  CSymProc *output = new CSymProc("Output", tm->GetNull());
  output->AddParam(new CSymParam(0, "x", tm->GetInt()));

  s->AddSymbol(input);
  s->AddSymbol(output);
}

CAstModule* CParser::module(void)
{
  //
  // module = "module" ident ";" varDeclaration { subroutineDecl } "begin" statSequence "end" ident ".".
  //
  
  CToken t;
  CAstModule *m;
  CAstStatement *statseq = NULL;
  string moduleName;

  Consume(tModule, &t);
  Consume(tIdent, &t);

  moduleName = t.GetValue();
  m = new CAstModule(t, moduleName);
  InitSymbolTable(m->GetSymbolTable());
  
  Consume(tSemicolon, &t);
  varDeclaration(m);

  while (_scanner->Peek().GetType() == tProcedure || _scanner->Peek().GetType() == tFunction) {
    subroutineDecl(m);
  }

  Consume(tBegin, &t);
  statseq = statSequence(m);
  Consume(tEnd, &t);
  Consume(tIdent, &t);
  if (t.GetValue() != moduleName) {
    SetError(t, "module identifier mismatch ('" + moduleName + "' != '" + t.GetValue() + "').");
  }

  Consume(tDot);
  m->SetStatementSequence(statseq);

  return m;
}

CAstDesignator* CParser::ident(CAstScope *s)
{
  //
  // ident = letter { letter | digit }.
  //
  // "letter { letter | digit }" is scanned as one token (tIdent)
  //
  
  CToken t;

  Consume(tIdent, &t);

  CSymtab *st = s->GetSymbolTable();
  const CSymbol *symbol = st->FindSymbol(t.GetValue());

  return new CAstDesignator(t, symbol);
}

CAstConstant* CParser::number(void)
{
  //
  // number = digit { digit }.
  //
  // "digit { digit }" is scanned as one token (tNumber)
  //
 
  CToken t;
 
  Consume(tNumber, &t);
 
  errno = 0;
  long long v = strtoll(t.GetValue().c_str(), NULL, 10);
  if (errno != 0) SetError(t, "invalid number.");
 
  return new CAstConstant(t, CTypeManager::Get()->GetInt(), v);
}

CAstConstant* CParser::boolean(void)
{
  //
  // boolean = "true" | "false".
  //

  CToken t;
  bool v;

  Consume(tBoolConst, &t);
  v = (t.GetValue() == "true");

  return new CAstConstant(t, CTypeManager::Get()->GetBool(), v);
}

CAstType* CParser::type(void)
{
  //
  // type = "integer" | "boolean".
  //
  // "integer" and "boolean" are scanned as one token (tInteger, tBoolean)
  //

  CToken t;

  if (_scanner->Peek().GetType() == tInteger) {
    Consume(tInteger, &t);
    return new CAstType(t, CTypeManager::Get()->GetInt());
  }
  else {
    Consume(tBoolean, &t);
    return new CAstType(t, CTypeManager::Get()->GetBool());
  }
}

CAstExpression* CParser::factor(CAstScope *s)
{
  //
  // factor = ident | number | boolean | "(" expression ")" | subroutineCall | "!" factor.
  //
  // FIRST(factor) = { tNumber, tIdent, tBoolConst, tLBrak, tNot }
  //
  
  CToken t;
  CAstExpression *n = NULL;
  CSymtab *st = s->GetSymbolTable();
  const CSymbol *symbol = NULL;

  switch (_scanner->Peek().GetType()) {
    // factor ::= ident | subroutineCall
    case tIdent:
      t = _scanner->Peek();
      symbol = st->FindSymbol(t.GetValue());
      if (symbol == NULL) {
        SetError(t, "undefined identifier '" + t.GetValue() + "'.");
      } else if (symbol->GetSymbolType() == stProcedure) {
        n = subroutineCall(s)->GetCall();
      } else {
        n = ident(s);
      }
      break;

    // factor ::= number
    case tNumber:
      n = number();
      break;

    // factor ::= boolean
    case tBoolConst:
      n = boolean();
      break;

    // factor ::= "(" expression ")"
    case tLBrak:
      Consume(tLBrak);
      n = expression(s);
      Consume(tRBrak);
      break;

    // factor ::= "!" factor
    case tNot:
      Consume(tNot, &t);
      n = new CAstUnaryOp(t, opNot, factor(s));
      break;

    default:
      SetError(_scanner->Peek(), "factor expected.");
      break;
  }

  return n;
}

CAstExpression* CParser::term(CAstScope *s)
{
  //
  // term = factor { factOp factor }.
  //
 
  CAstExpression *n = NULL;
 
  n = factor(s);
 
  while (_scanner->Peek().GetType() == tMulDiv || _scanner->Peek().GetType() == tAnd) {
    CToken t;
    CAstExpression *l = n, *r;

    if (_scanner->Peek().GetType() == tMulDiv) {
      Consume(tMulDiv, &t);
      r = factor(s);
      n = new CAstBinaryOp(t, t.GetValue() == "*" ? opMul : opDiv, l, r);
    } else {
      Consume(tAnd, &t);
      r = factor(s);
      n = new CAstBinaryOp(t, opAnd, l, r);
    }
  }

  return n;
}

CAstExpression* CParser::simpleexpr(CAstScope *s)
{
  //
  // simpleexpr = [ "+" | "-" ] term { termOp term }.
  //

  CToken t;
  CAstExpression *n = NULL;
  CAstConstant *num = NULL;
	long long v;

  if (_scanner->Peek().GetType() == tPlusMinus) {
    Consume(tPlusMinus, &t);
    n = term(s);
		CAstConstant *c = dynamic_cast<CAstConstant*>(n);
		
		if (t.GetValue() == "-") {
			if (c && c->GetType()->Match(CTypeManager::Get()->GetInt())) {
				v = c->GetValue();
				if (v > 2147483648) {
					SetError(t, "integer constant outside valid range.");
				}
			}
			n = new CAstUnaryOp(t, opNeg, n);
		} else {
			if (c && c->GetType()->Match(CTypeManager::Get()->GetInt())) {
				v = c->GetValue();
				if (v >= 2147483648) {
					SetError(t, "integer constant outside valid range.");
				}
			}
			n = new CAstUnaryOp(t, opPos, n);
		}
	} else {
		n = term(s);
		CAstConstant *c = dynamic_cast<CAstConstant*>(n);

		if (c && c->GetType()->Match(CTypeManager::Get()->GetInt())) {
			v = c->GetValue();
			if (v >= 2147483648) {
				SetError(n->GetToken(), "integer constant outside valid range.");
			}
		}
	}
 
  while (_scanner->Peek().GetType() == tPlusMinus || _scanner->Peek().GetType() == tOr) {
    CAstExpression *l = n, *r;

    if (_scanner->Peek().GetType() == tPlusMinus)
    {
      Consume(tPlusMinus, &t);
      r = term(s);
			CAstConstant *c = dynamic_cast<CAstConstant*>(r);
			long long v;

			if (c && c->GetType()->Match(CTypeManager::Get()->GetInt())) {
				v = c->GetValue();
				if (t.GetValue() == "+" && v >= 2147483648) {
					SetError(t, "integer constant outside valid range.");
				}
				if (t.GetValue() == "-" && v > 2147483648) {
					SetError(t, "integer constant outside valud range.");
				}
			}
			n = new CAstBinaryOp(t, t.GetValue() == "+" ? opAdd : opSub, l, r);
    } else {
      Consume(tOr, &t);
      r = term(s);

      n = new CAstBinaryOp(t, opOr, l, r);
    }
  }

  return n;
}

CAstExpression* CParser::expression(CAstScope* s)
{
  //
  // expression = simpleexpr [ relOp simpleexpression ].
  //
 
  CToken t;
  CAstExpression *left = NULL, *right = NULL;
  EOperation relop;
  string op;

  left = simpleexpr(s);

  if (_scanner->Peek().GetType() == tRelOp) {
    Consume(tRelOp, &t);
    right = simpleexpr(s);
    op = t.GetValue();

    if (op == "=")        relop = opEqual;
    else if (op == "#")   relop = opNotEqual;
    else if (op == "<")   relop = opLessThan;
    else if (op == "<=")  relop = opLessEqual;
    else if (op == ">")   relop = opBiggerThan;
    else if (op == ">=")  relop = opBiggerEqual;
    else SetError(t, "invalid relation.");

    return new CAstBinaryOp(t, relop, left, right);
  } else {
    return left;
  }
}

CAstStatAssign* CParser::assignment(CAstScope *s)
{
  //
  // assignment = ident := expression.
  //

  CToken t;

  CAstDesignator *lhs = ident(s);

  Consume(tAssign, &t);

  CAstExpression *rhs = expression(s);

  return new CAstStatAssign(t, lhs, rhs);
}

CAstStatCall* CParser::subroutineCall(CAstScope *s)
{
  //
  // subroutineCall = ident "(" [ expression { "," expression } ] ")".
  //
  // FIRST(expression) = { tNot, tPlusMinus, tLBrak, tIdent, tNumber, tBoolConst }
  //

  CToken t;
  EToken typ;
  CAstDesignator *id = NULL;
  CAstFunctionCall *fc = NULL;
  CAstExpression *arg = NULL;
  CSymProc *spr = NULL;

  id = ident(s);
  spr = (CSymProc *)id->GetSymbol();
  fc = new CAstFunctionCall(id->GetToken(), spr);

  Consume(tLBrak, &t);
  t = _scanner->Peek();
  typ = t.GetType();

  if (typ == tNot || typ == tPlusMinus || typ == tLBrak || typ == tIdent || typ == tNumber || typ == tBoolConst) {
    arg = expression(s);
    fc->AddArg(arg);

    while(_scanner->Peek().GetType() == tComma) {
      Consume(tComma, &t);
      arg = expression(s);
      fc->AddArg(arg);
    }
  }

  Consume(tRBrak, &t);

  return new CAstStatCall(t, fc);
}

CAstStatIf* CParser::ifStatement(CAstScope *s)
{
  //
  // ifStatement = "if" "(" expression ")" "then" statSequence [ "else" statSequence ] "end".
  //

  CToken t;
  CAstExpression *cond = NULL;
  CAstStatement *ifBody = NULL;
  CAstStatement *elseBody = NULL;

  Consume(tIf, &t);
  Consume(tLBrak, &t);
  cond = expression(s);
  Consume(tRBrak, &t);
  Consume(tThen, &t);
  ifBody = statSequence(s);

  if (_scanner->Peek().GetType() == tElse) {
    Consume(tElse, &t);
    elseBody = statSequence(s);
  }

  Consume(tEnd, &t);

  return new CAstStatIf(t, cond, ifBody, elseBody);
}
    
CAstStatWhile* CParser::whileStatement(CAstScope *s)
{
  //
  // whileStatement = "while" "(" expression ")" "do" statSequence "end".
  //

  CToken t;
  CAstExpression *cond = NULL;
  CAstStatement *body = NULL;

  Consume(tWhile, &t);
  Consume(tLBrak, &t);
  cond = expression(s);
  Consume(tRBrak, &t);
  Consume(tDo, &t);
  body = statSequence(s);
  Consume(tEnd, &t);

  return new CAstStatWhile(t, cond, body);
}

CAstStatReturn* CParser::returnStatement(CAstScope *s)
{
  //
  // returnStatement = "return" [ expression ].
  //
  // FIRST(expression) = { tNot, tPlusMinus, tLBrak, tIdent, tNumber, tBoolConst }
  //

  CToken t;
  CAstExpression *expr = NULL;
  EToken typ;

  Consume(tReturn, &t);
	t = _scanner->Peek();
  typ = t.GetType();

  if (typ == tNot || typ == tPlusMinus || typ == tLBrak || typ == tIdent || typ == tNumber || typ == tBoolConst) {
    expr = expression(s);
  }

  return new CAstStatReturn(t, s, expr);
}

CAstStatement* CParser::statement(CAstScope *s)
{
  //
  // statement = assignment | subroutineCall | ifStatement | whileStatement | returnStatement.
  //
  // FIRST(assignment) = FIRST(subroutineCall) = { tIdent }
  // FIRST(ifStatement) = { tIf }
  // FIRST(whileStatement) = { tWhile }
  // FIRST(returnStatement) = { tReturn }
  //

  CToken t;
  CSymtab *st = s->GetSymbolTable();
  CAstStatement *stm = NULL;
  t = _scanner->Peek();

  switch(t.GetType())
  {
    case tIf:
      stm = ifStatement(s);
      break;

    case tWhile:
      stm = whileStatement(s);
      break;

    case tReturn:
      stm = returnStatement(s);
      break;

    case tIdent:
      if (st->FindSymbol(t.GetValue())->GetSymbolType() == stProcedure) {
        stm = subroutineCall(s);
      } else {
        stm = assignment(s);
      }
      break;

    default:
      SetError(t, "statement expected.");
      break;
  }

  return stm;
}

CAstStatement* CParser::statSequence(CAstScope *s)
{
  //
  // statSequence = [ statement { ";" statement } ].
  // FOLLOW(statSequence) = { tElse, tEnd }
  //

  CToken t;
  CAstStatement *head = NULL, *tail = NULL, *st = NULL;

  if (_scanner->Peek().GetType() != tElse && _scanner->Peek().GetType() != tEnd) {
    head = statement(s);
    tail = head;
    
    while (_scanner->Peek().GetType() == tSemicolon) {
      Consume(tSemicolon, &t);
      st = statement(s);
      tail->SetNext(st);
      tail = st;
    }
  }

  return head;
}

void CParser::varDeclaration(CAstScope *s)
{
  //
  // varDeclaration = [ "var" { ident { "," ident } ":" type ";" } ].
  //

  CToken t;
  CAstType *typ = NULL;
  CSymtab *st = s->GetSymbolTable();
  CSymbol *sb = NULL;

  if (_scanner->Peek().GetType() == tVarDecl) {
    Consume(tVarDecl, &t);
    
    while (_scanner->Peek().GetType() == tIdent) {
			std::vector<string> tmpid;
			std::vector<CToken> tmptkn;
			Consume(tIdent, &t);

      if (st->FindSymbol(t.GetValue(), sLocal)) {
        SetError(t, "duplicate variable declaration '" + t.GetValue() + "'.");
      }

      tmpid.push_back(t.GetValue());
			tmptkn.push_back(t);

      while (_scanner->Peek().GetType() == tComma) {
        Consume(tComma, &t);
        Consume(tIdent, &t);

        if (st -> FindSymbol(t.GetValue(), sLocal)) {
          SetError(t, "duplicate variable declaration '" + t.GetValue() + "'.");
        }

        tmpid.push_back(t.GetValue());
				tmptkn.push_back(t);
      }

      Consume(tColon, &t);
      typ = type();
      
      for (int i = 0; i < tmpid.size(); i++) {
				for (int j = 0; j < i; j++) {
					if (tmpid[i] == tmpid[j]) {
						SetError(tmptkn[i], "duplicate variable declaration '" + tmpid[i] + "'.");
					}
				}
        sb = s->CreateVar(tmpid[i], typ->GetType());
        st->AddSymbol(sb);
      }
      
      Consume(tSemicolon, &t);
    }
  }
}

CAstProcedure* CParser::subroutineDecl(CAstScope *s)
{
  //
  // subroutineDecl = ( procedureDecl | functionDecl ) subroutineBody ident ";".
  // procedureDecl = "procedure" ident [ formalParam ] ";".
  // functionDecl = "function" ident [ formalParam ] ":" type ";".
  // formalParam = "(" [ ident { "," ident } ] ")".
  // subroutineBody = varDeclaration "begin" statSequence "end".
  //

  CToken t;
  CSymParam *spa = NULL;
  CSymProc *spr = NULL;
  CSymtab *st = s->GetSymbolTable();
  CAstType *fntyp = NULL;
  CAstStatement *stseq = NULL;
  CAstProcedure *proc = NULL;
  string pfn;
  std::vector<CSymParam *> tmp;
  int idx = 0;
  bool isProc = false;

  if (_scanner->Peek().GetType() == tProcedure) {
    Consume(tProcedure, &t);
    isProc = true;
  } else {
    Consume(tFunction, &t);
    isProc = false;
  }

  Consume(tIdent, &t);
  pfn = t.GetValue();

  // formalParam
  if (_scanner->Peek().GetType() == tLBrak) {
    Consume(tLBrak, &t);
    
    if (_scanner->Peek().GetType() == tIdent) {
      Consume(tIdent, &t);
    
      spa = new CSymParam(idx++, t.GetValue(), CTypeManager::Get()->GetInt());
      tmp.push_back(spa);

      while (_scanner->Peek().GetType() == tComma) {
        Consume(tComma, &t);
        Consume(tIdent, &t);

        for (int i = 0; i < tmp.size(); i++) {
          if (tmp[i]->GetName() == t.GetValue()) {
            SetError(t, "duplicate parameter declaration '" + t.GetValue() + "'.");
          }
        }

        spa = new CSymParam(idx++, t.GetValue(), CTypeManager::Get()->GetInt());
        tmp.push_back(spa);
      }
    }

    Consume(tRBrak, &t);
  }

  if (isProc) {
    Consume(tSemicolon, &t);
    spr = new CSymProc(pfn, CTypeManager::Get()->GetNull());
  } else {
    Consume(tColon, &t);
    fntyp = type();
    spr = new CSymProc(pfn, fntyp->GetType()); // just return null type
    Consume(tSemicolon, &t);
  }

  for (int i = 0; i < idx; i++) {
    spr->AddParam(tmp[i]);
  }

  st->AddSymbol(spr);

  proc = new CAstProcedure(t, pfn, s, spr);

  st = proc->GetSymbolTable();
  for (int i = 0; i < idx; i++) {
    st->AddSymbol(tmp[i]);
  }
  
  varDeclaration(proc);

  Consume(tBegin, &t);
  stseq = statSequence(proc);
  Consume(tEnd, &t);

  Consume(tIdent, &t);
  if (t.GetValue() != pfn) {
    SetError(t, "procedure/function identifier mismatch ('" + pfn + "' != '" + t.GetValue() + "').");
  }
  
  Consume(tSemicolon, &t);

  proc->SetStatementSequence(stseq);

  return proc;
}
