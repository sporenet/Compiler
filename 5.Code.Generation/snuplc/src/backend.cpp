//------------------------------------------------------------------------------
/// @brief SnuPL backend
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/11/28 Bernhard Egger created
/// 2013/06/09 Bernhard Egger adapted to SnuPL/0
///
/// @section license_section License
/// Copyright (c) 2012-2014 Bernhard Egger
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

#include <fstream>
#include <sstream>
#include <iomanip>
#include <cassert>

#include "backend.h"
using namespace std;


//------------------------------------------------------------------------------
// CBackend
//
CBackend::CBackend(ostream &out)
  : _out(out)
{
}

CBackend::~CBackend(void)
{
}

bool CBackend::Emit(CModule *m)
{
  assert(m != NULL);
  _m = m;

  if (!_out.good()) return false;

  bool res = true;

  try {
    EmitHeader();
    EmitCode();
    EmitData();
    EmitFooter();

    res = _out.good();
  } catch (...) {
    res = false;
  }

  return res;
}

void CBackend::EmitHeader(void)
{
}

void CBackend::EmitCode(void)
{
}

void CBackend::EmitData(void)
{
}

void CBackend::EmitFooter(void)
{
}


//------------------------------------------------------------------------------
// CBackendx86
//
CBackendx86::CBackendx86(ostream &out)
  : CBackend(out), _curr_scope(NULL)
{
  _ind = string(4, ' ');
}

CBackendx86::~CBackendx86(void)
{
}

void CBackendx86::EmitHeader(void)
{
  _out << "##################################################" << endl
       << "# " << _m->GetName() << endl
       << "#" << endl
       << endl;
}

void CBackendx86::EmitCode(void)
{
  _out << _ind << "#-----------------------------------------" << endl
       << _ind << "# text section" << endl
       << _ind << "#" << endl
       << _ind << ".text" << endl
       << _ind << ".align 4" << endl
       << endl
       << _ind << "# entry point and pre-defined functions" << endl
       << _ind << ".global main" << endl
       << _ind << ".extern Input" << endl
       << _ind << ".extern Output" << endl
       << endl;

  // TODO
  // forall s in subscopes do
  //   EmitScope(s)
  // EmitScope(program)

	vector<CScope*>::const_iterator sit = _m->GetSubscopes().begin();
	while (sit != _m->GetSubscopes().end())
		EmitScope(*sit++);
	EmitScope(_m);

	// TODO DONE

  _out << _ind << "# end of text section" << endl
       << _ind << "#-----------------------------------------" << endl
       << endl;
}

void CBackendx86::EmitData(void)
{
  _out << _ind << "#-----------------------------------------" << endl
       << _ind << "# global data section" << endl
       << _ind << "#" << endl
       << _ind << ".data" << endl
       << _ind << ".align 4" << endl
       << endl;

  EmitGlobalData(_m);

  _out << _ind << "# end of global data section" << endl
       << _ind << "#-----------------------------------------" << endl
       << endl;
}

void CBackendx86::EmitFooter(void)
{
  _out << _ind << ".end" << endl
       << "##################################################" << endl;
}

void CBackendx86::SetScope(CScope *scope)
{
  _curr_scope = scope;
}

CScope* CBackendx86::GetScope(void) const
{
  return _curr_scope;
}

void CBackendx86::EmitScope(CScope *scope)
{
  assert(scope != NULL);

  string label;

  if (scope->GetParent() == NULL) label = "main";
  else label = scope->GetName();

  // label
  _out << _ind << "# scope " << scope->GetName() << endl
       << label << ":" << endl;

  // TODO
  // ComputeStackOffsets(scope)
  //
  // emit function prologue
  //
  // forall i in instructions do
  //   EmitInstruction(i)
  //
  // emit function epilogue

	SetScope(scope);

	// function prologue
	// 1. push old ebp
	// 2. set esp to ebp
	// 3. save callee-saved registers
	// 4. make room for local variables
	EmitInstruction("pushl", "%ebp");
	EmitInstruction("movl", "%esp, %ebp");
	EmitInstruction("pushl", "%ebx");
	EmitInstruction("pushl", "%esi");
	EmitInstruction("pushl", "%edi");

	// compute stack offsets
	CSymtab *st = scope->GetSymbolTable();
	size_t size = ComputeStackOffsets(st, 8, -12);

	// subtract stack pointer by size
	EmitInstruction("subl", Imm(size) + ", %esp", "make room for locals");

	_out << endl;
	
	// emit all instructions
	CCodeBlock *cb = scope->GetCodeBlock();
	EmitCodeBlock(cb, st);

	_out << endl;

	// function epilogue
	// 1. delete room for local variables
	// 2. restore callee-saved registers
	// 3. restore old ebp
	// 4. issue the ret instruction
	_out << Label("exit") << ":" << endl;

	EmitInstruction("addl", Imm(size) + ", %esp", "remove locals");
	EmitInstruction("popl", "%edi");
	EmitInstruction("popl", "%esi");
	EmitInstruction("popl", "%ebx");
	EmitInstruction("popl", "%ebp");
	EmitInstruction("ret");

	// TODO DONE

	_out << endl;
}
void CBackendx86::EmitGlobalData(CScope *scope)
{
  assert(scope != NULL);

  // TODO
  // forall globals g in scope's symbol table do
  //   emit g respecting its alignment restrictions

	_out << _ind << "# scope: " << scope->GetName() << endl;

	CSymtab *st = scope->GetSymbolTable();
	vector<CSymbol*> sbs = st->GetSymbols();

	for (int i = 0; i < sbs.size(); i++) {
		CSymbol *sb = sbs[i];
		const CType *ty = sb->GetDataType();
		// assume global variables are of size 4
		if (sb->GetSymbolType() == stGlobal) {
			_out << left << setw(8) << sb->GetName() + ":" << setw(9) << ".skip " << setw(18) << 4;
			_out << " # " << ty << endl;
		}
	}

	// TODO DONE

	_out << endl;
}

void CBackendx86::EmitCodeBlock(CCodeBlock *cb, CSymtab *symtab)
{
  assert(cb != NULL);
  assert(symtab != NULL);

  const list<CTacInstr*> &instr = cb->GetInstr();
  list<CTacInstr*>::const_iterator it = instr.begin();

  while (it != instr.end()) EmitInstruction(*it++, symtab);
}

void CBackendx86::EmitInstruction(CTacInstr *i, CSymtab *symtab)
{
  assert(i != NULL);
  assert(symtab != NULL);

  ostringstream cmt;
  cmt << i;

	CTacName *name;
	CTacTemp *tmp;
	int nparams;

  EOperation op = i->GetOperation();
  switch (op) {
    // binary operators
    // dst = src1 op src2
		case opAdd:
		case opSub:
		case opMul:
		case opDiv:
			EmitInstruction("movl", Operand(i->GetSrc(1)) + ", %eax", cmt.str());
			switch (op) {
				case opAdd:
					EmitInstruction("addl", Operand(i->GetSrc(2)) + ", %eax");
					break;

				case opSub:
					EmitInstruction("subl", Operand(i->GetSrc(2)) + ", %eax");
					break;

				case opMul:
					EmitInstruction("imull", Operand(i->GetSrc(2)));
					break;

				case opDiv:
					EmitInstruction("cdq");
					EmitInstruction("movl", Operand(i->GetSrc(2)) + ", %ebx");
					EmitInstruction("idivl", "%ebx");
					break;

			}
			EmitInstruction("movl", "%eax, " + Operand(i->GetDest()));
			break;

		// unary operators
    // dst = op src1
		case opNeg:
			EmitInstruction("movl", Operand(i->GetSrc(1)) + ", %eax", cmt.str());
			EmitInstruction("negl", "%eax");
			EmitInstruction("movl", "%eax, " + Operand(i->GetDest()));
			break;

    // memory operations
    // dst = src1
		case opAssign:
			EmitInstruction("movl", Operand(i->GetSrc(1)) + ", %eax", cmt.str());
			EmitInstruction("movl", "%eax, " + Operand(i->GetDest()));
			break;

    // unconditional branching
    // goto dst
		case opGoto:
			EmitInstruction("jmp", Operand(i->GetDest()), cmt.str());
			break;

    // conditional branching
    // if src1 relOp src2 then goto dst
		case opEqual:
		case opNotEqual:
		case opLessThan:
		case opLessEqual:
		case opBiggerThan:
		case opBiggerEqual:
			EmitInstruction("movl", Operand(i->GetSrc(1)) + ", %eax", cmt.str());
			EmitInstruction("cmpl", Operand(i->GetSrc(2)) + ", %eax");
			EmitInstruction("j" + Condition(op), Operand(i->GetDest()));
			break;

    // function call-related operations
		case opParam:
			EmitInstruction("movl", Operand(i->GetSrc(1)) + ", %eax", cmt.str());
			EmitInstruction("pushl", "%eax");
			break;

		case opCall:
			EmitInstruction("call", Operand(i->GetSrc(1)), cmt.str());
			name = dynamic_cast<CTacName*>(i->GetSrc(1));
			tmp = dynamic_cast<CTacTemp*>(i->GetDest());
			nparams = dynamic_cast<const CSymProc*>(name->GetSymbol())->GetNParams();
			// if function has parameters
			if (nparams > 0)
				EmitInstruction("addl", Imm(nparams * 4) + ", %esp");
			// if function has return value
			if (tmp != NULL)
				EmitInstruction("movl", "%eax, " + Operand(tmp));
			break;

		case opReturn:
			if (i->GetSrc(1))
				EmitInstruction("movl", Operand(i->GetSrc(1)) + ", %eax", cmt.str());
			EmitInstruction("jmp", Label("exit"));
			break;

    // special
    case opLabel:
      _out << Label(dynamic_cast<CTacLabel*>(i)) << ":" << endl;
      break;

    case opNop:
      EmitInstruction("nop", "", cmt.str());
      break;


    default:
      EmitInstruction("# ???", "not implemented", cmt.str());
  }
}

void CBackendx86::EmitInstruction(string mnemonic, string args, string comment)
{
  _out << left
       << _ind
       << setw(7) << mnemonic << " "
       << setw(23) << args;
  if (comment != "") _out << " # " << comment;
  _out << endl;
}

string CBackendx86::Operand(CTac *op) const
{
  string operand;

	CTacConst *c = dynamic_cast<CTacConst*>(op);
	CTacName *n = dynamic_cast<CTacName*>(op);
	CTacTemp *t = dynamic_cast<CTacTemp*>(op);
	CTacLabel *l = dynamic_cast<CTacLabel*>(op);

  // TODO
  // return a string represending op

	if (c) {
		operand = Imm(c->GetValue());
	} else if (l) {
		operand = Label(l);
	} else if (n) {
		const CSymbol *sb = n->GetSymbol();
		ESymbolType st = sb->GetSymbolType();
		ostringstream o;
		// name -> offset(%base_register)
		if (st == stLocal || st == stParam) {
			o << sb->GetOffset() << "(" << sb->GetBaseRegister() << ")";
			operand = o.str();
		}
		// global variable
		else {
			operand = sb->GetName();
		}
	} else if (t) {
		// temporary variables are local variables
		const CSymbol *sb = t->GetSymbol();
		ostringstream o;
		o << sb->GetOffset() << "(" << sb->GetBaseRegister() << ")";
		operand = o.str();
	}

	// TODO DONE

  return operand;
}

string CBackendx86::Imm(int value) const
{
  ostringstream o;
  o << "$" << dec << value;
  return o.str();
}

string CBackendx86::Label(CTacLabel* label) const
{
  CScope *cs = GetScope();
  assert(cs != NULL);

  ostringstream o;
  o << "l_" << cs->GetName() << "_" << label->GetLabel();
  return o.str();
  return "l_" + cs->GetName() + "_" + label->GetLabel();
}

string CBackendx86::Label(string label) const
{
  CScope *cs = GetScope();
  assert(cs != NULL);

  return "l_" + cs->GetName() + "_" + label;
}

string CBackendx86::Condition(EOperation cond) const
{
  switch (cond) {
    case opEqual:       return "e";
    case opNotEqual:    return "ne";
    case opLessThan:    return "l";
    case opLessEqual:   return "le";
    case opBiggerThan:  return "g";
    case opBiggerEqual: return "ge";
    default:            assert(false); break;
  }
}


size_t CBackendx86::ComputeStackOffsets(CSymtab *symtab,
                                        int param_ofs,int local_ofs)
{
  assert(symtab != NULL);
  vector<CSymbol*> slist = symtab->GetSymbols();
	int this_local_ofs = 0, this_param_ofs = 0, size = 0;

  // TODO
  // foreach local symbol l in slist do
  //   compute aligned offset on stack and store in symbol l
  //   set base register to %ebp
  //
	// for each parameter p in slist do
	//   compute offset on stack and store in symbol p
	//   set base register to %ebp
	//
	// align size

	for (int i = 0; i < slist.size(); i++) {
		CSymbol *sb = slist[i];
		ESymbolType st = sb->GetSymbolType();
		const CType *dt = sb->GetDataType();

		if (st == stLocal) {
			if (dt->Match(CTypeManager::Get()->GetInt())) {
				// align by multiple of 4
				if (this_local_ofs % 4 != 0) {
					this_local_ofs -= 4 + this_local_ofs % 4;
				}
				// allocate size by 4
				this_local_ofs -= 4;
			} else if (dt->Match(CTypeManager::Get()->GetBool())) {
				this_local_ofs -= 1;
			}
			sb->SetBaseRegister("%ebp");
			sb->SetOffset(local_ofs + this_local_ofs);
		} else if (st == stParam) {
			// parameter type allows integer type only
			sb->SetBaseRegister("%ebp");
			sb->SetOffset(param_ofs + this_param_ofs);
			this_param_ofs += 4;
		}
	}

	// align size
	if (this_local_ofs % 4 != 0)
		size = -(this_local_ofs - (4 + this_local_ofs % 4));
	else
		size = -this_local_ofs;

  return size;
}
