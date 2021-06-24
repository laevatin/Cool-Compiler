
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

CgenClassTableP CgenClassTable::instance = nullptr;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(const char *dest_reg, int offset, const char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(const char *source_reg, int offset, const char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(const char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(const char *dest_reg, const char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(const char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(const char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(const char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(const char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(const char *dest_reg, const char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(const char *dest, const char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(const char *dest, const char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(const char *dest, const char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(const char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(const char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(const char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(const char *src1, const char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(const char *src1, const char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(const char *src1, const char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(const char *src1, const char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(const char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(const char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(const char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

// Pop the top element out of the stack. The stack shrines towards larger addresses.
static void emit_pop(const char *reg, ostream& str)
{
  emit_load(reg,1,SP,str);
  emit_addiu(SP,SP,4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(const char *dest, const char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(const char *source, const char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(const char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

static void emit_prologue(ostream& s) 
{
  emit_addiu(SP, SP, -12, s); // sp = sp - 12
  emit_store(FP, 3, SP, s);   // save old fp 
  emit_store(SELF, 2, SP, s); // save s0
  emit_store(RA, 1, SP, s);   // save ra
  emit_addiu(FP, SP, 4, s); // new fp
  emit_move(SELF, ACC, s);  // save a0 into s0
}

static void emit_epilogue(ostream& s, int numformal = 0)
{
  emit_load(FP, 3, SP, s);   // load old fp 
  emit_load(SELF, 2, SP, s); // load s0
  emit_load(RA, 1, SP, s);   // load ra
  emit_addiu(SP, SP, 12 + 4 * numformal, s); // sp = sp + 12
  s << RET << "\n";
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/

      emit_disptable_ref(idtable.lookup_string(STRINGNAME),s);// dispatch table 
      s << endl;                                              
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/
      emit_disptable_ref(idtable.lookup_string(INTNAME),s); // dispatch table 
      s << endl;                                          
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/
      emit_disptable_ref(idtable.lookup_string(BOOLNAME),s); // dispatch table
      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

// class_nameTab: A table, which at index (class tag)∗4 contains a pointer to 
// a String object containing the name of the class associated.
// 0: Object; 1: IO;

void CgenClassTable::code_class_nameTab()
{
  str << CLASSNAMETAB << LABEL;
  for (auto &c : cls) {
    str << WORD;
    stringtable.lookup_string(c->get_name()->get_string())->code_ref(str);
    // str << c->get_name();
    str << endl;
  }
}

// A table, which at index (class tag)∗8 contains a pointer to the 
// prototype object and at index (class tag)∗8 + 4 contains a pointer to 
// the initialization method for that class. 

void CgenClassTable::code_class_objTab()
{
  str << CLASSOBJTAB << LABEL;
  for (auto &c : cls) {
    str << WORD;
    emit_protobj_ref(c->get_name(), str);
    str << endl;
    str << WORD;
    emit_init_ref(c->get_name(), str);
    str << endl;
  }
}

// Emits the dispatch table for each class

void CgenClassTable::code_class_dispTab()
{
  for (auto &c : cls) {
    std::vector<std::pair<Symbol, Symbol>> methods;
    emit_disptable_ref(c->get_name(), str);
    str << LABEL;
    code_method_name(c, methods);
    std::map<Symbol, int> methodoffset;
    int offset = 0;
    for (auto &m : methods) {
      str << WORD; 
      emit_method_ref(m.first, m.second, str);
      // records the offset of the method name
      methodoffset[m.second] = offset++;
      str << endl;
    }
    classdisptaboffset.insert(std::make_pair(c->get_name(), std::move(methodoffset)));
  }
  // for (auto &p : classdisptaboffset) {
  //   cout << p.first << ": ";
  //   for (auto &p1 : p.second) {
  //     cout << p1.first << ", " << p1.second << "|";
  //   }
  //   cout << "\n";
  // }
}

// Helper method for code_class_dispTab to emit the method names

void CgenClassTable::code_method_name(CgenNodeP classnode, std::vector<std::pair<Symbol, Symbol>> &methods)
{
  Symbol classname = classnode->get_name();
  if (classname != Object) {
    code_method_name(classnode->get_parentnd(), methods);
  }

  Features fs = classnode->features;
  for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
    Feature f = fs->nth(i);
    if (f->is_method()) {
      Symbol name = f->get_name();
      bool is_defined = false;
      for (auto &m : methods) {
        if (m.second == name) {
          m.first = classname;
          is_defined = true;
        }
      }
      if (!is_defined) {
        methods.push_back(std::make_pair(classname, name));
      }
    }
  }
}

// Emits the class prototype object code for each class

void CgenClassTable::code_class_protObj()
{
  for (auto &c : cls) {
    Symbol classname = c->get_name();
    // GC tag at offset -4
    str << WORD; str << -1; str << endl;
    // Label
    emit_protobj_ref(classname, str); str << LABEL;
    // Class tag at offset 0
    str << WORD; str << classtag[classname]; str << endl;
    // Class size at offset 4
    str << WORD; str << classsize[classname]; str << endl;
    // Class dispatch table pointer at offset 8
    str << WORD; emit_disptable_ref(classname, str); str << endl;
    // Class attributes
    code_class_attr(c);
  } 
}

void CgenClassTable::code_class_attr(CgenNodeP classnode)
{
  if (classnode->get_name() != Object) {
    code_class_attr(classnode->get_parentnd());
  }
  Features fs = classnode->features;

  for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
    Feature f = fs->nth(i);
    if (!f->is_method()) {
      auto attr = dynamic_cast<attr_class *>(f);
      Symbol type = attr->type_decl;
      str << WORD;
      if (type == Bool) {
        // For type Bool, it is set to false (0)
        BoolConst bc = BoolConst(0);
        bc.code_ref(str);
      } 
      else if (type == Int) {
        // For type Int, it is set to 0
        inttable.lookup_string("0")->code_ref(str);
      }
      else if (type == Str) {
        // For type String, it is set to the empty string ""
        stringtable.lookup_string("")->code_ref(str);
      }
      else {
        // For other type, it is set to 0 
        str << 0;
      }
      str << endl;
    }
  }
}

//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
  // Singleton
   cur_class = nullptr;
   cgen_debug = 0;
   instance = this;
   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

   initialize_tag();
   stringclasstag = classtag[Str];
   intclasstag = classtag[Int];
   boolclasstag = classtag[Bool];
   // initialization of size must after the initialization of tag
   initialize_size();
   initialize_class_attr_offset();

   code();
   exitscope();
}

void CgenClassTable::initialize_tag() 
{
  int tag = 0;
  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    CgenNodeP node = l->hd();
    if (node->get_name() == Object) {
      initialize_tag_dfs(node, tag);
      break;
    }
  }

}

void CgenClassTable::initialize_size()
{
  // The order in cls guarantees that the child is evaluated after its parent
  for (auto &c : cls) {
    int obj_size = DEFAULT_OBJFIELDS;
    Symbol classname = c->get_name();
    if (classname != Object) 
      obj_size = classsize[c->get_parent()];

    Features fs = c->features;
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
      Feature f = fs->nth(i);
      if (!f->is_method()) {
        obj_size += 1;
      }
    }
    classsize[classname] = obj_size;
  }
}

int CgenClassTable::initialize_tag_dfs(CgenNodeP node, int& tag)
{
  Symbol classname = node->get_name();
  classtag[classname] = tag;
  cls.push_back(node);
  int child = tag++;

  for (List<CgenNode> *l = node->get_children(); l; l = l->tl()) {
    child = std::max(initialize_tag_dfs(l->hd(), tag), child);
  }
  classchildtag[classname] = child;
  return child;
}

void CgenClassTable::initialize_class_attr_offset() 
{
  for (auto &c : cls) {
    int offset = classsize[c->get_parent()];
    std::map<Symbol, int> attrmap;

    Features fs = c->features;
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
      Feature f = fs->nth(i);
			if (!f->is_method()) {
        attrmap[f->get_name()] = offset;
        offset += 1;
			}
		}

    classattroffset.insert(std::make_pair(c, std::move(attrmap)));
  }

  for (auto c : cls) {
    while (c->get_name() != Object) {
      classattroffset[c].insert(classattroffset[c->get_parentnd()].begin(), classattroffset[c->get_parentnd()].end());
      c = c->get_parentnd();
    }
  }

  // dump the classattroffset table
  // for (auto &p : classattroffset) {
  //   cout << p.first->get_name() << ": ";
  //   for (auto &p1 : p.second) {
  //     cout << p1.first << ", " << p1.second << "|";
  //   }
  //   cout << "\n";
  // }
}

void CgenClassTable::code_obj_init()
{
  for (auto &c : cls) {
    cur_class = c;
		Symbol name = c->get_name();
		emit_init_ref(name, str);
		str << LABEL;

    emit_prologue(str);
    if (name != Object) {
      str << JAL; emit_init_ref(c->get_parent(), str);
      str << endl;
    }

    Features fs = c->features;
    // int offset = classsize[c->get_parent()]; // returns 0 if c is Object
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
			Feature f = fs->nth(i);
			if (!f->is_method()) {
				attr_class* attr = dynamic_cast<attr_class *>(f);
        if (!attr->init->is_no_expr()) {
          attr->init->code(str);
          emit_store(ACC, get_class_attr_offset(c, attr->get_name()), SELF, str);
          
          if (cgen_debug)
            str << "# end emit " << attr->get_name() << endl; 
        }
        // offset += 1;
			}
		}
    // return self
    emit_move(ACC, SELF, str);
    emit_epilogue(str);
  }
}

/*****************************************************************************
 * 
 * Activation Record for method calling
 * 
 * High Address->| Arg 1 |
 *               | Arg 2 |
 *               | ..... |
 *               | Arg n |
 *               | OldFP |
 *               |  $s0  |
 *           fp->|  $ra  |
 *           sp->|       |
 * Low Address ->|       |
 * 
 * self is passed by $a0
 * 
 * ***************************************************************************/
void CgenClassTable::code_class_method() 
{
  for (auto &c : cls) {
    cur_class = c;
    Symbol classname = c->get_name();
    if (classname == Object || classname == Str || classname == Int || classname == IO)
      continue;

    // symtbl.enterscope();

    Features fs = c->features;
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
      Feature f = fs->nth(i);

      if (f->is_method()) {
        method_class *method = dynamic_cast<method_class *>(f);
        emit_method_ref(classname, f->get_name(), str);
        str << LABEL;
        emit_prologue(str);
        // add formals into symbol table
        Formals formals = method->formals;
        int offset = 2 + formals->len();
        symtbl.enterscope();
        for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
          formal_class *formal = dynamic_cast<formal_class *>(formals->nth(i));
          symtbl.addid(formal->name, new int(offset--));
        }
        method->expr->code(str);
        symtbl.exitscope();
        emit_epilogue(str, formals->len());
      }
    }

    // symtbl.exitscope();
  }
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
				   single_Formals(formal(arg, Str)),
				   Str, 
				   no_expr()))),
	    single_Features(method(substr, 
				   append_Formals(single_Formals(formal(arg, Int)), 
						  single_Formals(formal(arg2, Int))),
				   Str, 
				   no_expr()))),
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}



void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//
  if (cgen_debug) cout << "coding class_nameTab" << endl;
  code_class_nameTab();

  if (cgen_debug) cout << "coding class_objTab" << endl;
  code_class_objTab();

  if (cgen_debug) cout << "coding class dispatch table" << endl;
  code_class_dispTab();

  if (cgen_debug) cout << "coding class prototype object" << endl;
  code_class_protObj();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...
  if (cgen_debug) cout << "coding object initializer" << endl;
  code_obj_init();

  if (cgen_debug) cout << "coding class methods" << endl;
  code_class_method();

}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}

int CgenClassTable::get_class_attr_offset(CgenNodeP node, Symbol attr) 
{
  return classattroffset[node][attr];
}

int CgenClassTable::get_class_disptab_offset(Symbol cls, Symbol attr)
{
  return classdisptaboffset[cls][attr];
}

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s) {
  if (cgen_debug)
    s << "# assign class here\n";
  expr->code(s);
  int *offset = CgenClassTable::getInstance()->symtbl.lookup(name);

  if (!offset) {
    CgenNodeP cur_cls = CgenClassTable::getInstance()->cur_class;
    if (!cur_cls) {
      cout << "ERROR: cur_cls is nullptr\n";
      return;        
    } 
    int of = CgenClassTable::getInstance()->get_class_attr_offset(cur_cls, name);
    if (of == 0) {
      cout << "ERROR: offset of " << name <<  " is 0\n";
      return; 
    }
    emit_store(ACC, of, SELF, s);
  } else {
    // temporary variable on the stack
    emit_store(ACC, *offset, FP, s);
  }

}

void static_dispatch_class::code(ostream &s) {
  if (cgen_debug)
    s << "# static_dispatch_class here\n";
  
  CgenClassTableP classtable = CgenClassTable::getInstance();
  int offset_org = classtable->get_stack_depth();
  classtable->set_stack_depth(0);
  
  int label = classtable->new_label();
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s);
    emit_push(ACC, s);
  }
  expr->code(s);
  // check dispatch on void
  emit_bne(ACC, ZERO, label, s);
  Symbol filename = classtable->cur_class->get_filename();
  emit_load_string(ACC, stringtable.lookup_string(filename->get_string()), s);
  emit_load_imm(T1, line_number, s);
  emit_jal(DISPATCHABORT, s);

  emit_label_def(label, s);
  // load the dispatch table address
  std::ostringstream strstream;
  emit_disptable_ref(type_name, strstream);
  emit_load_address(T1, strstream.str().c_str(), s);

  int offset = classtable->get_class_disptab_offset(type_name, name);
  emit_load(T1, offset, T1, s);  // load the function pointer
  emit_jalr(T1, s);

  classtable->set_stack_depth(offset_org);
}

/*****************************************************************************
 * 
 * Activation Record for method calling
 * 
 * High Address->| Arg 1 |
 *               | Arg 2 |
 *               | ..... |
 *               | Arg n |
 *               | OldFP |
 *               |  $s0  |
 *           fp->|  $ra  |
 *           sp->|       |
 * Low Address ->|       |
 * 
 * self is passed by $a0
 * 
 * ***************************************************************************/
void dispatch_class::code(ostream &s) {
  if (cgen_debug)
    s << "# dispatch_class here\n";
  CgenClassTableP classtable = CgenClassTable::getInstance();
  int offset_org = classtable->get_stack_depth();
  classtable->set_stack_depth(0);

  int label = classtable->new_label();
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s);
    emit_push(ACC, s);
  }
  expr->code(s);
  emit_bne(ACC, ZERO, label, s);
  Symbol filename = classtable->cur_class->get_filename();
  emit_load_string(ACC, stringtable.lookup_string(filename->get_string()), s);
  emit_load_imm(T1, line_number, s);
  emit_jal(DISPATCHABORT, s);
  emit_label_def(label, s);
  emit_load(T1, 2, ACC, s); // get the dispatch table
  Symbol t_disp = expr->get_type();
  if (t_disp == SELF_TYPE) t_disp = classtable->cur_class->get_name();
  int offset = classtable->get_class_disptab_offset(t_disp, name);
  // s << t_disp << " " << offset << "\n";
  emit_load(T1, offset, T1, s);  // load the function pointer
  emit_jalr(T1, s);

  classtable->set_stack_depth(offset_org);
}

void cond_class::code(ostream &s) {
  if (cgen_debug)
    s << "# cond_class here\n";
  int label_else = CgenClassTable::getInstance()->new_label();
  pred->code(s);                    // get a bool in a0
  emit_load(T1, 3, ACC, s);         // get the value of bool into t1
  emit_beqz(T1, label_else, s);     // else branch
  then_exp->code(s);                // then branch
  int label_end = CgenClassTable::getInstance()->new_label();
  emit_branch(label_end, s);
  emit_label_def(label_else, s);    // else branch
  else_exp->code(s);
  emit_label_def(label_end, s);
}

void loop_class::code(ostream &s) {
  if (cgen_debug)
    s << "# loop_class here\n";
  int loop = CgenClassTable::getInstance()->new_label();
  int end = CgenClassTable::getInstance()->new_label();
  emit_label_def(loop, s);
  
  pred->code(s);
  emit_load(T1, 3, ACC, s);
  emit_beqz(T1, end, s);
  
  body->code(s);
  emit_branch(loop, s);

  emit_label_def(end, s);
  emit_move(ACC, ZERO, s);
}

void typcase_class::code(ostream &s) {
  if (cgen_debug)
    s << "# typcase_class here\n";
  CgenClassTableP classtable = CgenClassTable::getInstance();
  
  std::vector<std::pair<Symbol, int>> typelist;
  std::map<Symbol, int> typeidx;
  std::map<Symbol, int> typelabel;

  expr->code(s);
  // Get the class tag of the object created by expr
  
  int end = classtable->new_label();
  int abort1 = classtable->new_label();
  int abort2 = classtable->new_label();

  emit_beqz(ACC, abort2, s);
  emit_load(T1, 0, ACC, s);

  // Class tag is the order of depth-first traversal,
  // class child tag is the maximum tag the child can have,
  // so the class tag of a certain class' child is between
  // its class tag and the class child tag.
  // The closest ancestor of one class with tag t is the class with 
  // class child tag >= t and with the largest class tag.

  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    Case cs = cases->nth(i);
    branch_class *bc = dynamic_cast<branch_class *>(cs);
    if (bc) {
      Symbol t = bc->type_decl;
      typelist.push_back(std::make_pair(t, classtable->classtag[t]));
      typeidx[t] = i;
      typelabel[t] = classtable->new_label();
    }
  }

  std::sort(std::begin(typelist), std::end(typelist), 
    [](const auto &x1, const auto &x2) -> bool {
      return x1.second < x2.second;
    });

  Symbol prev_type = nullptr;

  emit_load_imm(T2, 0, s);

  std::ostringstream strstream;
  emit_label_ref(abort1, strstream);
  emit_load_address(T3, strstream.str().c_str(), s);

  for (auto& p : typelist) {
    Symbol t = p.first;
    int tag = classtable->classtag[t];
    int maxchildtag = classtable->classchildtag[t];
    int next_label = classtable->new_label();

    if (!prev_type) {
      emit_blti(T1, tag, abort1, s);
    } else {
      emit_blti(T1, tag, typelabel[prev_type], s); 
    }
    emit_bgti(T1, maxchildtag, next_label, s);
    
    // T2 indicates if there are at least one branch can be selected.
    emit_addiu(T2, T2, 1, s);

    strstream.str("");
    emit_label_ref(typelabel[t], strstream);
    emit_load_address(T3, strstream.str().c_str(), s);

    emit_label_def(next_label, s);
    prev_type = p.first;
  }

  // the last one may be selected
  s << "\tjr\t" << T3 << "\n";

  // abort1
  emit_label_def(abort1, s);
  emit_jal(CASE_ABORT, s);
  // abort2
  emit_label_def(abort2, s);
  emit_load_string(ACC, stringtable.lookup_string(
    classtable->cur_class->get_filename()->get_string()), s);
  emit_load_imm(T1, line_number, s);
  emit_jal(CASE_ABORT2, s);

  for (auto& p : typelist) {
    Symbol t = p.first;
    Case cs = cases->nth(typeidx[t]);
    branch_class *bc = dynamic_cast<branch_class *>(cs);
    if (bc) {
      emit_label_def(typelabel[t], s);
      // check if T2 is set
      emit_beqz(T2, abort1, s);

      classtable->symtbl.enterscope();
      int offset = classtable->next_stack_depth();
      emit_store(ACC, offset, FP, s);
      emit_addiu(SP, SP, -4, s);

      classtable->symtbl.addid(bc->name, new int(offset));
      bc->expr->code(s);
      
      emit_addiu(SP, SP, 4, s);
      classtable->prev_stack_depth();
      classtable->symtbl.exitscope();
      emit_branch(end, s);
    }
  }

  emit_label_def(end, s);

}

void block_class::code(ostream &s) {
  if (cgen_debug)
    s << "# block_class here\n";
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    body->nth(i)->code(s);
  }
}

/*****************************************************************************
 * 
 * Activation Record for let
 * 
 * High Address->| OldFP |
 *               |  $s0  |
 *           fp->|  $ra  |
 *               |  temp |
 *           sp->|       |
 * Low Address ->|       |
 * 
 * self is passed by $a0
 * 
 * ***************************************************************************/
void let_class::code(ostream &s) {
  if (cgen_debug)
    s << "# let_class here\n";
  CgenClassTableP classtable = CgenClassTable::getInstance();
  classtable->symtbl.enterscope();

  if (init->is_no_expr()) {
    if (type_decl == Int) 
      emit_load_int(ACC, inttable.lookup_string("0"), s);
    else if (type_decl == Bool)
      emit_load_bool(ACC, BoolConst(0), s);
    else if (type_decl == Str)
      emit_load_string(ACC, stringtable.lookup_string(""), s);
    else 
      emit_move(ACC, ZERO, s); // void 
  } else {
    init->code(s);
  }

  int offset = classtable->next_stack_depth();

  emit_store(ACC, offset, FP, s);
  emit_addiu(SP, SP, -4, s);

  classtable->symtbl.addid(identifier, new int(offset));
  body->code(s);

  emit_addiu(SP, SP, 4, s);
  classtable->prev_stack_depth();
  classtable->symtbl.exitscope();
}

void plus_class::code(ostream &s) {
  if (cgen_debug)
    s << "# plus_class here\n";
  e1->code(s);
  emit_push(ACC, s);
  e2->code(s);
  emit_push(ACC, s);
  emit_jal(COPY, s); // ACC is an int object
  emit_pop(T1, s);
  emit_load(T1, 3, T1, s);
  emit_pop(T2, s);
  emit_load(T2, 3, T2, s);
  emit_add(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
}

void sub_class::code(ostream &s) {
  if (cgen_debug)
    s << "# sub_class here\n";
  e1->code(s);
  emit_push(ACC, s);
  e2->code(s);
  emit_push(ACC, s);
  emit_jal(COPY, s); // ACC is an int object
  emit_pop(T1, s);
  emit_load(T1, 3, T1, s);
  emit_pop(T2, s);
  emit_load(T2, 3, T2, s);
  emit_sub(T1, T2, T1, s); // T2 is e1, T1 is e2
  emit_store_int(T1, ACC, s);
}

void mul_class::code(ostream &s) {
  if (cgen_debug)
    s << "# mul_class here\n";
  e1->code(s);
  emit_push(ACC, s);
  e2->code(s);
  emit_push(ACC, s);
  emit_jal(COPY, s); // ACC is an int object
  emit_pop(T1, s);
  emit_load(T1, 3, T1, s);
  emit_pop(T2, s);
  emit_load(T2, 3, T2, s);
  emit_mul(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
}

void divide_class::code(ostream &s) {
  if (cgen_debug)
    s << "# divide_class here\n";
  e1->code(s);
  emit_push(ACC, s);
  e2->code(s);
  emit_push(ACC, s);
  emit_jal(COPY, s); // ACC is an int object
  emit_pop(T1, s);
  emit_load(T1, 3, T1, s);
  emit_pop(T2, s);
  emit_load(T2, 3, T2, s);
  emit_div(T1, T2, T1, s); // T2 is e1, T1 is e2
  emit_store_int(T1, ACC, s);
}

void neg_class::code(ostream &s) {
  if (cgen_debug)
    s << "# neg_class here\n";
  e1->code(s);        
  emit_jal(COPY, s); // get a copy of the int object
  emit_load(T1, 3, ACC, s); // t1 = int.val
  emit_neg(T1, T1, s);
  emit_store_int(T1, ACC, s);
}

void lt_class::code(ostream &s) {
  if (cgen_debug)
    s << "# lt_class here\n";
  e1->code(s);
  emit_push(ACC,s);
  e2->code(s);
  emit_pop(T1,s);
  emit_load(T1,3,T1,s);
  emit_load(T2,3,ACC,s);
  emit_load_bool(ACC,BoolConst(1),s);
  int blt_label = CgenClassTable::getInstance()->new_label();
  emit_blt(T1,T2,blt_label,s);
  emit_load_bool(ACC,BoolConst(0),s);
  emit_label_def(blt_label,s);
}

void eq_class::code(ostream &s) {
  if (cgen_debug)
    s << "# eq_class here\n";
  e1->code(s);
  emit_push(ACC,s);
  e2->code(s);
  emit_pop(T1,s);
  emit_move(T2, ACC, s);
  emit_load_bool(ACC,BoolConst(1),s);
  int eq_label = CgenClassTable::getInstance()->new_label();
  emit_beq(T1, T2, eq_label, s);
  emit_load_bool(A1,BoolConst(0),s);
  emit_jal(EQUAL_TEST, s);
  emit_label_def(eq_label, s);
}

void leq_class::code(ostream &s) {
  if (cgen_debug)
    s << "# leq_class here\n";
  e1->code(s);
  emit_push(ACC,s);
  e2->code(s);
  emit_pop(T1,s);
  emit_load(T1,3,T1,s);
  emit_load(T2,3,ACC,s);
  emit_load_bool(ACC,BoolConst(1),s);
  int bleq_label = CgenClassTable::getInstance()->new_label();
  emit_bleq(T1,T2,bleq_label,s);
  emit_load_bool(ACC,BoolConst(0),s);
  emit_label_def(bleq_label,s);
}

void comp_class::code(ostream &s) {
  if (cgen_debug)
    s << "# comp_class here\n";
  e1->code(s);        
  emit_load(T1,3,ACC,s);
  emit_load_bool(ACC,BoolConst(1),s);
  int comp_label = CgenClassTable::getInstance()->new_label();
  emit_beqz(T1,comp_label,s);
  emit_load_bool(ACC,BoolConst(0),s);
  emit_label_def(comp_label,s);
}

void int_const_class::code(ostream& s)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s)   
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {
  if (cgen_debug)
    s << "# new class " << type_name << " here\n";

  if (type_name != SELF_TYPE) {
    std::ostringstream strstream;
    emit_protobj_ref(type_name, strstream);
    emit_load_address(ACC, strstream.str().c_str(), s);
    emit_jal(COPY, s);
    strstream.str(""); // clear the stream buffer
    emit_init_ref(type_name, strstream);
    emit_jal(strstream.str().c_str(), s);
    return;
  }

  emit_load_address(T1, CLASSOBJTAB, s);  // load class object table to t1
  emit_load(T2, 0, SELF, s);              // load self class tag to t2
  emit_sll(T2, T2, 3, s);                 // t2 *= 8
  emit_addu(T1, T1, T2, s);               // t1 += t2 t1 is the address of self class init
  emit_move(S1, T1, s);                   // since s1 is not used by the runtime system, we do not need to save it
  emit_load(ACC, 0, T1, s);               // load prototype obj into ACC
  emit_jal(COPY, s);                      // call Object.copy to generate new object
  emit_load(T1, 1, S1, s);                // load init function pointer into t1
  emit_jalr(T1, s);                       // call the init function

}

void isvoid_class::code(ostream &s) {
  if (cgen_debug)
    s << "# isvoid_class here\n";
  e1->code(s);
  emit_move(T1,ACC,s);
  emit_load_bool(ACC,BoolConst(1),s);
  int beqz_label = CgenClassTable::getInstance()->new_label();
  emit_beqz(T1,beqz_label,s);
  emit_load_bool(ACC,BoolConst(0),s);
  emit_label_def(beqz_label,s); 
}

void no_expr_class::code(ostream &s) {
  if (cgen_debug)
    s << "# no_expr_class here\n";
}

void object_class::code(ostream &s) {
  if (cgen_debug)
    s << "# object_class " << name << " here\n";
  // Object ID. Need symbol table
  if (type == SELF_TYPE) {
    emit_move(ACC, SELF, s);
  } else {
    int *offset = CgenClassTable::getInstance()->symtbl.lookup(name);

    if (!offset) {
      CgenNodeP cur_cls = CgenClassTable::getInstance()->cur_class;
      if (!cur_cls) {
        cout << "ERROR: cur_cls is nullptr\n";
        return;        
      } 
      int of = CgenClassTable::getInstance()->get_class_attr_offset(cur_cls, name);
      if (of == 0) {
        cout << "ERROR: offset of " << name <<  " is 0\n";
        return; 
      }
      emit_load(ACC, of, SELF, s);
      return;
    }
    // temporary variable on the stack
    emit_load(ACC, *offset, FP, s);
  }
}



