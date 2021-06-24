#include <assert.h>
#include <stdio.h>
#include <map>
#include <vector>
#include <unordered_map>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include <string>
#include <sstream>
#include <algorithm>

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
   static CgenClassTableP instance;
private:
   List<CgenNode> *nds;
   std::map<Symbol, int> classsize;
   std::map<CgenNodeP, std::map<Symbol, int>> classattroffset;
   std::map<Symbol, std::map<Symbol, int>> classdisptaboffset; 
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
   int labelcount;
   int stackdepth;
// Calculates the class tag for each class using dfs
   void initialize_tag();
   int initialize_tag_dfs(CgenNodeP, int&);

// Calculates the class size for each class
   void initialize_size();

// Calculates the class attributes offsets (word offset) for each class
   void initialize_class_attr_offset();

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   void code_class_nameTab();
   void code_class_objTab();
   void code_class_dispTab();
   void code_method_name(CgenNodeP, std::vector<std::pair<Symbol, Symbol>>&);
   void code_class_protObj();
   void code_class_attr(CgenNodeP);
   void code_obj_init();
   void code_class_method();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   // cls[i] contains the class with tag i
   std::vector<CgenNodeP> cls;
   SymbolTable<Symbol, int> symtbl;
   CgenNodeP cur_class;
   std::map<Symbol, int> classtag;
   std::map<Symbol, int> classchildtag;

   CgenClassTable(Classes, ostream& str);

   void code();
   CgenNodeP root();
   // getInstance should be invoked after initialization
   static CgenClassTableP getInstance() { return instance; };
   int get_class_attr_offset(CgenNodeP, Symbol);
   int new_label() { return labelcount++; };
   int get_class_disptab_offset(Symbol, Symbol);
   // get the current stack depth (the offset of last temp of fp in word).
   int get_stack_depth() { return stackdepth; };
   void set_stack_depth(int depth) { stackdepth = depth; };
   int next_stack_depth() { return --stackdepth; };
   int prev_stack_depth() { return ++stackdepth; }; 

};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};


