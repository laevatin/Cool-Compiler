#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include <unordered_map>
#include <vector>
#include <list>
#include <unordered_set>
#include <string>
#include <queue>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

typedef std::list<Symbol> SymbolList;
typedef std::unordered_set<Symbol> SymbolSet;
typedef std::queue<Symbol> SymbolQueue;
typedef std::unordered_map<Symbol, Feature> FeatureMap;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;
  Classes classes;
  MethodTableP methodtable;
  AttrTableP attrtable;
  // Adjacent list for inheritance graph
  std::unordered_map<Symbol, SymbolList> children_list;

  // Add class and to the class namespace
  void add_class_namespace(Class_ cls);
  // Helper function for build graph
  void add_inheritance_relation(Symbol cur, Symbol par);
  // Build the inheritance graph and check the duplicated definitions
  void build_inheritance_graph();
  // Check if the class being inherited is defined
  void check_base_class();
  // Check if the inheritance graph is a tree
  void analyse_inheritance_graph();
  // Set up the method and attribute table. Duplicated features will be reported. 
  void setup_featuretable();
  // Check the redefinition in inheritance relations
  void check_feature_inheritance();

public:
  std::unordered_map<Symbol, Class_> class_namespace;
  // Constructor checks if the class relationships are correct
  ClassTable(Classes);
  ~ClassTable();
  // Check if Main is defined and if it contains method main()
  void check_main();
  // Get the class named sym
  Class_ get_class(Symbol sym);
  // Check and decorate all the types
  void check_type();
  // is T1 <= T2
  bool is_child_type(Symbol T1, Symbol T2, Class_ cls);
  Symbol lub_type(Symbol T1, Symbol T2, Class_ cls);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

};

class FeatureTable {
private:
  virtual void multiple_def_err(Class_ class_, Feature feature) = 0;
protected:
  std::unordered_map<Symbol, FeatureMap> class_feature_map;
  ClassTableP classtable;
public:
  FeatureTable(ClassTableP);
  // Set feature corresponding to class_name. Duplicated name will be 
  // reported as error. 
  void set_feature(Symbol class_name, Feature feature);
  // Set feature corresponding to class_name. Duplicated name will be 
  // reported as error. 
  void set_feature(Class_ class_, Feature feature);
  // Get the feature in class named class_name. If current class does not 
  // have the feature, it will search its parent classes. returns nullptr on error.
  Feature get_feature(Symbol class_name, Symbol feature_name);
  // Get the feature in class_. If current class does not have the feature, 
  // it will search its parent classes. returns nullptr on error.
  Feature get_feature(Class_ class_, Symbol feature_name);
  // Remove the feature in class_ named class_name. Do nothing if current class
  // does not contain the feature. Useful for checking feature signatures.
  void remove_feature(Symbol class_name, Symbol feature_name);
  // Remove the feature in class_. Do nothing if current class
  // does not contain the feature. Useful for checking feature signatures.
  void remove_feature(Class_ class_, Symbol feature_name);
};

class MethodTable : public FeatureTable {
public:
  MethodTable(ClassTableP p) : FeatureTable(p) {};
private:
  void multiple_def_err(Class_ class_, Feature method) {
    classtable->semant_error(class_->get_filename(), method) << "Method " 
      << method->get_name()->get_string() << " is multiply defined.\n";
  };
};

class AttrTable : public FeatureTable {
public:
  AttrTable(ClassTableP p) : FeatureTable(p) {};
private:
  void multiple_def_err(Class_ class_, Feature attr) {
    classtable->semant_error(class_->get_filename(), attr) << "Attribute " 
      << attr->get_name()->get_string() << " is multiply defined in class.\n";
  };
};

#endif
