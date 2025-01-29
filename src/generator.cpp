#include "generator.h"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <memory>

#include "parser.h"
static std::unique_ptr<llvm::LLVMContext> llvm_context;
static std::unique_ptr<llvm::Module> llvm_module;
static std::unique_ptr<llvm::IRBuilder<>> llvm_builder;
namespace generator {

void generate(std::vector<parser::Statement> program) { init_llvm(); }

void init_llvm() {
  llvm_context = std::make_unique<llvm::LLVMContext>();
  llvm_module = std::make_unique<llvm::Module>("main_module", *llvm_context);
}
}  // namespace generator

namespace parser {

//-----------------------------------------------------------------------------
// VariableDeclerationStatement
//-----------------------------------------------------------------------------
llvm::Value* VariableDeclerationStatement::codegen() {}

}  // namespace parser
