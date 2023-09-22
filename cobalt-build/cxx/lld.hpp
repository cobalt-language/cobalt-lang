#ifndef COBALT_LLD_HPP
#define COBALT_LLD_HPP
#include "rust/cxx.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>
namespace llvm {
struct LldReturn;
void lld_entry(rust::Slice<const *char const>, llvm::raw_ostream &,
               llvm::raw_ostream &, LldReturn &);
std::unique_ptr<llvm::raw_ostream> string_ostream(std::string &);
std::unique_ptr<llvm::raw_ostream> file_ostream(rust::Slice<const uint8_t>);
llvm::raw_ostream *outs_ptr();
llvm::raw_ostream *errs_ptr();
} // namespace llvm
#endif