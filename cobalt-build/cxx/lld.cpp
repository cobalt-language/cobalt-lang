#include "cobalt-build/src/lld/glue.rs.h"
#include <lld/Common/Driver.h>
namespace llvm {
int lld_entry(rust::Slice<const char *> rust_args, llvm::raw_ostream &stdout,
              llvm::raw_ostream &stderr, LldFlavor flavor) {
  llvm::ArrayRef<const char *> args{rust_args.data(), rust_args.length()};
  switch (flavor) {
  case Elf:
    return lld::elf::link(args, stdout, stderr, false, false);
  case Coff:
    return lld::coff::link(args, stdout, stderr, false, false);
  case MachO:
    return lld::macho::link(args, stdout, stderr, false, false);
  case Wasm:
    return lld::wasm::link(args, stdout, stderr, false, false);
  }
}
std::unique_ptr<llvm::raw_ostream> string_ostream(std::string &buf) {
  return std::make_unique<llvm::raw_string_ostream>(buf);
}
std::unique_ptr<llvm::raw_ostream>
file_ostream(rust::Slice<const uint8_t> path) {
  std::string_view view{reinterpret_cast<const char *>(path.data()),
                        path.length()};
  std::error_code ec;
  auto out = std::make_unique<llvm::raw_fd_ostream>(view, ec);
  if (ec)
    throw ec;
  return out;
}
llvm::raw_ostream *outs_ptr() { return &llvm::outs(); }
llvm::raw_ostream *errs_ptr() { return &llvm::errs(); }
} // namespace llvm
