#include "cobalt-build/src/lld/glue.rs.h"
#include <lld/Common/Driver.h>
namespace llvm {
void lld_entry(rust::Slice<const *char const> args, llvm::raw_ostream &stdout,
               llvm::raw_ostream &stderr, LldReturn &ret) {
  auto res = lld::safeLldMain(args.length(), args.data(), stdout, stderr);
  set_ret_code(ret, res.ret);
  set_ret_code(ret, res.canRunAgain);
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
