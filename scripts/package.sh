#!/usr/bin/env bash
# Packages scheme-rs as a self-contained, relocatable bundle (the
# Guile/libpython shipping model): the interpreter binary plus the shared
# runtime image it links against.
#
#   dist/
#     scheme-rs             the interpreter; resolves its libraries relative
#                           to itself via @executable_path
#     libscheme_rs.dylib    the runtime image (install name
#                           @rpath/libscheme_rs.dylib)
#     libstd-*.dylib        Rust's standard library. Shared on purpose: a
#                           static libstd inside the runtime dylib plus
#                           another inside a plugin would recreate the
#                           dual-image problem one level down.
#     libtest_plugin.dylib  the test plugin, built in the same cargo
#                           invocation so it links the exact same runtime
#
# Plugins dlopened by the packaged binary reference
# @rpath/libscheme_rs.dylib and @rpath/libstd-*.dylib; dyld matches those
# against the images the host has already loaded, so the process holds
# exactly one runtime image. load_plugin's identity canary rejects anything
# else.
set -euo pipefail

if [[ "$(uname)" != "Darwin" ]]; then
    echo "package.sh: only macOS is implemented so far" >&2
    exit 1
fi

cd "$(dirname "$0")/.."

# Host binary and plugin must agree on the exact scheme-rs build (symbol
# names include the crate disambiguator), so build both in one invocation.
cargo build --profile dist -p scheme-rs -p test-plugin --all-features

target=target/dist
dist=dist
rm -rf "$dist"
mkdir -p "$dist"

cp "$target/scheme-rs" "$dist/"
cp "$target/deps/libscheme_rs.dylib" "$dist/"
cp "$target/deps/libtest_plugin.dylib" "$dist/"
cp "$(rustc --print target-libdir)"/libstd-*.dylib "$dist/"

# rustc records the runtime dylib's absolute build path as its install name;
# rewrite everything to @rpath so the bundle is relocatable and can never
# resolve back into the build tree.
build_path=$(otool -D "$dist/libscheme_rs.dylib" | tail -1)
install_name_tool -id @rpath/libscheme_rs.dylib "$dist/libscheme_rs.dylib"
install_name_tool -id @rpath/libtest_plugin.dylib "$dist/libtest_plugin.dylib"
install_name_tool -change "$build_path" @rpath/libscheme_rs.dylib "$dist/scheme-rs"
install_name_tool -change "$build_path" @rpath/libscheme_rs.dylib "$dist/libtest_plugin.dylib"
install_name_tool -add_rpath @executable_path "$dist/scheme-rs"
install_name_tool -add_rpath @loader_path "$dist/libscheme_rs.dylib"
install_name_tool -add_rpath @loader_path "$dist/libtest_plugin.dylib"

# install_name_tool invalidates code signatures; re-sign ad hoc.
for f in "$dist/scheme-rs" "$dist"/*.dylib; do
    codesign --force --sign - "$f"
done

echo "packaged into $dist/"
