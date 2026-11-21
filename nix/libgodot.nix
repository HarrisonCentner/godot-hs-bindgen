# Godot built as a shared library (libgodot.so), for programs that host the
# engine instead of being loaded by it. nixpkgs only packages the editor and
# the export templates, so this reuses the export template's derivation with
# Godot's `library_type` build option.
#
# This is a full engine build: about half an hour on a 12-core machine.

{ pkgs }:
pkgs.godot.export-template.overrideAttrs (old: {
  pname = "libgodot";

  sconsFlags =
    builtins.filter (flag: !(pkgs.lib.hasPrefix "target=" flag)) old.sconsFlags
    ++ [
      # The debug template keeps Godot's error checks and messages.
      "target=template_debug"
      "library_type=shared_library"
      # nixpkgs builds with production=true, which makes Godot refuse --path
      # and a project in the working directory, accepting only a packed
      # project next to the executable. A hosting program needs --path.
      "disable_path_overrides=no"
    ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib $out/include
    cp bin/libgodot.*.so $out/lib/libgodot.so
    cp core/extension/libgodot.h core/extension/gdextension_interface.gen.h $out/include/
    runHook postInstall
  '';

  # nixpkgs asks for a `big-parallel` builder. That is a scheduling hint for
  # build farms; without it the derivation is refused on machines that do not
  # advertise the feature, so it is dropped here.
  requiredSystemFeatures = [ ];
})
