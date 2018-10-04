{ lib, stdenv, emacs, emacsPackagesNg, emacsPackagesNgGen,
  runCommand, makeWrapper,
}:
let
  emacsWithPackages = (emacsPackagesNgGen emacs).emacsWithPackages;
  emacsUsePackage = emacsWithPackages (epkg: [ epkg.melpaPackages.use-package epkg.melpaPackages.general ]);

  extractPackages = emacsDir: runCommand "package-names"
  {}
  ''
    mkdir -p $out
    cd ${emacsDir}
    ${emacsUsePackage}/bin/emacs --batch \
      -f toggle-debug-on-error \
      --load ${./extract-packages.el} \
      --load ${emacsDir}/init.el \
      --eval "(output-packages \"$out\")"
  '';

  packagesFromFile = packageSet: file:
    let lines = lib.splitString "\n" (lib.fileContents file);
    in map (x: packageSet.${x}) lines;

  lookupPackages = file:
    let basename = builtins.baseNameOf file; in
    with emacsPackagesNg;
    if      basename == "melpa"        then packagesFromFile melpaPackages       file
    else if basename == "melpa-stable" then packagesFromFile melpaStablePackages file
    else if basename == "elpa"         then packagesFromFile elpaPackages        file
    else if basename == "top-level"    then packagesFromFile emacsPackagesNg     file
    else [];

  topPackages = extracted:
    let files = lib.filterAttrs (n: v: v == "regular") (builtins.readDir extracted);
        pkgs = lib.mapAttrs (n: v: lookupPackages "${extracted}/${n}") files;
    in lib.flatten (lib.attrValues pkgs);

  recDeps =
  let
    isPackage = pkg: lib.isDerivation pkg && lib.any (x: x == "recipe") (lib.attrNames pkg);
    immediateDeps = pkg: lib.filter isPackage pkg.propagatedBuildInputs;
  in pkgs: pkgs ++ lib.concatMap (x: recDeps (immediateDeps x)) pkgs;

  toPaths = deps: map (x: "${x}/share/emacs/site-lisp/elpa/${x.pname}-${x.version}") deps;

  byteCompileDir = emacsDir: siteStart: runCommand "emacsDir-byte-compiled"
  {
    inherit siteStart;
    emacs = emacsUsePackage;
  }
  ''
    mkdir -p $out

    cd ${emacsDir}
    # Copy directory structure
    find . -type d -depth | xargs -I{} mkdir -p $out/{}
    # Copy files
    find . -type f -name "*.el"| xargs -I{} install -m 0600 {} $out/{}

    cd $out
    $emacs/bin/emacs --batch \
      --load ${siteStart}/share/emacs/site-lisp/site-start.el \
      --eval "(byte-recompile-directory \".\" 0 't)"
  '';

in

emacsDir:

let
  deriv = stdenv.mkDerivation rec {
    inherit emacs;

    name = (lib.appendToName "with-packages" emacs).name;

    nativeBuildInputs = [ makeWrapper ];

    phases = [ "installPhase" ];

    extractedPackages = extractPackages emacsDir;

    emacsPackages = toPaths (recDeps (topPackages extractedPackages ++ [ emacsPackagesNg.use-package ]));

    siteStart = runCommand "site-start.el"
    {
      inherit emacs;
      deps = emacsPackages;
    }
    ''
      mkdir -p $out/share/emacs/site-lisp

      siteStart="$out/share/emacs/site-lisp/site-start.el"

      echo "(load-file \"$emacs/share/emacs/site-lisp/site-start.el\")" >> $siteStart
      for p in $deps; do
        echo "(add-to-list 'load-path \"$p\")" >> $siteStart
      done

      # Disable :ensure and :pin in use-package
      cat ${./disable-use-pin.el} >> $siteStart

      # Byte-compiling improves start-up time only slightly, but costs nothing.
      $emacs/bin/emacs --batch -f batch-byte-compile "$siteStart"
    '';

    installPhase = ''
      mkdir -p "$out/bin"
      # Wrap emacs and friends so they find our site-start.el before the original.
      for prog in ${emacs}/bin/*; do # */
        local progname=$(basename "$prog")
        rm -f "$out/bin/$progname"
        makeWrapper "$prog" "$out/bin/$progname" \
          --prefix EMACSLOADPATH ":" "${siteStart}/share/emacs/site-lisp:"
      done
      # Wrap MacOS app
      # this has to pick up resources and metadata
      # to recognize it as an "app"
      if [ -d "$emacs/Applications/Emacs.app" ]; then
        mkdir -p $out/Applications/Emacs.app/Contents/MacOS
        cp -r $emacs/Applications/Emacs.app/Contents/Info.plist \
              $emacs/Applications/Emacs.app/Contents/PkgInfo \
              $emacs/Applications/Emacs.app/Contents/Resources \
              $out/Applications/Emacs.app/Contents
        makeWrapper $emacs/Applications/Emacs.app/Contents/MacOS/Emacs $out/Applications/Emacs.app/Contents/MacOS/Emacs \
          --suffix EMACSLOADPATH ":" "${siteStart}/share/emacs/site-lisp:"
      fi
      mkdir -p $out/share
      # Link icons and desktop files into place
      for dir in applications icons info man; do
        ln -s $emacs/share/$dir $out/share/$dir
      done
    '';
    inherit (emacs) meta;
  };

in deriv
