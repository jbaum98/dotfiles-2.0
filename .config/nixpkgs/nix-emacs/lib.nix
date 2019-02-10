{ lib, runCommand, emacs, emacsPackagesNg, emacsPackagesNgGen }:
let
  emacsWithPackages = (emacsPackagesNgGen emacs).emacsWithPackages;

  # Run extract-packages.el on initFile
  extractPackagesDir = emacsInit: initFile: runCommand "package-names"
  { inherit emacsInit initFile; }
  ''
    mkdir -p $out
    $emacsInit/bin/emacs --batch \
      -f toggle-debug-on-error \
      --load ${./extract-packages.el} \
      --load $initFile \
      --eval "(expkgs-output-packages \"$out\")"
  '';

  filePaths = dir:
  let files = lib.filterAttrs (n: v: v == "regular") (builtins.readDir dir);
  in lib.mapAttrs (n: v: "${dir}/${n}") files;

  parseExtract = extracted:
  let splitFile = file: lib.splitString "\n" (lib.fileContents file);
  in lib.mapAttrs (n: splitFile) (filePaths extracted);

  # Get all the packages listed in file from packageSet
  packagesFromFile = packageSet: file:
    let lines = lib.splitString "\n" (lib.fileContents file);
    in map (x: packageSet.${x}) lines;

 # Lookup packages from a file
 getPkgSet = name: epkgs:
    if      name == "melpa"        then epkgs.melpaPackages
    else if name == "melpa-stable" then epkgs.melpaStablePackages
    else if name == "elpa"         then epkgs.elpaPackages
    else {};

  isPackage = pkg: lib.isDerivation pkg && lib.any (x: x == "recipe") (lib.attrNames pkg);

  immediateDeps = pkg: lib.filter isPackage pkg.propagatedBuildInputs;

  # Recursively get the dependencies of a list of Emacs packages.
  recDeps = pkgs: pkgs ++ lib.concatMap (x: recDeps (immediateDeps x)) pkgs;

  /* Convert a list of Emacs packages to a list of directories
     that should be added to the load-path. */
  toPaths = deps: map (x: "${x}/share/emacs/site-lisp/elpa/${x.pname}-${x.version}") deps;


in rec {
  # The emacs capable of running our configuration
  emacsInit = emacsWithPackages (epkg: with epkg.melpaPackages; [
    use-package
    general
  ]);

  extractPackageNames = initFile:
    parseExtract (extractPackagesDir emacsInit initFile);

  allNames = extracted: lib.flatten (lib.attrValues extracted);

  indexNames = names: epkgs: map (n: epkgs.${n}) names;

  makeOverlay = extracted: self: super:
    let
      ix = pkgSet: pkgNames:
        map (pkgName: lib.nameValuePair pkgName pkgSet.${pkgName}) pkgNames;
      pkgPairs = setName:
        if      setName == "melpa"        then ix self.melpaPackages
        else if setName == "melpa-stable" then ix self.melpaStablePackages
        else if setName == "elpa"         then ix self.elpaPackages
        else _: [];
    in lib.listToAttrs (lib.flatten (lib.mapAttrsToList pkgPairs extracted));

  /* Byte compile a directory containg Emacs lisp files. */
  byteCompileDir = dir: siteStart: runCommand "${dir}-byte-compiled" {}
  ''
    mkdir -p $out

    cd ${dir}
    # Copy directory structure
    find . -type d -depth | xargs -I{} mkdir -p $out/{}
    # Copy files
    find . -type f -name "*.el"| xargs -I{} install -m 0600 {} $out/{}

    cd $out
    ${emacsInit}/bin/emacs --batch \
      --load ${siteStart}/share/emacs/site-lisp/site-start.el \
      --eval "(byte-recompile-directory \".\" 0 't)"
  '';


  /* Given a list of Emacs packages, create a site-start.el file that
     will add the correct paths to the load-path.

     Also, disableds the default behavior of :ensure and :pin.
   */
   siteStart = epkgs: runCommand "site-start.el" {
     loadpaths = toPaths (recDeps epkgs);
   }
    ''
      mkdir -p $out/share/emacs/site-lisp

      siteStart="$out/share/emacs/site-lisp/site-start.el"

      echo "(load-file \"${emacs}/share/emacs/site-lisp/site-start.el\")" >> $siteStart
      for lp in $loadpaths; do
        echo "(add-to-list 'load-path \"$lp\")" >> $siteStart
      done

      # Disable :ensure and :pin in use-package
      cat ${./disable-use-pin.el} >> $siteStart

      # Byte-compiling improves start-up time only slightly, but costs nothing.
      $emacs/bin/emacs --batch -f batch-byte-compile "$siteStart"
    '';

}
