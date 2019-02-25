self: super:
{
  emacsPackagesNg = super.emacsPackagesNg.overrideScope'(selfE: superE: {
    emacs = superE.emacs.override {
      inherit (super) imagemagick;
    };
  });
}
