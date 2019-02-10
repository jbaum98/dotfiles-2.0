self: super:
{
  emacsPackagesNg = super.emacsPackagesNg.overrideScope'(self: super: {
    merlin = super.merlin.overrideAttrs (oldAttrs: {
      meta = oldAttrs.meta // { broken = false; }; 
    });

    flycheck-ocaml = (super.flycheck-ocaml
      .override { inherit (self) merlin; })
      .overrideAttrs (oldAttrs: {
        meta = oldAttrs.meta // { broken = false; }; 
      });
  });
}
