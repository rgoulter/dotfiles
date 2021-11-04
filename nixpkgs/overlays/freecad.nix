self: super:

with super;
{
  freecad = freecad.override {
    opencascade-occt =
      opencascade-occt.overrideAttrs(oldAttrs: { 
        nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ rapidjson ];
        cmakeFlags = ''-D USE_RAPIDJSON:bool="ON"'';
      });
  };
}
