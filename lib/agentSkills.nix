{
  lib,
  skillsDir,
}: let
  entries = builtins.readDir skillsDir;
  names =
    lib.attrNames (lib.filterAttrs (_name: type: type == "directory") entries);
in
lib.listToAttrs (
  map (name: {
    name = ".agents/skills/${name}";
    value = {
      source = builtins.path {
        path = skillsDir + "/${name}";
        name = "agent-skill-${name}";
      };
    };
  }) names
)
