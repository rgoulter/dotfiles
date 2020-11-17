self: super:

let
  terraform = super.callPackage ./pkgs/terraform.nix { };
in {
  terraform_0_13 = terraform.terraform_0_13;

  terraform_0_13-full = terraform.terraform_0_13-full;
}

