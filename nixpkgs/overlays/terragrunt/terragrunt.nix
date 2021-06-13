{ stdenv, lib, buildGoModule, fetchFromGitHub, fetchhg, fetchbzr, fetchsvn, makeWrapper, terraform }:

buildGoModule rec {
  pname = "terragrunt";
  version = "0.24.3";

   src = fetchFromGitHub {
    owner = "gruntwork-io";
    repo = pname;
    rev = "v${version}";
    sha256 = "1yqqg79va14jpvary9qzgrc0f26nx6q8xaq9w4xqan4h3w5x75x5";
  };

  vendorSha256 = "0f466qn5vp74mwx9s4rcbw1x793w8hr5dcf2c12sgshya1bxs4nl";

  doCheck = false;

  buildInputs = [ makeWrapper ];

  preBuild = ''
    buildFlagsArray+=("-ldflags" "-X main.VERSION=v${version}")
  '';

  postInstall = ''
    wrapProgram $out/bin/terragrunt \
      --set TERRAGRUNT_TFPATH ${lib.getBin terraform}/bin/terraform
  '';

  meta = with lib; {
    description = "A thin wrapper for Terraform that supports locking for Terraform state and enforces best practices.";
    homepage = "https://github.com/gruntwork-io/terragrunt/";
    license = licenses.mit;
    maintainers = [ ];
  };
}
