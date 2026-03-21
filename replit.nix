{ pkgs }: {
	deps = [
   pkgs.openssl
   pkgs.curl
   pkgs.rPackages.shiny
		pkgs.R
	];
}