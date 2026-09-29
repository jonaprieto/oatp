class Oatp < Formula
  desc "Orchestrate automated theorem provers from Lean 4 and the CLI"
  homepage "https://github.com/jonaprieto/oatp"
  license "Apache-2.0"

  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/jonaprieto/oatp/releases/download/v0.7.11/oatp-0.7.11-macos-arm64.tar.gz"
      sha256 "50c195457ade0e94a23d056d0c36c6ba2708e5c3a5ab983649a7ab501b413017"
    else
      url "https://github.com/jonaprieto/oatp/releases/download/v0.7.11/oatp-0.7.11-macos-x86_64.tar.gz"
      sha256 "bceb134d9cd9b154fe9cc42f16455807f3a8e8ae8b5788b27a11158ab43453c8"
    end
  end

  on_linux do
    if Hardware::CPU.intel?
      url "https://github.com/jonaprieto/oatp/releases/download/v0.7.11/oatp-0.7.11-linux-x86_64.tar.gz"
      sha256 "eca67e908a30d43e917c66301212616adfa7fc2b3117564171e391e6a0271df7"
    else
      odie "oatp currently publishes Linux x86_64 binaries only"
    end
  end

  def install
    bin.install "oatp"
  end

  test do
    assert_match "oatp version #{version}", shell_output("#{bin}/oatp --version")
  end
end
