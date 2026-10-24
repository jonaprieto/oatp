class Oatp < Formula
  desc "Orchestrate automated theorem provers from Lean 4 and the CLI"
  homepage "https://github.com/jonaprieto/oatp"
  license "Apache-2.0"

  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/jonaprieto/oatp/releases/download/v0.7.12/oatp-0.7.12-macos-arm64.tar.gz"
      sha256 "d2a35506ca51469b7be89b8fb90afe9da6d701f677227ea1bd7f36bb5b7de6be"
    else
      url "https://github.com/jonaprieto/oatp/releases/download/v0.7.12/oatp-0.7.12-macos-x86_64.tar.gz"
      sha256 "503de75d5a5bc5813d61b5e358bd8b324e3937009c9270470226eb2e5b3e794e"
    end
  end

  on_linux do
    if Hardware::CPU.intel?
      url "https://github.com/jonaprieto/oatp/releases/download/v0.7.12/oatp-0.7.12-linux-x86_64.tar.gz"
      sha256 "753dbcaf07b200030248bd616998a09406fbd5f2031925f1579f2622da83e3ec"
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
