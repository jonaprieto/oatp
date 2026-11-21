class Oatp < Formula
  desc "Orchestrate automated theorem provers from Lean 4 and the CLI"
  homepage "https://github.com/jonaprieto/oatp"
  license "Apache-2.0"

  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/jonaprieto/oatp/releases/download/v0.7.14/oatp-0.7.14-macos-arm64.tar.gz"
      sha256 "e5f8f1c2d9aec215e25f4f6a2937ae165add389bd1207ed97a83f06517a5d267"
    else
      url "https://github.com/jonaprieto/oatp/releases/download/v0.7.14/oatp-0.7.14-macos-x86_64.tar.gz"
      sha256 "2b5ed041c394babdcb776bedd920e2de8a6aa5b1b92dae5f29cd531e1d0f7599"
    end
  end

  on_linux do
    if Hardware::CPU.intel?
      url "https://github.com/jonaprieto/oatp/releases/download/v0.7.14/oatp-0.7.14-linux-x86_64.tar.gz"
      sha256 "78bc3d7ad40b0cb8170d376d2d5a2715ed4efd2a63f410ed2c9458f2477e81bd"
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
