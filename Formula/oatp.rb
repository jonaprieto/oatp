class Oatp < Formula
  desc "Orchestrate automated theorem provers from Lean 4 and the CLI"
  homepage "https://github.com/jonaprieto/oatp"
  license "Apache-2.0"

  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/jonaprieto/oatp/releases/download/v0.7.13/oatp-0.7.13-macos-arm64.tar.gz"
      sha256 "a0b53d0f1c803a460634cbafd3887bf42eb80921dc71e06b9eceac267f313535"
    else
      url "https://github.com/jonaprieto/oatp/releases/download/v0.7.13/oatp-0.7.13-macos-x86_64.tar.gz"
      sha256 "b02292d90fd2eeb3b7e455ce57f4c33f1cfbf91df3f2e71268e81f506c05fd05"
    end
  end

  on_linux do
    if Hardware::CPU.intel?
      url "https://github.com/jonaprieto/oatp/releases/download/v0.7.13/oatp-0.7.13-linux-x86_64.tar.gz"
      sha256 "e7016c3c73e04dd3bf520610e22fb0de1abb9389e0bc5bf223b99c09d0be5f7f"
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
