class Emacs < Formula
  desc "GNU Emacs Native Compiled"
  homepage "https://akrl.sdf.org/gccemacs.html"
  head "git://git.sv.gnu.org/emacs.git", :branch => "feature/native-comp"

  depends_on "autoconf" => :build
  depends_on "pkg-config" => :build
  depends_on "texinfo" => :build
  depends_on "gcc"
  depends_on "gnupg"
  depends_on "gnutls"
  depends_on "jansson"
  depends_on "librsvg"

  uses_from_macos "libxml2"

  def install
    gcc_version = if Formula["gcc"].head?
        "HEAD"
      else
        Formula["gcc"].version.to_s.slice(/\d+/)
      end

    # cores = `sysctl -n hw.ncpu`.chomp

    # - Transform ctags name to ctags.emacs so it doesn't conflict with other
    #   ctags installations that may exist.
    # - Build without ImageMagick because the macOS build uses the native Cocoa
    #   image library instead.
    args = %W[
      --program-transform-name='s/^ctags$/ctags.emacs/'
      --enable-locallisppath=#{HOMEBREW_PREFIX}/share/emacs/site-lisp
      --infodir=#{info}/emacs
      --prefix=#{prefix}
      --with-nativecomp
      --without-imagemagick
      --without-dbus
      --without-pop
    ]

    ENV["CFLAGS"] = %w[
      -Ofast
      -march=native
      -pipe
      -falign-functions=64
      -fomit-frame-pointer
      -funit-at-a-time
      -fforce-addr
      -mfpmath=sse
      -fno-finite-math-only
      -fstack-check
    ].join(" ")
    ENV["LDFLAGS"] = "-Wl"

    ENV["BYTE_COMPILE_EXTRA_FLAGS"] = "--eval '(setq comp-speed 2)'"
    ENV.prepend_path "LIBRARY_PATH", File.join(
      Formula["gcc"].opt_prefix, "/lib/gcc/", gcc_version, "/lib"
    )

    if build.head?
      ENV.prepend_path "PATH", Formula["gnu-sed"].opt_libexec / "gnubin"
      system "./autogen.sh"
    end

    system "./configure", *args
    system "make", "install"
    # mv "lisp", "nextstep/Emacs.app/Contents"
    # mv "nextstep/Emacs.app", prefix
    # print "To install the GUI app, run:"
    # print "mv #{prefix}/Emacs.app ~/Applications"
  end

  test do
    assert_equal "4", shell_output("#{bin}/emacs --batch --eval=\"(print (+ 2 2))\"").strip
  end
end
