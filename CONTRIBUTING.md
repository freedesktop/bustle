Want to get involved? Great!
============================

Make sure you have an up-to-date Haskell toolchain. I recommend using
[Stack](https://haskellstack.org/) for development. Make sure you run
`stack update` if you install it from a distro package before continuing.

Grab the latest code from git:

    git clone https://gitlab.freedesktop.org/bustle/bustle.git
    cd bustle

Build it:

    stack build

Run it:

    stack exec bustle

Test it:

    stack test

Please file bugs and merge requests at
<https://gitlab.freedesktop.org/bustle/bustle>.

In new code, try to follow
<https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md>.
The author did not follow it in the past but it seems like a good kind of
thing to aim for.

Releasing Bustle
================

* Ideally, automate the steps below
* Write news in `NEWS.md` and `data/org.freedesktop.Bustle.appdata.xml.in`
* Update version number in `bustle.cabal`

```sh
# Tag release, build and sign the tarballs
make maintainer-make-release

# Stick source and binaries on freedesktop.org
mkdir x.y.z
cp dist/bustle-x.y.z* x.y.z/
scp -r x.y.z annarchy.freedesktop.org:/srv/www.freedesktop.org/www/software/bustle/

# Upload source to Hackage
stack upload .

git push origin --tags master
```

* Add `.1` to version number in `bustle.cabal`
* Update <https://github.com/flathub/org.freedesktop.Bustle>
