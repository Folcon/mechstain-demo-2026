# Mechstain

This page is for a **very alpha** prototype which is still **actively** in development.

Mechstain is something I'm fiddling around with to play with the ideas of an evolving world where:

- There is minimal direct combat
- There's space for non-standard races
- Lots of things are generated, including as much as possible:
  - The tech tree
  - What's available for people in these civilisations to buy or trade

The art is going to be very basic to start, but that may change as things continue.

## Usage

FIXME

## How do I get `JWM`?

1) Go to [JWM's Build Artifacts](https://github.com/HumbleUI/JWM/actions/workflows/build-deploy.yml),
2) download the jars artifact from the latest working build, unzip it
3) use `mvn install:install-file -Dfile=<downloaded location>/jars/jwm-b3fecb126a.jar -DpomFile=<downloaded location>/jars/maven/META-INF/maven/io.github.humbleui/jwm/pom.xml`

From `https://github.com/HumbleUI/HumbleUI/issues/13#issuecomment-1017687992`

# Minecraft
- Look at generating a server running [witchcraft](https://github.com/lambdaisland/witchcraft) and use that to visualise the generated world.

# Better debugging
- https://andersmurphy.com/2019/06/04/clojure-a-debug-macro-for-threading-macros-using-tap.html
- https://www.birkey.co/2018-10-26-datafy-and-tap%3E-in-clojure-1.10.html
- https://github.com/weavejester/hashp/issues/2
- https://github.com/dgrnbrg/spyscope

# Performance
- https://tech.redplanetlabs.com/2020/09/02/clojure-faster/
- Matrices
  - [Expressing Boolean Logic with Matrices](https://ozaner.github.io/boolean-logic-with-matrices/)
  - [Logical Matrix](https://en.wikipedia.org/wiki/Logical_matrix)
- High Performance Datalog
  - [Souffle](https://souffle-lang.github.io/examples)

## Building Binaries
TODO: Look at:
- https://github.com/Raynes/lein-bin
- https://www.reddit.com/r/Clojure/comments/45y3l0/how_do_you_create_a_standalone_executable_with/
- Two things here:
  - https://libgdx.com/wiki/deployment/deploying-your-application
  - https://libgdx.com/wiki/deployment/bundling-a-jre
    - https://github.com/libgdx/packr
    - https://github.com/libgdx/packr/issues/33#issuecomment-70629532
- https://www.ej-technologies.com/products/install4j/overview.html

# Release
## To github
- Create a tag:
  ```shell
  git tag v0.0.1 -a
  ```
- Push the tag:
  ```shell
  git push origin tag v0.0.1
  ```
- Delete a local tag
  ```shell
  git tag -d v0.0.1
  ```
- Delete a remote tag
  ```shell
  git push --delete origin v0.0.1
  ```
- Oneliner for debugging
  ```shell
  git tag -d v0.0.1 && git tag v0.0.1 -a -m "Trying to get build workflow working" && git push -f --follow-tags
  TAG=v0.0.1 MSG="Trying to get build workflow working" && git tag -d $TAG && git tag $TAG -a -m $MSG && git push -f --follow-tags
  ```

## OS Specific Stuff
### Cross Platform Scripts
- I managed to find this ***after*** I finished working out my build stuff, which *sucks*, but I'll revisit it later when I have some nicer art etc and want to make it look better...
  - https://github.com/rokstrnisa/unattach/blob/master/package.sh#L47

### macos
#### Code Signing
- [How to automatically sign MacOS apps using Github Actions](https://localazy.com/blog/how-to-automatically-sign-macos-apps-using-github-actions)
- [MacOS Java Notarization](https://www.joelotter.com/2020/08/14/macos-java-notarization.html)
- [SO: Code signing + notarization using jpackage utility isn't working on macOS](https://stackoverflow.com/a/61091234)
- [Notarize Disk Image Developer ID Distribution](https://indiespark.top/programming/notarize-disk-image-developer-id-distribution/)
- Improvements
  - Better packages coming out the other side, ie with icons

# Minimising Jar Size
- https://geokon-gh.github.io/clojure-packaging.html

# Debugging Release
- [Run GitHub Actions locally](https://github.com/nektos/act)

# Store Links
- [itch.io](https://folcon.itch.io/mechstain-7drl-2026), it may be 👻(hidden 😉).

## License

Copyright © 2026 Folcon

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
