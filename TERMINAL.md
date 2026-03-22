# Inkstain
```shell
 make dev
 make prod
```

# HumbleUI
```shell
 clj -M:dev
```

# Build JWM
## all
```shell
  rm -rf macos/build && JAVA_HOME=/usr/local/Cellar/openjdk/25.0.2/libexec/openjdk.jdk/Contents/Home arch -arm64 python3 script/build.py
```
## java only
```shell
 JAVA_HOME=/usr/local/Cellar/openjdk/25.0.2/libexec/openjdk.jdk/Contents/Home arch -arm64 python3 script/build.py --only java
```
## native only
```shell
  rm -rf macos/build && JAVA_HOME=/usr/local/Cellar/openjdk/25.0.2/libexec/openjdk.jdk/Contents/Home arch -x86_64 python3 script/build.py --only native
  rm -rf macos/build && JAVA_HOME=/usr/local/Cellar/openjdk/25.0.2/libexec/openjdk.jdk/Contents/Home arch -arm64 python3 script/build.py --native
```
