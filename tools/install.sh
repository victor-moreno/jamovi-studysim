#!/usr/bin/env bash
# Build and install the module into jamovi desktop and/or a running jamovi
# Docker container, then run tools/smoke.R (if present) against the install.
#
#   bash tools/install.sh              both targets, whichever are available
#   bash tools/install.sh desktop
#   bash tools/install.sh docker [container]     (default container: jamovi)
#
# The desktop target uses whichever R `Rscript` resolves to (respecting
# ~/.Rprofile, which may append jamovi.app's bundled module library). With rig,
# pick the active R with `rig default <version>` first: it must match the R that
# jamovi.app bundles, or rlang (behind jmvcore) fails to load.
set -euo pipefail
source "$(dirname "$0")/_module.sh"

TARGET="${1:-both}"
CONTAINER="${2:-jamovi}"
ARTIFACT="$MODULE_DIR/${MODULE}_${VERSION}.jmo"
SMOKE="$ROOT/tools/smoke.R"

# R preamble for the smoke test: jamovi's base library, the installed module and
# jmv's library first, as jamovi's engine sets them up (jmc does not bundle
# packages jmv already ships, e.g. car, so a module importing one needs jmv's).
smoke_preamble() {
  printf '.libPaths(c("%s", "%s", "%s", .libPaths()))\n' "$1" "$2" "$(dirname "$(dirname "$1")")/jmv/R"
  printf 'suppressPackageStartupMessages(library("%s", character.only = TRUE))\n' "$MODULE"
}

# ── desktop ──────────────────────────────────────────────────────────────────
install_desktop() {
  local APP LOG MODDIR OLD i
  APP="${JAMOVI_CURRENT_APP:-/Applications/jamovi.app}"
  [ -d "$APP" ] || { echo "!! desktop: $APP is not installed — skipping"; return 0; }

  MODDIR="$HOME/Library/Application Support/jamovi/modules/$MODULE"
  # the Built: stamp of any copy already installed; the new one must differ
  OLD="$(grep '^Built:' "$MODDIR/R/$MODULE/DESCRIPTION" 2>/dev/null || true)"

  echo ">> desktop: building $MODULE $VERSION with $(Rscript -e 'cat(R.version.string)')"
  LOG="$(mktemp "$ROOT/.tmp/install.XXXXXX")"
  ( cd "$MODULE_DIR" && Rscript -e 'jmvtools::install()' ) 2>&1 | tee "$LOG" | grep -vE '^\s*$' || true

  # jmvtools::install() reports errors on stdout and still exits 0, and it can
  # claim success after failing to drive jamovi.app. Judge by the artifacts.
  [ -f "$ARTIFACT" ] || { echo "error: jmvtools did not produce $ARTIFACT" >&2; rm -f "$LOG"; return 1; }
  if grep -qE 'SingletonLock|sandbox initialization failed|GPU process isn.t usable' "$LOG"; then
    echo
    echo "!! jamovi.app could not be driven (already open, or a sandboxed session)."
    echo "!! The .jmo was built. Install it by hand:"
    echo "!!   jamovi -> Modules -> jamovi library -> Sideload -> $ARTIFACT"
    rm -f "$LOG"; return 0
  fi
  if ! grep -q 'Module installed successfully' "$LOG"; then
    echo "error: jmvtools::install() did not install the module (see above)" >&2
    rm -f "$LOG"; return 1
  fi
  rm -f "$LOG"

  # jamovi unpacks the module shortly AFTER jmvtools reports success. Waiting for the
  # directory is not enough when an older copy is installed: the smoke test would load
  # the old copy (measured: a new analysis "is not an exported object"). Wait for a new
  # Built: stamp instead.
  local NEW=""
  for i in $(seq 1 60); do
    NEW="$(grep '^Built:' "$MODDIR/R/$MODULE/DESCRIPTION" 2>/dev/null || true)"
    [ -n "$NEW" ] && [ "$NEW" != "$OLD" ] && break
    sleep 1
  done
  if [ -z "$NEW" ] || [ "$NEW" = "$OLD" ]; then
    echo "!! desktop: jamovi has not replaced the installed copy of $MODULE after 60 s;"
    echo "!! restart jamovi or sideload $ARTIFACT by hand"
    return 1
  fi
  echo ">> desktop: installed at $MODDIR (${NEW#Built: })"

  if [ -f "$SMOKE" ]; then
    echo ">> desktop: smoke test"
    { smoke_preamble "$APP/Contents/Resources/modules/base/R" "$MODDIR/R"; cat "$SMOKE"; } \
      | Rscript --vanilla -
  fi
}

# ── docker ───────────────────────────────────────────────────────────────────
# Copy route, no jamovi-src needed: the module's .jmo (built here against
# jamovi.app by tools/build-jmo.sh, or given as JMO=/path/to.jmo) is unpacked
# into $HOME/.jamovi/modules/<module> in the container, which is then restarted
# so jamovi reads it. The .jmo carries R code built on macOS: fine for a pure-R
# module whose dependencies the image already has; a bundled package with
# compiled code (libs/) is a macOS binary and will not load in the Linux image
# (the script warns). To keep the module across 'down'/'up', mount a volume at
# /root/.jamovi (the skill's templates/docker/docker-compose.override.yaml).
install_docker() {
  # A sandbox that cannot read ~/.docker/config.json makes the CLI print a
  # warning and list nothing, which looks like "no container". Use a writable config.
  if [ -z "${DOCKER_CONFIG:-}" ] && ! [ -r "$HOME/.docker/config.json" ]; then
    export DOCKER_CONFIG="$ROOT/.tmp/dockercfg"; mkdir -p "$DOCKER_CONFIG"
  fi
  command -v docker >/dev/null || { echo "!! docker: no docker CLI — skipping"; return 0; }
  if ! docker ps --format '{{.Names}}' 2>/dev/null | grep -qx "$CONTAINER"; then
    echo "!! docker: container '$CONTAINER' is not running — skipping"
    return 0
  fi
  local CHOME ARCH JMOFILE UNPACK PORT i b
  CHOME="$(docker exec "$CONTAINER" sh -c 'echo $HOME')"
  case "$(docker exec "$CONTAINER" uname -m)" in
    aarch64|arm64) ARCH=arm64 ;;
    *) ARCH=x64 ;;
  esac

  JMOFILE="${JMO:-}"
  if [ -z "$JMOFILE" ]; then
    echo ">> docker: building $MODULE $VERSION (tools/build-jmo.sh current)"
    bash "$ROOT/tools/build-jmo.sh" current || return 1
    JMOFILE="$(ls -t "$ROOT"/dist/"${MODULE}_${VERSION}"_current_*_"$ARCH".jmo 2>/dev/null | head -1)"
  fi
  [ -f "$JMOFILE" ] || { echo "error: no .jmo for $ARCH (set JMO=/path/to.jmo)" >&2; return 1; }

  UNPACK="$ROOT/.tmp/jmo-docker"
  rm -rf "$UNPACK"; mkdir -p "$UNPACK"
  unzip -q "$JMOFILE" -d "$UNPACK" || return 1
  [ -d "$UNPACK/$MODULE" ] || { echo "error: $JMOFILE has no $MODULE/ folder" >&2; return 1; }
  for b in "$UNPACK/$MODULE"/R/*/; do
    [ -d "$b/libs" ] && echo "!! docker: bundled package $(basename "$b") has compiled code built on macOS; it will not load in the container"
  done

  echo ">> docker: copying $(basename "$JMOFILE") into $CONTAINER:$CHOME/.jamovi/modules/$MODULE"
  docker exec "$CONTAINER" sh -c "mkdir -p '$CHOME/.jamovi/modules' && rm -rf '$CHOME/.jamovi/modules/$MODULE'" || return 1
  docker cp "$UNPACK/$MODULE" "$CONTAINER:$CHOME/.jamovi/modules/$MODULE" || return 1
  rm -rf "$UNPACK"

  # jamovi reads the modules folder when it starts
  echo ">> docker: restarting $CONTAINER"
  docker restart "$CONTAINER" >/dev/null || return 1
  PORT="$(docker port "$CONTAINER" 2>/dev/null | sed -n 's/.*:\([0-9]*\)$/\1/p' | sort -n | head -1)"
  for i in $(seq 1 60); do
    if [ -z "$PORT" ] || curl -s -o /dev/null "http://127.0.0.1:$PORT"; then break; fi
    sleep 1
  done
  if ! docker inspect -f '{{range .Mounts}}{{println .Destination}}{{end}}' "$CONTAINER" | grep -qE "^$CHOME/.jamovi(/modules)?$"; then
    echo "!! docker: $CHOME/.jamovi is not a volume: the module is lost when the container is"
    echo "!! recreated (down/up). See the skill's templates/docker/docker-compose.override.yaml"
  fi

  if [ -f "$SMOKE" ]; then
    echo ">> docker: smoke test"
    { smoke_preamble /usr/lib/jamovi/modules/base/R "$CHOME/.jamovi/modules/$MODULE/R"; cat "$SMOKE"; } \
      | docker exec -i "$CONTAINER" bash -c 'source /usr/lib/jamovi/bin/env.conf 2>/dev/null || true; Rscript --vanilla -'
  fi
  echo ">> docker: installed $MODULE $VERSION"
}

case "$TARGET" in
  desktop) install_desktop ;;
  docker)  install_docker ;;
  both)    install_desktop || true; echo; install_docker || true ;;
  *)       echo "usage: install.sh [desktop|docker|both] [container]" >&2; exit 1 ;;
esac
