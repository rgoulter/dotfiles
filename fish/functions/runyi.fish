# Requires Cabal >= 1.20

function runyi
    set YI_DIR $HOME/github/yi
    env CABAL_SANDBOX_CONFIG=$YI_DIR/cabal.sandbox.config cabal exec $YI_DIR/dist/build/yi/yi -- $argv
end
