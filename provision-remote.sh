#!/bin/bash
# provision-remote.sh <hostname>
#
# This script provisions a remote host with the necessary environment
# for Common Lisp development (Sly/Slynk) and LSP (bash-ls, python-ls).
# It does NOT require sudo.

set -e

HOST=$1

if [ -z "$HOST" ]; then
    echo "Usage: $0 <hostname>"
    exit 1
fi

echo "--- Provisioning $HOST ---"

# 1. Prepare Remote Paths and Tools
ssh "$HOST" 'bash -s' <<'EOF'
    set -e
    mkdir -p "$HOME/.local/bin" "$HOME/bin"
    
    # Path setup in common RC files
    for rc in "$HOME/.bashrc" "$HOME/.zshrc" "$HOME/.profile"; do
        if [ ! -f "$rc" ]; then touch "$rc"; fi
        if ! grep -q ".local/bin" "$rc"; then
            echo 'export PATH="$HOME/.local/bin:$HOME/bin:$PATH"' >> "$rc"
            echo "Added ~/.local/bin to $rc"
        fi
    done
    export PATH="$HOME/.local/bin:$HOME/bin:$PATH"

    # Install NVM and Node.js (for bash-language-server)
    if ! command -v node &> /dev/null; then
        echo "Installing NVM and Node.js..."
        curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.7/install.sh | bash
        export NVM_DIR="$HOME/.nvm"
        [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
        nvm install --lts
    fi

    # Ensure nvm is loaded for the rest of this block
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

    # Install Bash Language Server
    echo "Installing bash-language-server..."
    npm install -g bash-language-server
    
    # Symlink node, npm, and bash-language-server to ~/.local/bin for easier discovery
    NODE_PATH=$(command -v node)
    if [ -n "$NODE_PATH" ]; then
        ln -sf "$NODE_PATH" "$HOME/.local/bin/node"
    fi
    NPM_PATH=$(command -v npm)
    if [ -n "$NPM_PATH" ]; then
        ln -sf "$NPM_PATH" "$HOME/.local/bin/npm"
    fi
    BASH_LS_PATH=$(command -v bash-language-server)
    if [ -n "$BASH_LS_PATH" ]; then
        ln -sf "$BASH_LS_PATH" "$HOME/.local/bin/bash-language-server"
        ln -sf "$BASH_LS_PATH" "$HOME/.local/bin/bash-ls"
    fi

    # Install Python Language Server
    echo "Installing python-lsp-server..."
    if command -v pip3 &> /dev/null; then
        pip3 install --user "python-lsp-server" || echo "Warning: pip3 install python-lsp-server failed"
    elif command -v pip &> /dev/null; then
        pip install --user "python-lsp-server" || echo "Warning: pip install python-lsp-server failed"
    fi

    # Create directory structure for Sly
    mkdir -p "$HOME/SourceCode/lisp/sly-contribs"
    mkdir -p "$HOME/.roswell/local-projects"
EOF

# 2. Sync Sly/Slynk extensions from the local machine
# We assume the local machine has these repos in ~/.emacs.d/straight/repos/
echo "--- Syncing Sly extensions ---"
SLY_EXTS=(sly-asdf sly-named-readtables sly-macrostep sly-quicklisp)
for ext in "${SLY_EXTS[@]}"; do
    LOCAL_PATH="$HOME/.emacs.d/straight/repos/$ext/"
    if [ -d "$LOCAL_PATH" ]; then
        echo "Syncing $ext..."
        rsync -az --exclude '.git' "$LOCAL_PATH" "$HOST:~/SourceCode/lisp/sly-contribs/$ext/"
        ssh "$HOST" "ln -sf ~/SourceCode/lisp/sly-contribs/$ext ~/.roswell/local-projects/"
    else
        echo "Warning: Local Sly extension $ext not found at $LOCAL_PATH"
    fi
done

# Sync sly-stepper (special case)
STEPPER_LOCAL="$HOME/SourceCode/lisp/emacs_stuff/sly-stepper/"
if [ -d "$STEPPER_LOCAL" ]; then
    echo "Syncing sly-stepper..."
    rsync -az --exclude '.git' "$STEPPER_LOCAL" "$HOST:~/SourceCode/lisp/sly-contribs/sly-stepper/"
    ssh "$HOST" "ln -sf ~/SourceCode/lisp/sly-contribs/sly-stepper ~/.roswell/local-projects/"
fi

# 3. Create/Update start-slynk.sh
echo "--- Updating start-slynk.sh ---"
ssh "$HOST" 'cat > ~/start-slynk.sh <<EOF_INNER
#!/bin/bash
# start-slynk.sh - Durable Slynk server using tmux
PORT=\${1:-4005}
SESSION="slynk"

# Ensure ~/.local/bin is in PATH for ros/sbcl
export PATH="\$HOME/.local/bin:\$HOME/bin:\$PATH"

if ! command -v tmux &> /dev/null; then
    echo "Error: tmux is not installed on remote host."
    exit 1
fi

tmux kill-session -t "\$SESSION" 2>/dev/null
# Start SBCL/Roswell and ensure slynk is loaded before creating server
tmux new-session -d -s "\$SESSION" "sbcl --eval \"(unless (find-package :slynk) (ql:quickload :slynk))\" --eval \"(slynk:create-server :port \$PORT :dont-close t)\""

if [ \$? -eq 0 ]; then
    echo "Slynk started in tmux session \"\$SESSION\" on port \$PORT"
else
    echo "Failed to start Slynk."
    exit 1
fi
EOF_INNER
chmod +x ~/start-slynk.sh'

echo "--- Provisioning of $HOST complete ---"
