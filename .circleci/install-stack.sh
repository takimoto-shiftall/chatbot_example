#!/bin/bash

if [ -f ~/.local/bin/stack ]
then
    echo "Stack was already installed."
else
    echo "Start Download and install stack."
    mkdir -p ~/.local/bin
    curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --strip-components=1 -C ~/.local/bin '*/stack'
fi

export PATH=$HOME/.local/bin:$PATH